{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- | The '.ao' file is a format for developing an AO dictionary in
-- the conventional filesystem + text editor. It is also a suitable
-- format for import and export of AO dictionaries.
--
-- The format is extremely simple:
--
--     import1 import2 import3
--     @word1 ao code here
--     @word2 more ao code
--     @word3
--     "word definitions may require
--      multiple lines, though it is
--      common to favor one-line defs
--     ~
--     @word4 etc.
--
-- The '@' at each new line is an entry separator. It isolates any
-- parse errors. Imports correspond to other '.ao' files that can
-- be found in the AO_PATH. The '.ao' suffix is implicit. The full
-- dictionary is specified by providing a root file (oft by text).
-- Usually, this initial file is just an initial imports section,
-- e.g. the `aoi` executable loads "aoi".
--
-- Cyclic and redundant imports are not an issue. A file is loaded
-- at most once. Identical definitions do not conflict. The order
-- in which files or definitions are loaded is weakly significant:
-- if definitions conflict, the last one wins. But a warning will
-- still be emitted. There is no requirement to import a definition
-- before using a word; only the final dictionary is relevant.
--
-- For now, the presence of words of form `compile!foo` will direct
-- some words to be implemented as separate ABC resources. These
-- can be accessed via `loadRscFile`.  
--
-- Long term, the intention is to eschew the filesystem and use a
-- database approach with caching and incremental update. However,
-- the '.ao' format may still be useful for import/export.
--
module AO.AOFile
    ( loadAOFiles, AOFile(..)
    , aoFilesToDefs, AOFMeta(..)
    , loadAODict0
    
    -- integrating AO precompilation and ABC resource models
    , loadAODict
    , loadRscFile, saveRscFile
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Either
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import Text.Parsec.Text() 
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified System.Environment as Env
import qualified System.IO.Error as Err
import qualified Data.ByteString.Base64.URL as B64

import AO.Parser
import AO.Dict
import AO.Code
import AO.Precompile
import ABC.Resource

type Import   = Text
type Line     = Int
type LoadAO m = StateT (LoaderState m) m
data LoaderState m = LoaderState
    { ld_path :: [FS.FilePath]      -- 
    , ld_warn :: (String -> m ())   -- 
    , ld_todo :: ![Import]           -- upcoming work (FIFO)
    , ld_completed :: ![AOFile]
    }

-- | contents of a specific file
data AOFile = AOFile
    { aof_imp :: Import        -- this import 
    , aof_path :: FS.FilePath  -- file source
    , aof_imps :: [Import]     -- imports section
    , aof_defs :: [AODef Line] -- definitions with line numbers
    }

-- | metadata after extracting defs from the files
data AOFMeta = AOFMeta 
    { aofm_import :: !Import
    , aofm_path   :: !FS.FilePath
    , aofm_line   :: {-# UNPACK #-} !Line
    }

-- show will not print file contents, just the location (for debugging)
instance Show AOFile where showsPrec _ = shows . aof_path

-- | Load AO files based on an initial file text. Usually, this
-- root is just a singleton imports section like "aoi" or "std",
-- so (for convenience) string based arguments are supported.
class AOFileRoot src where 
    -- | loadAOFiles takes two arguments:
    --    a root source (e.g. string or filepath)
    --    an operation to output warning messages (String -> m ()).
    loadAOFiles 
        :: (MonadIO m) 
        => src -> (String -> m ()) 
        -> m [AOFile]

instance AOFileRoot String where loadAOFiles = loadAOFiles . T.pack
instance AOFileRoot Text where 
    loadAOFiles = loadAO . importFileT root rootF where
        root = T.pack "root text"
        rootF = FS.fromText root
instance AOFileRoot FS.FilePath where 
    loadAOFiles = loadAO . importFileP root where
        root = T.pack "root file"

loadAO :: (MonadIO m) => LoadAO m () -> (String -> m ()) -> m [AOFile]
loadAO ldRoot warn = ld_completed `liftM` execStateT ldDeep st0 where
    st0 = LoaderState [] warn [] []
    ldDeep = initAO_PATH >> ldRoot >> runUntilDone

-- Process AO_PATH once per toplevel load. Emit warnings for any issues
-- with the AO_PATH environment variable. Reduces path to canonical
-- directories.
initAO_PATH :: (MonadIO m) => LoadAO m ()
initAO_PATH = getAO_PATH >>= procPaths where
    getAO_PATH = liftIO $ Err.tryIOError (Env.getEnv "AO_PATH")
    procPaths (Left _) = emitWarning eNoPath >> setPath []
    procPaths (Right sps) =
        let paths = FS.splitSearchPathString sps in
        mapM getCanonDir paths >>= \ lErrOrDir ->
        let (errs, dirs) = partitionEithers lErrOrDir in
        mapM_ (emitWarning . show) errs >>
        when (null dirs) (emitWarning eNoDirsInPath) >>
        setPath dirs
    setPath p = modify $ \ ld -> ld { ld_path = p }
    eNoPath = "Environment variable AO_PATH is not defined."
    eNoDirsInPath = "No accessible directories in AO_PATH!"
    getCanonDir = liftIO . Err.tryIOError . canonicalizeDirPath

-- canonizalize + assert isDirectory (may fail with IOError)
canonicalizeDirPath :: FS.FilePath -> IO FS.FilePath 
canonicalizeDirPath fp = 
    FS.canonicalizePath fp >>= \ cfp ->
    FS.isDirectory cfp >>= \ bDir ->
    if bDir then return cfp else -- success case
    let emsg = show cfp ++ " is not a directory." in
    let etype = Err.doesNotExistErrorType in
    Err.ioError $ Err.mkIOError etype emsg Nothing (Just (show fp))

-- Import AO file(s) associated with a given import name.
--
-- If more than one, we'll favor first elements in list
-- for conventional behavior with PATH environment vars.
loadImport :: (MonadIO m) => Import -> LoadAO m ()
loadImport imp = 
    gets ld_path >>= \ dirs ->
    let fn = FS.fromText imp FS.<.> T.pack "ao" in
    let fpaths = map (FS.</> fn) dirs in
    filterM (liftIO . FS.isFile) fpaths >>= \ lFound ->
    specialIfNotUnique imp lFound >>
    mapM_ (importFileP imp) lFound -- import all files found

-- special handling or warnings for non-unique import
-- 
-- I want to discourage ambiguity. The AO dictionary concept isn't
-- about 'overloading' at the dictionary layer. So I emit warnings.
specialIfNotUnique :: (Monad m) => Import -> [FS.FilePath] -> LoadAO m ()
specialIfNotUnique _ (_:[]) = return () -- unique, no special action
specialIfNotUnique imp [] = 
    emitWarning (T.unpack imp ++ " import not found!") >>
    emitFile (AOFile imp FS.empty [] []) -- prevent retry 
specialIfNotUnique imp locations = emitWarning emsg where
    emsg = L.concat (header:body)
    header = T.unpack imp ++ " import ambiguous. Locations: "
    body = fmap (showString "\n  " . show) locations

-- load a specific import file.
importFileP :: (MonadIO m) => Import -> FS.FilePath -> LoadAO m ()
importFileP imp fpath = load >>= proc where
    load = liftIO $ Err.tryIOError $ FS.readTextFile fpath
    proc (Left e) = emitWarning (show e)
    proc (Right code) = importFileT imp fpath code

-- load a specific file after possessing the text.
importFileT :: (Monad m) => Import -> FS.FilePath -> Text -> LoadAO m ()
importFileT thisImp fpath code =
    readAOFileText fpath code >>= \ (imps,defs) ->
    emitFile (AOFile thisImp fpath imps defs) >>
    mapM_ addTodo imps -- add imports to task list

-- extract basic entries from a file text; emit a warning for
-- any entry that fails to parse
readAOFileText :: (Monad m) => FS.FilePath -> Text -> LoadAO m ([Import], [AODef Line])
readAOFileText fp code = case splitEntries $ dropTheBOM code of
    [] -> return ([],[]) -- not possible, but also not a problem
    ((_,impEnt):defEnts) -> 
        let src = T.unpack $ either id id $ FS.toText fp in
        let imps = T.words impEnt in
        let (errs,defs) = partitionEithers $ fmap (parseEntry src) defEnts in
        mapM_ (emitWarning . show) errs >> -- report parse errors
        return (imps, defs)

-- parse an entry, correcting for line and column (for error reporting)
parseEntry :: String -> (Line, Text) -> Either P.ParseError (AODef Line)
parseEntry src (ln, entry) = P.parse parser "" entry where
    parser = 
        P.setPosition (P.newPos src ln 2) >>
        parseWord >>= \ word ->
        parseAO >>= \ code -> 
        return (word, (code, ln))

-- remove initial byte order mark (BOM) if necessary
-- (a BOM is automatically added to unicode text by some editors)
dropTheBOM :: Text -> Text
dropTheBOM t = case T.uncons t of 
    Just ('\xfeff', t') -> t'
    _ -> t -- no BOM

-- entries are separated by "\n@". Exceptional case if the first
-- character is '@', so handle that.
splitEntries :: Text -> [(Line,Text)]
splitEntries t = case T.uncons t of 
    Just ('@', t') -> (0, T.empty) : numberTheLines 1 (T.splitOn eSep t') 
    _ -> numberTheLines 1 (T.splitOn eSep t)

-- Entry separator is the "\n@" sequence. Entries are separated 
-- prior to parsing in order to isolate parse errors. 
eSep :: Text
eSep = T.pack "\n@"

-- recover line numbers for each entry
numberTheLines :: Line -> [Text] -> [(Line,Text)]
numberTheLines _ [] = []
numberTheLines n (e:es) = (n,e) : numberTheLines n' es where
    n' = T.foldl accumLF (1 + n) e
    accumLF ct '\n' = 1 + ct
    accumLF ct _ = ct

-- runUntilDone will handle tasks (imports) on the stack
-- until said stack is empty. The stack based ordering 
-- results in the desired final order where the 'last'
-- def for any word is closest to the end of the list.
runUntilDone :: (MonadIO m) => LoadAO m ()
runUntilDone = getNextTodo >>= doAndRepeat where
    doAndRepeat Nothing = return ()
    doAndRepeat (Just imp) = loadImport imp >> runUntilDone
    
-- emit a warning through a provided capability.
emitWarning :: (Monad m) => String -> LoadAO m ()
emitWarning w = gets ld_warn >>= \ warnOp -> lift (warnOp w)

-- add a file to the output.
emitFile :: (Monad m) => AOFile -> LoadAO m ()
emitFile f = modify $ \ ld ->
    let fs' = (f : ld_completed ld) in
    ld { ld_completed = fs' }

-- add an import to the 'todo' list. This is a LIFO stack.
addTodo :: (Monad m) => Import -> LoadAO m ()
addTodo imp = modify $ \ ld -> 
    let td' = (imp : ld_todo ld) in
    ld { ld_todo = td' }

-- obtain the next 'todo' import, filtering those imports 
-- that have already been processed. This ensures each file
-- is loaded at most once regardless of dups or cycles.
getNextTodo :: (Monad m) => LoadAO m (Maybe Import)
getNextTodo =
    get >>= \ ld ->
    case ld_todo ld of
        [] -> return Nothing
        (x:xs) ->
            let lCompleted = fmap aof_imp (ld_completed ld) in
            let beenDone = L.elem x lCompleted in
            let ld' = ld { ld_todo = xs } in
            put ld' >>
            if beenDone then getNextTodo 
                        else return (Just x)


-- | translate a list of files into a list of definitions.
aoFilesToDefs :: [AOFile] -> [AODef AOFMeta]
aoFilesToDefs = L.concatMap defsInFile where
    addMeta imp fp (w,(code,ln)) = (w,(code,AOFMeta imp fp ln))
    defsInFile aof = 
        let imp = aof_imp aof in
        let path = aof_path aof in
        imp `seq` path `seq`
        fmap (addMeta imp path) (aof_defs aof)

-- | Load an AO dictionary from a root source, then process this
-- into an AODict. All errors are reported through the same 
-- interface. Does not run code through precompilation.
loadAODict0 :: (AOFileRoot s, MonadIO m) 
           => s -> (String -> m ()) -> m (AODict AOFMeta)
loadAODict0 src warnOp = 
    loadAOFiles src warnOp >>=  
    buildAODict (warnOp . showDictIssue) . aoFilesToDefs

-- | Load an AO dictionary and additionally precompile it, saving
-- the ciphertext resources into an AO_TEMP subdirectory. These
-- resources may later be fetched (and additional ones saved) by
-- use of `loadRscFile` and `saveRscFile`.
loadAODict :: (AOFileRoot s, MonadIO m) 
           => s -> (String -> m ()) -> m (AODict AOFMeta)
loadAODict src warnOp =
    loadAODict0 src warnOp >>= \ d0 ->
    let (df,(_prcd,secd)) = preCompileDict d0 in
    emitCT warnOp secd >>
    return df

-- the base64 encoding of the 192-bit hash, with a minor wrapper to
-- integrate with filesystems. Only 160-bits are preserved in a
-- case-insensitive filesystem, but that should still be good enough.
hctFile :: HashCT -> FS.FilePath
hctFile = ext . FS.fromText . pref . T.decodeUtf8 . B64.encode where
    pref = T.cons 'R'
    ext = flip FS.addExtension $ T.pack "hct"

emitCT :: (MonadIO m) => (String -> m ()) -> M.Map HashCT [CipherText] -> m ()
emitCT _ secd | M.null secd = return ()
emitCT warnOp secd = 
    liftIO getRscDir >>= \ rscDir ->
    mapM_ (saveCT warnOp rscDir) (M.toList secd) 

saveCT :: (MonadIO m) => (String -> m ()) -> FS.FilePath -> (HashCT,[CipherText]) -> m ()
saveCT warnOp rsc (hct,[ct]) =
    let fp = rsc FS.</> hctFile hct in
    let trySaveFile = Err.tryIOError (FS.writeFile fp ct) in
    liftIO trySaveFile >>= either (warnOp . show) return
saveCT warnOp _ (hct,_) = warnOp emsg where
    emsg = "secure hash collision (192 bit) @ " ++ FS.encodeString (hctFile hct)

loadRscFile :: (MonadIO m) => HashCT -> m [CipherText]
loadRscFile hct = 
    liftIO getRscDir >>= \ rsc ->
    let fp = rsc FS.</> hctFile hct in
    let tryReadFile = Err.tryIOError (FS.readFile fp) in
    let zeroOrOne = either (const []) (:[]) in
    liftIO (zeroOrOne <$> tryReadFile)

saveRscFile :: (MonadIO m) => HashCT -> CipherText -> m ()
saveRscFile hct ct =
    liftIO getRscDir >>= \ rsc ->
    let fp = rsc FS.</> hctFile hct in
    let trySaveFile = Err.tryIOError (FS.writeFile fp ct) in
    (liftIO trySaveFile) >>= either (fail . show) return

-- (idempotent) obtain (and create) the ABC ciphertext resource 
-- storage directory. This will serve as the primary location for
-- resources until we upgrade to a proper database. 
getRscDir :: IO FS.FilePath
getRscDir =  
    getAO_TEMP >>= \ aoTmp ->
    let rscDir = aoTmp FS.</> FS.fromText (T.pack "rsc") in
    FS.createDirectory True rscDir >>
    return rscDir

-- (idempotent) obtain (and create) the AO_TEMP directory
-- may raise an IOError based on permissions or similar
getAO_TEMP :: IO FS.FilePath
getAO_TEMP = 
    let getVar = Err.tryIOError (Env.getEnv "AO_TEMP") in
    let withDefault = either (const "aotmp") id in
    withDefault <$> getVar >>= \ d0 -> 
    let fp0 = FS.fromText (T.pack d0) in
    FS.createTree fp0 >>
    FS.canonicalizePath fp0

showDictIssue :: AODictIssue AOFMeta -> String
showDictIssue (AODefOverride w defs) = 
    let msgHdr = "word " ++ T.unpack w ++ " has override(s)" in
    let locations = fmap (("\n  " ++) . wordLocStr w . snd) defs in
    L.concat (msgHdr:locations)
showDictIssue (AODefCycle defs) = 
    let wordsInCycle = T.unpack $ T.unwords $ fmap fst defs in
    let msgHdr = "cycle detected involving: " ++ wordsInCycle in
    let defToLoc (w,(_,aofm)) = wordLocStr w aofm in
    let locations = fmap (("\n  " ++) . defToLoc) defs in
    L.concat (msgHdr:locations)
showDictIssue (AODefMissing (w,(_,aofm)) missingWords) = 
    let wl = L.unwords (fmap T.unpack missingWords) in
    "word " ++ T.unpack w ++ " needs definitions for: " ++ wl
      ++ "\n  " ++ wordLocStr w aofm

locStr :: AOFMeta -> String
locStr aofm = pathStr ++ ":" ++ show (aofm_line aofm) where
    pathStr = T.unpack $ either id id $ FS.toText (aofm_path aofm)

wordLocStr :: Word -> AOFMeta -> String
wordLocStr word aofm = T.unpack word ++ "@" ++ locStr aofm

