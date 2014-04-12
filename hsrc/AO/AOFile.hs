
-- | The '.ao' file is a format for developing an AO dictionary in
-- the conventiona filesystem + text editor. It is also a suitable
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
-- dictionary is specified by providing an imports section (which
-- may depend on the project). Cyclic or redundant imports are not 
-- a problem; each file is loaded at most once.
-- 
-- Definitions are processed almost independently. For example, a
-- word needs not be defined or imported before use; it only needs
-- to be part of the final dictionary. Cyclic or missing words are
-- detected at the dictionary level. However, in case of multiple
-- definitions, the 'last' definition is favored (and a warning is
-- issued). 
--
module AO.AOFile
    ( loadAOFiles
    ) where

import Control.Applicative
import Control.Monad

import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Data.Either
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS

import qualified System.Environment as Env
import qualified System.IO.Error as Err

import AO.Code
import AO.Char (isPathSep)
import AO.Parser

type Import = String
type LoadAO = StateT LoaderState
data LoaderState = LoaderState
    { ld_path :: [FS.FilePath] -- 
    , ld_todo :: [Import]      -- upcoming work (FIFO)
    , ld_warn :: [String]      -- warnings (in reverse)
    , ld_done :: [(Import,AOFile)]
    }

-- | Load AO files based on an initial import or list thereof.
--
-- This first argument is parsed as an import section for
-- consistency purposes. So "foo" would import a dictionary
-- starting with "foo.ao", while "foo bar baz" would import
-- all three (favoring definitions from 'baz'). 
--
-- 
--
-- This  files will return a list of warnings and a list of

loadAOFiles :: String -> IO ([String],[AOFile])



-- add a warning to the list of warnings
emitWarning :: (Monad m) => String -> LoadAO m ()
emitWarning w = modify $ \ ld ->
    let ws' = (w:ld_warn ld) in
    ld { ld_warn = ws' }

-- add an import to the 'todo' list. This is a LIFO stack.
addTodo :: (Monad m) => Import -> LoadAO m ()
addTodo imp = modify $ \ ld -> 
    let td' = (imp : ld_todo ld) in
    ld { ld_todo = td' }

-- obtain the next 'todo' item, filtering those that
-- have already been completed.
getNextTodo :: (Monad m) => LoadAO m (Maybe Import)
getNextTodo =
    get >>= \ ld ->
    case ld_todo ld of
        [] -> return Nothing
        (x:xs) ->
            let ld' = ld { ld_todo = xs } in
            put ld' >>
            let beenDone = L.elem x (map fst (ld_done ld)) in
            if beenDone then getNextTodo 
                        else return (Just x)

-- Process AO_PATH once per toplevel load. Emit warnings into list.
initAO_PATH :: (MonadIO m) => LoadAO m ()
initAO_PATH = getAO_PATH >>= procPaths where
    getAO_PATH = liftIO $ Err.tryIOError (Env.getEnv "AO_PATH")
    procPaths (Left _) =
        emitWarning "Environment variable AO_PATH is not defined." >>
        setEmptyPath
    procPaths (Right str) =
        let paths = splitPath str in
        mapM tryCanonicalize paths >>= 
        filterM keepGoodDirectories >>=
        setNonEmptyPath 
    splitPath = map FS.fromText . T.split isPathSep . T.pack 
    tryCanonicalize = liftIO $ Err.tryIOError . FS.canonicalizePath
    keepGoodDirectories (Left error) = emitWarning (show error) >> return False
    keepGoodDirectories (Right path) = (liftIO . FS.isDirectory) path
    setNonEmptyPath [] =
        emitWarning "No directories found in AO_PATH." >>
        setEmptyPath
    setNonEmptyPath dirs = setLoaderPath (L.nub dirs)
    setEmptyPath =
        emitWarning "Please set AO_PATH to a directory of '.ao' files." >>
        setLoaderPath []
    setLoaderPath p = modify $ \ ld -> ld { ld_path = p }




splitImports :: String -> [String]
splitImports = L.words

{-

-- load text for a given import.
-- (will print error then return empty text if import not found.)
importText :: Import -> IO Text
importText imp =
    getAO_PATH >>= \ fdirs ->
    let fn = FS.fromText imp FS.<.> T.pack "ao" in
    let fpaths = map (FS.</> fn) fdirs in
    firstM (map FS.readTextFile fpaths) >>= \ mbT ->
    case mbT of
        Just txt -> return txt
        Nothing -> 
            let eMsg = "unable find `" ++ T.unpack imp 
                       ++ "` for import"
            in
            reportError eMsg >> 
            return T.empty

-- return first success from list of actions)
firstM :: [IO a] -> IO (Maybe a)
firstM [] = return Nothing
firstM (op:ops) = 
    Err.tryIOError op >>= 
    either (const (firstM ops)) (return . Just)

-- import then parse.
importDictFile :: Import -> IO DictFile
importDictFile imp = readDictFileText imp <$> importText imp

-- deep load a dictionary from an initial import
--   loads each import exactly once (no cycles or redundancies)
--   order in list is such that rightmost words should override leftmost
importDictFiles :: Import -> IO [(Import,DictFile)]
importDictFiles = importDictFiles' [] . (:[])

importDictFiles' :: [(Import,DictFile)] -> [Import] -> IO [(Import,DictFile)]
importDictFiles' done [] = return done
importDictFiles' done (imp:imps) =
    let alreadyImported = L.elem imp (map fst done) in
    if alreadyImported then importDictFiles' done imps else
    importDictFile imp >>= \ df ->
    let imps' = L.reverse (df_imports df) ++ imps in
    importDictFiles' ((imp,df):done) imps'


module AO.ParseAO
    ( readDictFileText
    , parseEntry, parseAODef, parseAction
    , parseNumber, parseMultiLineText, parseInlineText
    ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Data.Ratio
import Data.Text (Text)
import Data.Either (partitionEithers)
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import Text.Parsec.Text()
import AO.AOTypes
import AO.V

readDictFileText :: Import -> Text -> DictFile
readDictFileText imp = readDictFileE imp . splitEntries . dropBOM

-- ignore a leading byte order mark (added by some text editors)
dropBOM :: Text -> Text
dropBOM t = case T.uncons t of { Just ('\xfeff', t') -> t' ; _ -> t }

-- entries are separated by "\n@". Exceptional case if the first
-- character is '@', so handle that.
splitEntries :: Text -> [(Line,Text)]
splitEntries t = 
    case T.uncons t of 
        Just ('@', t') -> (0, T.empty) : lnum 1 (T.splitOn eSep t') 
        _ -> lnum 1 (T.splitOn eSep t)
    where eSep = T.pack "\n@"

-- recover line numbers for each entry
lnum :: Line -> [Text] -> [(Line,Text)]
lnum _ [] = []
lnum n (e:es) = (n,e) : lnum n' es where
    n' = T.foldl accumLF (1 + n) e
    accumLF ct '\n' = 1 + ct
    accumLF ct _ = ct

-- first entry is imports, other entries are definitions
readDictFileE :: Import -> [(Line,Text)] -> DictFile
readDictFileE _ [] = DictFile [] [] []
readDictFileE imp (impEnt:defEnts) = DictFile imps defs errs where
    imps = splitImports (snd impEnt)
    (errL,defL) = (partitionEithers . map readEnt) defEnts
    readEnt = distrib . second (P.parse parseEntry "")
    errs = map etext errL
    defs = map swizzle defL
    swizzle (ln,(w,aodef)) = (w,(ln,aodef))
    fixSrc = (`P.setSourceName` T.unpack imp)
    fixLn ln = (`P.incSourceLine` (ln - 1))
    fixCol 1 = (`P.incSourceColumn` 1) -- offset for the '@'
    fixCol _ = id 
    etext (ln,pe) =
        let fixPos = fixCol ln . fixLn ln . fixSrc in
        let pe' = P.setErrorPos (fixPos (P.errorPos pe)) pe in
        (ln, T.pack (show pe'))

distrib :: (a,Either b c) -> Either (a,b) (a,c)
distrib (a,Left b) = Left (a,b)
distrib (a,Right c) = Right (a,c)


-- | Each entry consists of: 'word definition'. The '@' separating
-- entries has already been removed. The initial word is followed
-- by any word separator - usually a space or newline.
parseEntry :: P.Stream s m Char => P.ParsecT s u m (W,AODef)
parseEntry =
    parseEntryWord >>= \ w ->
    expectWordSep >>
    parseAODef >>= \ def ->
    return (w,def)

-- enforce word separator without consuming anything
expectWordSep :: (P.Stream s m Char) => P.ParsecT s u m ()
expectWordSep = (wordSep P.<|> P.eof) P.<?> "word separator" where
    wordSep = P.lookAhead (P.satisfy isWordSep) >> return ()


-}

