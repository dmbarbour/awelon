{-# LANGUAGE CPP #-}

-- | This module is responsible for loading '.ao' files from the
-- filesystem, reading AO_PATH, and generally interacting with
-- the operating system.
module AO.LoadAO 
    (
    ) where

-- OS-dependent AO_PATH separator
isPathSep :: Char -> Bool
#if defined(WinPathFmt)
isPathSep = (== ';') -- flag defined if os(windows) in cabal file
#else
isPathSep = (== ':') -- suitable for most *nix systems and Mac
#endif

{-
------------------------
-- FILESYSTEM LOADERS --
------------------------

-- obtain unique, canonicalized AO_PATHs
getAO_PATH :: IO [FS.FilePath]
getAO_PATH = 
    Err.tryIOError (Env.getEnv "AO_PATH") >>= \ aop ->
    case aop of
        Left _ -> FS.getWorkingDirectory >>= return . (:[])
        Right str ->
            let paths = splitPath str in
            mapM (Err.tryIOError . FS.canonicalizePath) paths >>= \ cps ->
            filterM FS.isDirectory (L.nub $ rights cps)


splitPath :: String -> [FS.FilePath]
splitPath = map FS.fromText . T.split isPathSep . T.pack 

reportError :: Text -> IO ()
reportError = Sys.hPutStrLn Sys.stderr . T.unpack

distrib :: a -> Either b c -> Either (a,b) (a,c)
distrib a (Left b) = Left (a,b)
distrib a (Right c) = Right (a,c)

-- load unique dict files for a given import identifier
-- implicit argument: AO_PATH environment variable
--
-- load errors (other than 'does not exist') reported on stderr to
--   help detect permission errors and similar
--
-- 'false' ambiguity - where multiple instances of an import have
-- the exact same text - are ignored.
loadDictFiles :: Import -> IO [(FS.FilePath, DictF)]
loadDictFiles imp = 
    getAO_PATH >>= \ paths ->
    let fn = FS.fromText imp FS.<.> (T.pack "ao") in
    let files = map (FS.</> fn) paths in
    mapM (Err.tryIOError . FS.readFile) files >>= \ loaded ->
    let errs = L.filter (not . Err.isDoesNotExistError) $ lefts loaded in
    mapM_ (reportError . T.pack . show) errs >>
    let lp = rights $ zipWith distrib files loaded in
    let uf = L.nubBy ((==) `on` snd) lp in
    let ds = map (second (readDictFile . T.decodeUtf8)) uf in
    return ds

type ImpR = Either Error (FS.FilePath, DictF)

-- import a dictionary file (based on AO_PATH)
-- returns Left if no file found or ambiguous files found
importDictFile :: Import -> IO ImpR
importDictFile imp =
    loadDictFiles imp >>= \ dicts ->
    case dicts of
        ((p,df):[]) -> return (Right (p,df))
        [] -> err missing
        _  -> err (ambiguous (map fst dicts))
    where 
    err = return . Left
    ambiguous paths = 
        imp `T.append` (T.pack " ambiguous in AO_PATH: ") 
            `T.append` (T.concat $ map showPath paths)
    showPath = T.append (T.pack "\n    ") . either id id . FS.toText
    missing = imp `T.append` (T.pack " missing in AO_PATH")

-- | Load a full dictionary from the filesystem. Does not process 
-- the dictionary, other than to load it.
--
-- Currently the 'root' dictionary is considered separately from the
-- imports model. I'm unsure whether this is a good or bad idea, but
-- it will work. Import will still be discovered.
--
-- A few specific errors - e.g filesystem permission failures, are not
-- recorded as errors relevant to AO. Those are reported to stderr.
--
loadDict :: FS.FilePath -> IO ([Error],Dict)
loadDict fRoot = 
    Err.tryIOError (FS.readFile fRoot) >>= \ eb ->
    case eb of
        Left err -> -- failed to load root dictionary file
            let etxt = T.pack (show (fRoot, err)) in
            reportError etxt >> return ([etxt],M.empty)
        Right bytes -> 
            let d0 = readDictFile (T.decodeUtf8 bytes) in
            let seed = Right (fRoot, d0) in
            deeplyImport (L.reverse (fst d0)) [(T.empty,seed)] >>= 
            return . buildDict

-- load a dictionary, then compile it, and combine the errors
loadDictC :: FS.FilePath -> IO ([Error],DictC)
loadDictC fRoot =
    loadDict fRoot >>= \ (loadErrors, dict) ->
    let (dictErrors, dictC) = compileDict dict in
    return (loadErrors ++ dictErrors, dictC)

-- import a dictionary given just a list of imports (ordered same as
-- an AO file import section, i.e. rightmost has priority).
importDict :: [Import] -> IO ([Error], Dict)
importDict imps = deeplyImport (L.reverse imps) [] >>= return . buildDict

-- import and compile a dictionary
importDictC :: [Import] -> IO ([Error], DictC)
importDictC imps = 
    importDict imps >>= \ (loadErrors, dict) ->
    let (dictErrors, dictC) = compileDict dict in
    return (loadErrors ++ dictErrors, dictC)
    
-- recursively load imports. AO's import semantics is to load each 
-- import into the dictionary, left to right. However, this can be
-- optimized by loading right to left and skipping redundant loads.
-- Each distinct import is loaded only once. The full dictionary is
-- loaded.
--
-- Cycles, at this point, will be broken at an arbitrary position.
-- They'll be properly detected later.
deeplyImport :: [Import] -> [(Import,ImpR)] -> IO [(Import, ImpR)]
deeplyImport [] impsDone = return impsDone
deeplyImport (imp:imps) impsDone = 
    case L.lookup imp impsDone of
        Just _ -> deeplyImport imps impsDone -- import already loaded
        Nothing ->
            importDictFile imp >>= \ impR ->
            let impsDeep = either (const []) (L.reverse . fst . snd) impR in
            deeplyImport (impsDeep ++ imps) ((imp,impR):impsDone)

-}