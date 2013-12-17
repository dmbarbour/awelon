{-# LANGUAGE FlexibleContexts, PatternGuards, CPP #-}

-- | This module can read basic AO dictionary files and process
-- an AO dictionary in simple ways for bootstrapping of AO. 
--
-- This implementation of AO performs only moderate validation,
-- mostly to forbid cycles and detect missing words. The dictionary
-- may 
--
-- See AboutAO.md for details on the dictionary file and AO
--
module AO
    ( Action(..)

    -- PROCESSING AO

    -- FILESYSTEM OPERATIONS
    , getAO_PATH, importDictFile, loadFullDict

    -- READERS/PARSERS
    , readDictFile, parseEntry, parseWord, parseAction, parseNumber
    , parseMultiLineText, parseInlineText
    ) where

import Control.Monad
import Control.Exception (assert)
import Control.Arrow (second)
import Data.Either (rights, lefts)
import Data.Ratio
import Data.Text (Text)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import Text.Parsec.Text()
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified System.Environment as Env
import qualified System.IO.Error as Err
import qualified System.IO as Sys
import ABC

type DictF = ([Import],[(Line, ParseEnt)]) -- one dictionary file
type ParseEnt = Either P.ParseError (W,AO) -- one parsed entry (or error)
type Import = Text -- name of dictionary file (minus '.ao' file extension)
type Entry = Text  -- text of entry within a dictionary file
type Line = Int    -- line number for an entry
type W = Text
data Action 
    = Word W        -- ref to dictionary
    | Num Rational  -- literal number
    | Lit Text      -- literal text
    | Block AO      -- block of AO
    | Amb [AO]      -- ambiguous choice of AO (non-empty)
    | Prim ABC      -- inline ABC
    deriving Show
newtype AO = AO [Action] deriving Show
type Error = Text
type Dict = M.Map W AO

---------------------------------------------
-- READER / PARSER FOR AO DICTIONARY FILES --
---------------------------------------------

readDictFile :: Text -> DictF
readDictFile = readDictFileE . splitEntries . dropBOM

-- drop initial Byte Order Mark from text (if necessary)
dropBOM :: Text -> Text
dropBOM t = case T.uncons t of { Just ('\xfeff', t') -> t' ; _ -> t }

-- split entries (by '\n@'); handle edge case starting with @
splitEntries :: Text -> [(Line,Entry)]
splitEntries t = 
    case T.uncons t of 
        Just ('@', t') -> (0, T.empty) : lnum 1 (T.splitOn eSep t') 
        _ -> lnum 1 (T.splitOn eSep t)
    where eSep = T.pack "\n@"

-- recover line numbers for each entry
lnum :: Line -> [Entry] -> [(Line,Entry)]
lnum _ [] = []
lnum n (e:es) = (n,e) : lnum n' es where
    n' = T.foldl accumLF (1 + n) e
    accumLF ct '\n' = 1 + ct
    accumLF ct _ = ct

-- process entries
readDictFileE :: [(Line,Entry)] -> DictF
readDictFileE (imps:defs) = (impL,defL) where
    impL = splitImports (snd imps)
    defL = map (second (P.parse parseEntry "")) defs
readDictFileE _ = error "error in splitEntries"

-- imports simply whitespace separated
splitImports :: Entry -> [Import]
splitImports t =
    let tAfterWS = T.dropWhile isSpace t in
    if T.null tAfterWS then [] else
    let (imp,t') = T.break isSpace tAfterWS in
    imp : splitImports t'

-- parse entry after having separated entries.
parseEntry :: P.Stream s m Char => P.ParsecT s u m (W,AO)
parseEntry =
    parseWord >>= \ w ->
    P.manyTill parseAction P.eof >>= \ actions ->
    return (w, AO actions)

parseWord :: (P.Stream s m Char) => P.ParsecT s u m W
parseWord = 
    (P.satisfy isWordStart P.<?> "start of word") >>= \ c1 ->
    P.many (P.satisfy isWordCont) >>= \ cs ->
    (expectWordSep P.<?> "word separator or continuing word character") >> 
    return (T.pack (c1:cs))

expectWordSep :: (P.Stream s m Char) => P.ParsecT s u m ()
expectWordSep = (wordSep P.<|> P.eof) P.<?> "word separator" where
    wordSep = P.lookAhead (P.satisfy isWordSep) >> return ()

-- An AO action is word, text, number, block, amb, or inline ABC.
-- Whitespace is preserved as inline ABC.
parseAction :: (P.Stream s m Char) => P.ParsecT s u m Action
parseAction = parser P.<?> "word or primitive" where
    parser = word P.<|> spaces P.<|> prim P.<|> text P.<|> 
             number P.<|> block P.<|> amb
    prim = P.char '%' >> ((invocation P.<|> inlineABC) P.<?> "inline ABC")
    invocation =
        P.char '{' >> 
        P.manyTill (P.satisfy isTokenChar) (P.char '}') >>= \ txt ->
        expectWordSep >>
        return ((Prim . ABC) [Invoke (T.pack txt)])
    inlineABC = 
        P.many1 (P.oneOf inlineOpCodeList) >>= \ ops ->
        (expectWordSep P.<?> "word separator or ABC code") >>
        return ((Prim . ABC) (map Op ops))
    spaces = -- spaces are preserved as inline ABC for now
        P.many1 (P.satisfy isSpace) >>= \ ws -> 
        return ((Prim . ABC) (map Op ws))
    word = parseWord >>= return . Word
    text = (parseInlineText P.<|> parseMultiLineText) >>= return . Lit
    number = parseNumber >>= \ r -> expectWordSep >> return (Num r)
    block = 
        P.char '[' >> 
        P.manyTill parseAction (P.char ']') >>= 
        return . Block . AO
    amb =
        P.char '(' >>
        P.sepBy1 (P.many parseAction) (P.char '|') >>= \ opts ->
        P.char ')' >>
        return (Amb (map AO opts))

-- AO is sensitive to line starts for multi-line vs. inline text.
atLineStart, notAtLineStart :: (Monad m) => P.ParsecT s u m ()
atLineStart = P.getPosition >>= \ pos -> when (P.sourceColumn pos > 1) mzero
notAtLineStart = P.getPosition >>= \ pos -> unless (P.sourceColumn pos > 1) mzero 
        
parseMultiLineText, lineOfText, parseInlineText 
    :: (P.Stream s m Char) => P.ParsecT s u m Text
parseMultiLineText = 
    atLineStart >> P.char '"' >> 
    lineOfText >>= \ firstLine ->
    P.manyTill (P.char ' ' >> lineOfText) (P.char '~') >>= \ moreLines ->
    expectWordSep >> -- require word separator after ~
    return (T.intercalate (T.singleton '\n') (firstLine : moreLines))

lineOfText = P.manyTill (P.satisfy (/= '\n')) (P.char '\n') >>= return . T.pack

parseInlineText = 
    notAtLineStart >> P.char '"' >> 
    P.manyTill (P.satisfy isInlineTextChar) (P.char '"') >>= \ txt ->
    expectWordSep >> -- require word separator after end quote
    return (T.pack txt)

-- Numbers in AO are intended to be convenient for human users. The
-- cost is that there are many formats to parse. The following
-- formats are supported:
--   integral (e.g. 42)
--   decimal  (e.g. 12.3)
--   fractional (e.g. 2/3)
--   scientific (e.g. 3.4e5)
--   percentile (e.g. 98.7%)
--   hexadecimal (e.g. 0x221E; natural numbers only)
-- These are similar to some extent. This parser attempts to 
-- reuse partial matches. All numbers are converted to exact
-- rationals in the end.
parseNumber :: P.Stream s m Char => P.ParsecT s u m Rational
parseNumber = parser P.<?> "number" where
    parser = (P.try parseHexadecimal) P.<|> parseDecimal
    parseHexadecimal =
        P.char '0' >> P.char 'x' >> 
        P.many1 (P.satisfy isHexDigit) >>=
        return . fromIntegral . hexToNum
    parseDecimal = 
        P.option False (P.char '-' >> return True) >>= \ bNeg ->
        parseUnsignedIntegral >>= \ n ->
        parseFragment n >>= \ r ->
        return (if bNeg then (negate r) else r)
    parseUnsignedIntegral = zeroInt P.<|> posInt
    zeroInt = P.char '0' >> return 0
    posInt = 
        P.satisfy isNZDigit >>= \ c1 ->
        P.many (P.satisfy isDigit) >>= \ cs ->
        return (decToNum (c1:cs))
    parseFragment n =
        (P.char '/' >> fractional n) P.<|>
        (P.char '.' >> decimalDot n) P.<|>
        (postDecFragment (fromIntegral n))
    fractional num = posInt >>= \ den -> return (num % den)
    decimalDot n = 
        P.many1 (P.satisfy isDigit) >>= \ ds ->
        let fNum = decToNum ds in
        let fDen = 10 ^ length ds in
        let f = fNum % fDen in
        assert ((0 <= f) && (f < 1)) $
        let r = f + fromIntegral n in
        postDecFragment r
    postDecFragment r =
        (P.char '%' >> return (r * (1 % 100))) P.<|>
        (P.char 'e' >> scientific r) P.<|>
        (return r)
    scientific r =
        P.option False (P.char '-' >> return True) >>= \ bNeg ->
        parseUnsignedIntegral >>= \ n ->
        let factor = 10 ^ n in
        if bNeg then return (r * (1 % factor))
                else return (r * fromInteger factor)

hexToNum :: [Char] -> Integer
hexToNum = foldl addHexDigit 0 where
    addHexDigit n c = 16*n + (fromIntegral (h2i c))
    h2i c | (('0' <= c) && (c <= '9')) = fromEnum c - fromEnum '0'
          | (('a' <= c) && (c <= 'f')) = 10 + fromEnum c - fromEnum 'a'
          | (('A' <= c) && (c <= 'F')) = 10 + fromEnum c - fromEnum 'A'
          | otherwise = error "illegal hex digit"

decToNum :: [Char] -> Integer
decToNum = foldl addDecDigit 0 where
    addDecDigit n c = 10 * n + (fromIntegral (d2i c))
    d2i c | (('0' <= c) && (c <= '9')) = fromEnum c - fromEnum '0'
          | otherwise = error "illegal decimal digit"

----------------------------------
-- Character Predicates for AO
----------------------------------

-- AO recognizes only two kinds of whitespace - SP and LF
-- 
-- I have several character predicates to support parsing that might
-- be slightly distinct from Data.Char.
isSpace, isControl, isDigit, isNZDigit, isHexDigit :: Char -> Bool
isSpace c = (' ' == c) || ('\n' == c)
isControl c = isC0 || isC1orDEL where
    n = fromEnum c
    isC0 = n <= 0x1F
    isC1orDEL = n >= 0x7F && n <= 0x9F
isDigit c = ('0' <= c) && (c <= '9')
isNZDigit c = isDigit c && not ('0' == c)
isHexDigit c = isDigit c || smallAF || bigAF where
    smallAF = ('a' <= c) && (c <= 'f')
    bigAF = ('A' <= c) && (c <= 'F')

-- words in AO are separated by spaces, [], (|). They also may not
-- start the same as a number, %inlineABC, or an @word entry.
isWordSep, isWordStart, isWordCont :: Char -> Bool
isWordSep c = isSpace c || block || amb where
    block = '[' == c || ']' == c
    amb = '(' == c || '|' == c || ')' == c
isWordCont c = not (isWordSep c || isControl c || '"' == c)
isWordStart c = isWordCont c && not (number || '%' == c || '@' == c) where
    number = isDigit c || '-' == c

-- tokens in AO are described with %{...}. They can have most
-- characters, except for {, }, and LF.
isTokenChar, isInlineTextChar :: Char -> Bool
isTokenChar c = not ('{' == c || '\n' == c || '}' == c)
isInlineTextChar c = not ('"' == c || '\n' == c)

------------------------
-- FILESYSTEM LOADERS --
------------------------

-- obtain unique, canonicalized AO_PATHs
getAO_PATH :: IO [FS.FilePath]
getAO_PATH = 
    Env.lookupEnv "AO_PATH" >>= \ aop ->
    case aop of
        Nothing -> FS.getWorkingDirectory >>= return . (:[])
        Just str ->
            let paths = splitPath str in
            mapM (Err.tryIOError . FS.canonicalizePath) paths >>= \ cps ->
            filterM FS.isDirectory (L.nub $ rights cps)

-- OS-dependent AO_PATH separator
isPathSep :: Char -> Bool
#if defined(WinPathFmt)
isPathSep = (== ';')
#else
isPathSep = (== ':')
#endif

splitPath :: String -> [FS.FilePath]
splitPath = map FS.fromText . T.split isPathSep . T.pack 

reportError :: (Show e) => e -> IO ()
reportError = Sys.hPutStrLn Sys.stderr . show

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
    let fn = FS.fromText imp FS.<.> (T.pack ".ao") in
    let files = map (FS.</> fn) paths in
    mapM (Err.tryIOError . FS.readFile) files >>= \ loaded ->
    let lpaths = zipWith distrib paths loaded in
    let xerrs = L.filter (not . Err.isDoesNotExistError . snd) $ lefts lpaths in 
    mapM_ reportError xerrs >>
    let uniqueFiles = L.nubBy ((==) `on` snd) $ rights lpaths in
    return (map (second (readDictFile . T.decodeUtf8)) uniqueFiles)

type ImpR = Either Error (FS.FilePath, DictF)

-- import a dictionary file (based on AO_PATH)
-- returns Left if no file found or ambiguous files found
importDictFile :: Import -> IO ImpR
importDictFile imp =
    loadDictFiles imp >>= \ lpaths ->
    case lpaths of
        ((p,df):[]) -> return (Right (p,df))
        [] -> err missing
        _  -> err (ambiguous (map fst lpaths))
    where 
    err = return . Left
    ambiguous paths = 
        imp `T.append` (T.pack " ambiguous in AO_PATH: ") 
            `T.append` (T.pack (show paths))
    missing = imp `T.append` (T.pack " missing in AO_PATH")

-- | Load a full dictionary from the filesystem.
--
-- Currently the 'root' dictionary is considered separately from the
-- imports model. I'm unsure whether this is a good or bad idea, but
-- it will work. Cycles will still be discovered.
--
-- Errors returned are also reported on stderr. The design here aims 
-- to report many errors in a single run.
--
loadFullDict :: FS.FilePath -> IO ([Error],Dict)
loadFullDict fRoot = 
    Err.tryIOError (FS.readFile fRoot) >>= \ eb ->
    case eb of
        Left err -> -- failed to load root dictionary file
            let etxt = T.pack (show (fRoot, err)) in
            reportError etxt >> return ([etxt],M.empty)
        Right bytes -> 
            let d0 = readDictFile (T.decodeUtf8 bytes) in
            let seed = Right (fRoot, d0) in
            deeplyImport (L.reverse (fst d0)) [(T.empty,seed)] >>= \ impL ->
            let edf = buildDict impL in
            mapM_ reportError (fst edf) >>
            return edf

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

-- buildDict centralizes a lot of processing
-- potential todo: include location info in dict (via annotations)
buildDict :: [(Import,ImpR)] -> ([Error],Dict)
buildDict imps = (errors, dict) where
    errors = importErrors -- ambiguous or missing imports
          ++ importCycleErrors -- cyclic imports
          ++ parseErrors -- bad parse entries
          ++ dictErrors -- cyclic definitions, missing words
    impsD = map (uncurry distrib) imps
    importErrors = map (T.pack . show) $ lefts impsD
    goodImports = rights impsD -- [(Import, (FilePath, DictF))]
    importCycleErrors = map cycToErr $ detectCycles $ map impAdj goodImports
    impAdj (imp,(_,(impList,_))) = (imp,impList)
    cycToErr cyc = 
        T.pack "Import cycle: " `T.append` 
        T.intercalate (T.pack " \x2192 ") cyc
    parseEnts = L.concatMap parseEnt goodImports -- [Either (FP,(L,P.Err)) (FP, (L, (W, AO)))] 
    parseEnt (_, (fp, (_, ents))) = map (distrib fp . uncurry distrib) ents
    parseErrors = map showParseError (lefts parseEnts)
    showParseError (fp,(ln,err)) = T.pack (show (modErrPos (fixpos fp ln) err))
    modErrPos f e = P.setErrorPos (f (P.errorPos e)) e
    fixpos fp ln = (`P.setSourceName` (show fp)) . (`P.incSourceLine` (ln - 1))
    d0 = M.fromList (map (snd . snd) (rights parseEnts))
    (dictErrors, dict) = processDict d0
       
-- AO forbids recursive import lists and definitions. It is important
-- that these errors be caught to avoid infinite expansions for any
-- definitions.
--
-- This function reports cycles based on a depth first search on an
-- adjacency list. 
--
-- Cycles are returned as lists of elements, with the last item
-- implicitly linked to the first. 
detectCycles :: (Ord v) => [(v,[v])] -> [[v]]
detectCycles = deforest . M.map (L.sort . L.nub) . M.fromListWith (++) where
    deforest g = case M.minViewWithKey g of
        Nothing -> []
        Just ((v0,vs),_) -> 
            let (visited, cycles) = dfs g [v0] [v0] vs in
            let g' = L.foldr M.delete g visited in
            cycles ++ deforest g'
    dfs _ cx _ [] = (cx, [])
    dfs g cx p (v:vs) = 
        case (L.elemIndex v p) of
            Nothing -> -- no loop; may need to visit 'v'
                if L.elem v cx then dfs g cx p vs else 
                let (cxd,cycd) = dfs g (v:cx) (v:p) (edgesFrom g v) in
                let (cxw,cycw) = dfs g cxd p vs in
                (cxw, cycd ++ cycw)
            Just n -> -- loop found; 'v' necessarily has been visited 
                assert (L.elem v cx) $ -- assert visited v
                let cyc0 = L.reverse (L.take (n + 1) p) in
                let (cxw,cycw) = dfs g cx p vs in
                (cxw,(cyc0:cycw))
    edgesFrom g v = maybe [] id $ M.lookup v g

-- basic post-process of dictionary...
--
--  * report missing or cyclic defs
--  * remove cyclic defs from dictionary
--  * remove incomplete defs from dictionary
--
processDict :: Dict -> ([Error], Dict)
processDict d0 = (errors, df) where
    cycles = detectCycles (M.toList (M.map aoWords d0))
    cycleErrors = map showCycleError cycles
    showCycleError cyc =
        T.pack "Cyclic def detected: " `T.append`
        T.intercalate (T.pack " \x2192 ") cyc
    d' = L.foldr M.delete d0 (L.concat cycles) -- d0 without cycles
    df = d' -- TODO: remove incomplete defs
    errors = cycleErrors ++ incompleteDefErrors
    incompleteDefErrors = [] -- TODO!

-- extract words from a definition to help detect cycles or missing
-- definitions. Most words in AO have small definitions, so extracting
-- words from a definition is fairly cheap.
aoWords :: AO -> [W]
aoWords (AO ao) = L.concatMap actionWords ao

actionWords :: Action -> [W] 
actionWords (Word w) = [w]
actionWords (Block ao) = aoWords ao
actionWords (Amb opts) = L.concatMap aoWords opts
actionWords _ = []



