{-# LANGUAGE FlexibleContexts, PatternGuards, CPP #-}

-- | Code to work with AO files...
--
module AO.AO
    ( Action(..), AO(..), Dict, DictC, Import


    
    

    -- LOADING AND PROCESSING AO
    , loadDict, loadDictC, importDict, importDictC
    , aoWords, compileDict, compileAO, compileAction
    , applyWithAdverbs

    -- READERS/PARSERS
    , readDictFile, parseEntry
    , parseEntryWord, parseBasicWord, parseFullWord
    , parseAction, parseNumber
    , parseMultiLineText, parseInlineText
    ) where

import Control.Monad
import Control.Exception (assert)
import Control.Arrow (second)
import Data.Either (rights, lefts)
import Data.Ratio ((%))
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
import AO.ABC


applyWithAdverbs :: W
applyWithAdverbs = T.pack "applyWithAdverbs"

------------------------------
-- PROCESSING OF DICTIONARY --
------------------------------

-- | Compile an acyclic dictionary (from AO to ABC). 
-- 
-- At the moment, no typechecking is performed. The words are
-- simply expanded to ABC. Each word is expanded only once.
--
-- Errors may be reported if a word is missing.
compileDict :: Dict -> ([Error], DictC)
compileDict dict = (errors, dictC) where
    (dictMW,dictC) = L.foldl compileW (M.empty, M.empty) (M.toList dict)
    missingErrors = L.map mwError $ M.toList dictMW
    mwError (w,mws) = 
        T.pack "ERROR: word " `T.append` w `T.append` 
        T.pack " requires definitions for: " `T.append` T.unwords mws
    errors = missingErrors
    loadW w = distrib w $ maybe (Left ()) Right $ M.lookup w dict
    compileW r (w,def) = case (M.lookup w (snd r), M.lookup w (fst r)) of
        (Just _, _) -> r -- word has already been compiled
        (_, Just _) -> r -- word cannot be compiled (missing words)
        (Nothing, Nothing) -> -- attempt to compile this word
            -- first compile words used by this def into dictionary
            let reqWords = rights $ map loadW $ aoWords def in 
            let (dmw,dc) = L.foldl compileW r reqWords in
            -- compile the provided definition; use updated dictionary
            case compileAO dc def of
                Left mws -> (M.insert w (L.nub mws) dmw , dc)
                Right abcdef -> (dmw , M.insert w abcdef dc)

-- compile AO code given a dictionary containing the required
-- word definitions. If words are missing, it will return a 
-- list of those, instead.
compileAO :: DictC -> AO -> Either [W] ABC
compileAO dc (AO actions) = 
    let r0 = map (compileAction dc) actions in
    let abcOps (ABC ops) = ops in
    case lefts r0 of
        [] -> Right $ ABC $ L.concat $ map abcOps $ rights r0
        lw -> Left (L.concat lw)

-- compile a single action, given a precompiled dictionary 
-- containing all required words... or report missing words
compileAction :: DictC -> Action -> Either [W] ABC
compileAction dc (Word w []) = compileWord dc w
compileAction dc (Word w advs) =
    -- foo\adv = [foo] [\adv] applyWithAdverbs
    let cW = compileWord dc w in
    let cAdvs = compileAction dc (Adverbs advs) in
    let cAp = compileWord dc applyWithAdverbs in
    let errs = either id (const []) in
    case (cW,cAdvs,cAp) of
        (Right abcW, Right abcAdv, Right abcAp) ->
            Right $ ABC $
            (Qu (B (block abcW)) : Op 'l' : 
             Qu (B (block abcAdv)) : Op 'l' :
             inABC abcAp)
        _ -> Left $ errs cW ++ errs cAp ++ errs cAdvs
compileAction dc (Adverbs advs) = 
    -- \adverbs = \a \d \v \e \r \b \s (from dictionary)
    let cAdvs = L.map (compileWord dc . adverbWord) advs in
    case lefts cAdvs of
        [] -> Right $ ABC $ L.concatMap inABC $ rights cAdvs
        errors -> Left $ L.concat errors
compileAction _ (Num r) = Right $ ABC $ [Qu (N r), Op 'l']
compileAction _ (Lit txt) = Right $ ABC $ [Qu (toABCV txt), Op 'l']
compileAction dc (AOB ao) =
    case compileAO dc ao of
        Left mws -> Left mws -- missing word in a block
        Right abc -> Right $ ABC $ [Qu (B (block abc)), Op 'l']
compileAction dc (Amb options) =
    let r0 = map (compileAO dc) options in
    case lefts r0 of
        [] -> Right $ ABC $ [AMBC (rights r0)]
        lw -> Left (L.concat lw)
compileAction _ (Prim abc) = Right abc

compileWord :: DictC -> W -> Either [W] ABC
compileWord dc w = 
    case M.lookup w dc of
        Nothing -> Left [w]
        Just abc -> Right abc

-- extract words from a definition to help detect cycles or missing
-- definitions. Most words in AO have small definitions, so extracting
-- words from a definition is fairly cheap.
aoWords :: AO -> [W]
aoWords (AO ao) = L.concatMap actionWords ao

actionWords :: Action -> [W] 
actionWords (Word w advs) = w : adverbModWords advs
actionWords (Adverbs advs) = map adverbWord advs 
actionWords (AOB ao) = aoWords ao
actionWords (Amb opts) = L.concatMap aoWords opts
actionWords _ = []

-- if we have any adverbs, we must also use word 
--   applyWithAdverbs, otherwise there are none
adverbModWords :: [ADV] -> [W]
adverbModWords [] = []
adverbModWords advs = applyWithAdverbs : map adverbWord advs

-- each adverb is a two character word of form `\c`.
adverbWord :: ADV -> W
adverbWord = T.cons '\\' . T.singleton

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
    parseEntryWord >>= \ w ->
    expectWordSep >>
    P.manyTill parseAction P.eof >>= \ actions ->
    return (w, AO actions)

-- single word or adverb being defined
parseEntryWord :: P.Stream s m Char => P.ParsecT s u m W
parseEntryWord = (adverb P.<|> parseBasicWord) P.<?> "entry word or adverb" 
    where adverb = P.char '\\' >> advChar >>= return . adverbWord

parseBasicWord :: P.Stream s m Char => P.ParsecT s u m W
parseBasicWord =
    (P.satisfy isWordStart P.<?> "start of word") >>= \ c1 ->
    P.many (P.satisfy isWordCont) >>= \ cs ->
    return (T.pack (c1:cs))

-- a full word has optional adverbs
parseFullWord :: P.Stream s m Char => P.ParsecT s u m (W,[ADV])
parseFullWord =
    parseBasicWord >>= \ w ->
    P.option [] actionAdverbs >>= \ advs ->
    expectWordSep >>
    return (w,advs)

advChar :: P.Stream s m Char => P.ParsecT s u m ADV
advChar = P.satisfy isAdvChar

-- adverbs are allowed as naked words, and operate similarly
-- to inline ABC: `\adverb` expands to `\a \d \v \e \r \b`.
-- The main difference is that these operations are user defined!
actionAdverbs :: P.Stream s m Char => P.ParsecT s u m [ADV]
actionAdverbs = P.char '\\' >> (P.many1 advChar P.<?> "adverb")

expectWordSep :: (P.Stream s m Char) => P.ParsecT s u m ()
expectWordSep = (wordSep P.<|> P.eof) P.<?> "word separator" where
    wordSep = P.lookAhead (P.satisfy isWordSep) >> return ()

-- I don't want to allow hard-coding of arbitrary capabilities in AO.
-- Instead, certain tokens are allowed based on having non-operational
-- meaning.
--    prefix '&' for annotations
--    prefix '$' for sealers
--    prefix '/' for unsealers
validToken :: String -> Bool
validToken [] = False
validToken (c:_) = ('&' == c) || ('$' == c) || ('/' == c)

-- An AO action is word, text, number, block, amb, or inline ABC.
-- Whitespace is preserved as inline ABC.
parseAction :: (P.Stream s m Char) => P.ParsecT s u m Action
parseAction = parser P.<?> "word or primitive" where
    parser = number P.<|> text P.<|> spaces P.<|>
             word P.<|> adverbs P.<|> prim P.<|>  
             aoblock P.<|> amb
    prim = P.char '%' >> ((inlineTok P.<|> inlineABC) P.<?> "inline ABC")
    inlineTok = 
        P.char '{' >>
        P.manyTill (P.satisfy isTokenChar) (P.char '}') >>= \ txt ->
        expectWordSep >>
        assertValidToken txt >>
        return ((Prim . ABC) [Invoke (T.pack txt)])
    assertValidToken t = unless (validToken t) $ P.unexpected 
        ("unrecognized token %{" ++ t ++ "}")
    inlineABC = 
        P.many1 (P.oneOf inlineOpCodeList) >>= \ ops ->
        (expectWordSep P.<?> "word separator or ABC code") >>
        return ((Prim . ABC) (map Op ops))
    adverbs = 
        actionAdverbs >>= \ advs ->
        expectWordSep >>
        return (Adverbs advs)
    spaces = -- spaces are preserved as inline ABC for now
        P.many1 (P.satisfy isSpace) >>= \ ws -> 
        return ((Prim . ABC) (map Op ws))
    word = parseFullWord >>= \ (w,advs) -> return (Word w advs)
    text = (parseInlineText P.<|> parseMultiLineText) >>= return . Lit
    number = 
        parseNumber >>= \ r -> 
        expectWordSep >> 
        return (Num r)
    aoblock = 
        P.char '[' >> 
        P.manyTill parseAction (P.char ']') >>= 
        return . AOB . AO
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
    parseUnsignedIntegral = (zeroInt P.<|> posInt) P.<?> "digits"
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
isWordSep c = isSpace c || bracket || amb where
    bracket = '[' == c || ']' == c
    amb = '(' == c || '|' == c || ')' == c
isWordCont c = not (isWordSep c || isControl c || '"' == c || '\\' == c)
isWordStart c = isWordCont c && not (number || '%' == c || '@' == c) where
    number = isDigit c || '-' == c

-- adverb characters
isAdvChar :: Char -> Bool
isAdvChar c = isWordCont c 

-- tokens in AO are described with %{...}. They can have most
-- characters, except for {, }, and LF. In general, developers
-- are limited in what kind of capabilities they may hard-code
-- into source, but the limit is enforced downstream.
isTokenChar, isInlineTextChar :: Char -> Bool
isTokenChar c = not ('{' == c || '\n' == c || '}' == c)
isInlineTextChar c = not ('"' == c || '\n' == c)


-- buildDict centralizes a lot of processing
-- reports many errors, and removes cyclic definitions
buildDict :: [(Import,ImpR)] -> ([Error],Dict)
buildDict imps = (errors, dictCycleFree) where
    errors = importErrors -- ambiguous or missing imports
          ++ importCycleErrors -- cyclic imports
          ++ parseErrors -- bad parse entries
          ++ dupWarnings -- warn whenever a word is defined twice
          ++ defCycleErrors
    defCycles = (detectCycles . M.toList . M.map aoWords) dict
    defCycleErrors = map showCycleError defCycles
    showCycleError cyc =
        T.pack "ERROR: cycle among definitions: " `T.append`
        T.intercalate (T.pack " \x2192 ") cyc
    dictCycleFree = L.foldr M.delete dict (L.concat defCycles) -- dict without cycles
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
    -- list of words, including duplicates (value is non-empty list)
    dictM = M.fromListWith (++) $ L.map toEntM (rights parseEnts)
    toEntM x@(_, (_, (w, _))) = (w, [x])
    dict = M.map (toDictEnt . L.head) dictM
    toDictEnt (fp, (l, (w, ao))) = withFrame fp l w ao
    dupWarnings = L.map toDupWarning $ M.toList $ M.filter hasDupDef dictM
    hasDupDef (_:_:_) = True
    hasDupDef _ = False
    toDupWarning (w, defs) = 
        T.pack "Word redefined: " `T.append` w `T.append` 
        T.pack " @ " `T.append` T.unwords (L.map dupLoc $ L.reverse defs)
    dupLoc (fp, (l, _)) =
        (either id id . FS.toText . FS.filename) fp 
        `T.snoc` ':' `T.append` (T.pack (show l))

-- Add location annotations to the compiled entries.
--  This adds two annotations: {&@word file line} and {&@-} to push and pop
--  frame information within the AO code. This can help with debugging.
withFrame :: FS.FilePath -> Line -> W -> AO -> AO
withFrame fp l w (AO actions) = AO (inFrame : (actions ++ [exFrame])) where 
    exFrame = Prim $ ABC [Invoke $ T.pack "&@-"]
    inFrame = Prim $ ABC [Invoke $ frameText]
    frameText = T.pack "&@" `T.append` w `T.snoc` '@' 
                `T.append` fptxt `T.snoc` ':' 
                `T.append` (T.pack (show l))
    fptxt = either id id $ FS.toText $ FS.filename fp
       
-- AO forbids recursive import lists and definitions. It is important
-- that these errors be caught to avoid infinite expansions for any
-- definitions.
--
-- This function reports cycles based on a depth first search on an
-- adjacency list. Cycles are reported as lists of linked elements, 
-- with the last item implicitly linked to the first. 
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


