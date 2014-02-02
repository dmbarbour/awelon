{-# LANGUAGE FlexibleContexts #-}
-- This file provides the main AST and parsers for an AO
-- dictionary file. It does not process AO, beyond parsing it.
-- (Which is to say, it does not detect cycles, missing words,
-- or other problems.) It also doesn't load imports recursively.
module AO.ParseAO
    ( readDictFileText, parseEntry
    , parseAODef, parseAction
    , parseWord, parseNumber
    , parseMultiLineText, parseInlineText
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

-- imports simply whitespace separated
splitImports :: Text -> [Import]
splitImports t =
    let tAfterWS = T.dropWhile isSpace t in
    if T.null tAfterWS then [] else
    let (imp,t') = T.break isSpace tAfterWS in
    imp : splitImports t'

-- | Each entry consists of: 'word definition'. The '@' separating
-- entries has already been removed. The initial word is followed
-- by any word separator - usually a space or newline.
parseEntry :: P.Stream s m Char => P.ParsecT s u m (W,AODef)
parseEntry =
    parseWord >>= \ w ->
    parseAODef >>= \ def ->
    return (w,def)

-- enforce word separator without consuming anything
expectWordSep :: (P.Stream s m Char) => P.ParsecT s u m ()
expectWordSep = (wordSep P.<|> P.eof) P.<?> "word separator" where
    wordSep = P.lookAhead (P.satisfy isWordSep) >> return ()

parseWord :: P.Stream s m Char => P.ParsecT s u m W
parseWord =
    P.satisfy isWordStart >>= \ c1 ->
    P.many (P.satisfy isWordCont) >>= \ cs ->
    expectWordSep >>
    return (T.pack (c1:cs))

-- | a definition is trivially a sequence of AO actions
parseAODef :: P.Stream s m Char => P.ParsecT s u m AODef
parseAODef = S.fromList <$> P.manyTill parseAction P.eof

-- adverbs are allowed as naked words, and operate similarly
-- to inline ABC: `\adverb` expands to `\a \d \v \e \r \b`.
-- The main difference is that these operations are user defined!
--actionAdverbs :: P.Stream s m Char => P.ParsecT s u m [ADV]
--actionAdverbs = P.char '\\' >> P.many1 advChar

-- I don't want to allow hard-coding of arbitrary capabilities in AO,
-- so I'll do a little filtering at parse time. A few capabilities are
-- allowed:
--    prefix '&' for annotations
--    prefix ':' for sealers
--    prefix '.' for unsealers
validToken :: String -> Bool
validToken [] = False
validToken (c:_) = ('&' == c) || (':' == c) || ('.' == c)

-- An AO action is word, text, number, block, amb, or inline ABC.
-- Whitespace is preserved as inline ABC.
parseAction :: (P.Stream s m Char) => P.ParsecT s u m Action
parseAction = parser P.<?> "word or primitive" where
    parser = word P.<|> spaces P.<|> aoblock P.<|> amb P.<|>
             number P.<|> text P.<|> prim 
    word = Word <$> parseWord
    text = Lit <$> (parseInlineText P.<|> parseMultiLineText)
    spaces = (Prim . S.fromList . map Op) <$> P.many1 (P.satisfy isSpace)
    prim = P.char '%' >> ((inlineTok P.<|> inlineABC) P.<?> "inline ABC")
    inlineTok = 
        P.char '{' >> 
        P.manyTill (P.satisfy isTokenChar) (P.char '}') >>= \ txt ->
        expectWordSep >> assertValidToken txt >> 
        return ((Prim . S.singleton . Invoke . T.pack) txt)
    assertValidToken t = unless (validToken t) $ P.unexpected 
        ("unrecognized token %{" ++ t ++ "}")
    inlineABC = 
        P.many1 (P.oneOf inlineOpCodeList) >>= \ ops ->
        (expectWordSep P.<?> "word separator or ABC code") >>
        return ((Prim . S.fromList . map Op) ops)
    number = parseNumber >>= \ r -> expectWordSep >> return (Num r)
    aoblock = 
        P.char '[' >> P.manyTill parseAction (P.char ']') >>= 
        return . BAO . S.fromList
    amb =
        P.char '(' >>
        P.sepBy1 (P.many parseAction) (P.char '|') >>= \ options ->
        P.char ')' >>
        return ((Amb . (map S.fromList)) options)

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

lineOfText = T.pack <$> P.manyTill P.anyChar (P.char '\n')

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
--isAdvChar :: Char -> Bool
--isAdvChar c = isWordCont c 

-- tokens in AO are described with %{...}. They can have most
-- characters, except for {, }, and LF. In general, developers
-- are limited in what kind of capabilities they may hard-code
-- into source, but the limit is enforced downstream.
isTokenChar, isInlineTextChar :: Char -> Bool
isTokenChar c = not ('{' == c || '\n' == c || '}' == c)
isInlineTextChar c = not ('"' == c || '\n' == c)


