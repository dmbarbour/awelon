{-# LANGUAGE FlexibleContexts, PatternGuards #-}

-- | This module can read basic AO dictionary files and process
-- an AO dictionary in simple ways for bootstrapping of AO. 
--
-- See AboutAO.md for details on the dictionary file and AO
--
module AO
    ( DictFile(..), Src(..), Def(..), Action(..)
    , readDictFile
    , parseEntry, parseWord, parseAction
    , parseNumberUnits, parseNumber, parseUnitString, canonicalUnits
    , parseMultiLineText, parseInlineText
    ) where

import Control.Monad
import Control.Exception (assert)
import Control.Arrow (second)
import Data.Ratio
import Data.Text (Text)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.Parsec as P
import Text.Parsec.Text()
-- import Filesystem
import ABC

data DictFile = DictF [Import] [Def]
data Def = Def W Src (Either Error AO)
data Src = Src Import Integer -- source file & entry number
type Import = Text -- name of dictionary file (minus '.ao' file extension)
type Entry = Text  -- text of entry within a dictionary file
type W = Text
data Action 
    = Word W             -- ref to dictionary
    | Num Rational Units -- literal number
    | Lit Text           -- literal text
    | Block AO           -- block of AO
    | Amb [AO]           -- ambiguous choice of AO (non-empty)
    | Prim ABC           -- inline ABC
    deriving Show
newtype AO = AO [Action] deriving Show
type Units = [(Text,Integer)]
type Error = Text

---------------------------------------------
-- READER / PARSER FOR AO DICTIONARY FILES --
---------------------------------------------

readDictFile :: Import -> Text -> DictFile
readDictFile src = readDictFileE src . splitEntries . dropBOM

dropBOM :: Text -> Text
dropBOM t = -- drop initial Byte Order Mark from text (if necessary)
    case T.uncons t of
        Just ('\xfeff', t') -> t'
        _ -> t 

splitEntries :: Text -> [Entry]
splitEntries t = 
    case T.uncons t of 
        Just ('@', t') -> T.empty : T.splitOn eSep t'
        _ -> T.splitOn eSep t
    where eSep = T.pack "\n@"

readDictFileE :: Import -> [Entry] -> DictFile
readDictFileE src (imps:defs) = DictF impL defL where
    impL = splitImports imps
    defL = map (readDefE src) (zip [1..] defs)
readDictFileE _ _ = error "error in splitEntries"

splitImports :: Entry -> [Import]
splitImports t =
    let tAfterWS = T.dropWhile isSpace t in
    if T.null tAfterWS then [] else
    let (imp,t') = T.break isSpace tAfterWS in
    imp : splitImports t'

readDefE :: Import -> (Integer,Entry) -> Def
readDefE srcI (eNum,eTxt) = Def word src def where
    src = Src srcI eNum
    (word,def) = pp (P.parse parseEntry "" eTxt)
    pp (Right (w,d)) = (w, Right d)
    pp (Left e) =
        -- parse error; estimate the word
        let wEst = T.takeWhile (not . isWordSep) eTxt in
        let eMsg = T.pack (show e) in
        (wEst, Left eMsg)

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
parseAction = parser P.<?> "word, text, number, block, amb, or inline ABC" where
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
    number = parseNumberUnits >>= return . uncurry Num
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

-- Numbers may have units in AO. A unit string is marked with '`'.
-- A number must be followed by a word separator. 
parseNumberUnits :: P.Stream s m Char => P.ParsecT s u m (Rational, Units)
parseNumberUnits = 
    parseNumber >>= \ r ->
    P.option [] (P.char '`' >> parseUnitString) >>= \ u ->
    expectWordSep >>
    return (r,u)

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

parseUnitString :: (P.Stream s m Char) => P.ParsecT s u m Units
parseUnitString = mbDivUnit P.<?> "unit string" where
    mbDivUnit =
        undivUnit >>= \ uNum ->
        P.option [] (P.char '/' >> undivUnit) >>= \ uDen ->
        let ul = (uNum ++ map (second negate) uDen) in
        return (canonicalUnits ul)
    undivUnit = (P.char '1' >> return []) P.<|> unitProd
    unitProd = P.sepBy1 oneUnit (P.char '*')
    oneUnit = 
        unitLabel >>= \ label ->
        P.option 1 (P.char '^' >> unitFactor) >>= \ factor ->
        return (label,factor)
    unitLabel = P.many1 (P.satisfy isUnitChar) >>= return . T.pack
    unitFactor = P.many1 (P.satisfy isDigit) >>= return . decToNum    

canonicalUnits, aggrUnits :: Units -> Units
canonicalUnits = aggrUnits . L.sortBy (compare `on` fst)
aggrUnits ((_,0):more) = aggrUnits more
aggrUnits ((u,r1):(u',r2):more) | (u == u') = aggrUnits ((u,r1+r2):more)
aggrUnits (l:more) = l : aggrUnits more
aggrUnits [] = []

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
-- start the same as a number, inline ABC, or a new `@word` entry.
isWordSep, isWordStart, isWordCont :: Char -> Bool
isWordSep c = isSpace c || block || amb where
    block = '[' == c || ']' == c
    amb = '(' == c || '|' == c || ')' == c
isWordCont c = not (isWordSep c || isControl c || '"' == c)
isWordStart c = isWordCont c && not (number || abc || entry) where
    number = isDigit c || '-' == c
    abc = '%' == c
    entry = '@' == c

-- tokens in AO are described with %{...}. They can have most
-- characters, except for {, }, and LF.
isTokenChar, isUnitChar, isInlineTextChar :: Char -> Bool
isTokenChar c = not ('{' == c || '\n' == c || '}' == c)
isInlineTextChar c = not ('"' == c || '\n' == c)
isUnitChar c = isWordStart c && not ('^' == c || '*' == c || '/' == c)


