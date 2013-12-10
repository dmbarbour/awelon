{-# LANGUAGE FlexibleContexts #-}

-- | This module can read basic AO dictionary files and process
-- an AO dictionary in simple ways for bootstrapping of AO. 
--
-- See AboutAO.md for details on the dictionary file and AO
--
module AO
    ( parseDictFile
    ) where

import Control.Monad
import Control.Exception (assert)
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P
-- import Filesystem
import ABC

data DictF = DictF -- single dictionary file
    { df_imps :: [W]
    , df_defs :: [(W, [Action])]
    } 
type W = Text 
type Units = [(Text,Rational)]
data Action 
    = Word W                -- ref to dictionary
    | Number Rational Units -- literal number
    | Literal Text          -- literal text
    | Block [Action]        -- block of AO
    | Ambiguous [[Action]]  -- ambiguous choice
    | Primitive [Op]        -- inline ABC

------------------------------------
-- PARSER FOR AO DICTIONARY FILES
------------------------------------

parseDictFile :: (P.Stream s m Char) => P.ParsecT s u m DictF
parseDictFile = parser P.<?> "AO dictionary file" where
    parser = 
        parseImports >>= \ imps ->
        parseDefinitions >>= \ defs ->
        return (DictF { df_imps = imps, df_defs = defs })

parseImports :: (P.Stream s m Char) => P.ParsecT s u m [W]
parseImports = parser P.<?> "imports" where
    parser = P.optional spaces >> P.manyTill importWord endOfEntry
    importWord = parseWord >>= \ w -> spaces >> return w
    spaces = P.skipMany1 (P.satisfy isSpace)

parseDefinitions :: (P.Stream s m Char) => P.ParsecT s u m [(W,[Action])]
parseDefinitions = parser P.<?> "definitions" where
    parser = P.manyTill parseDef P.eof
    parseDef = 
        parseWord >>= \ w ->
        P.manyTill parseAction endOfEntry >>= \ actions ->
        return (w,actions)

-- Some of these may be redundant with Haskell's functions.
-- However, I want precise, local knowledge of what I mean.
-- 
--   AO acknowledges only two kinds of space - SP, LF
--   control characters include C0, C1, DEL
--   word separators include spaces, block [], amb (|)
--
-- Words may include higher unicode spaces (e.g. NBSP).
-- Tokens in %{token} may not contain {, }, or LF
--
isSpace, isDigit, isNZDigit, isHexDigit, isControl, 
    isWordSep, isWordStart, isWordCont, isTokenChar :: Char -> Bool

isSpace c = (' ' == c) || ('\n' == c)
isDigit c = ('0' <= c) && (c <= '9')
isNZDigit c = ('1' <= c) && (c <= '9')
isHexDigit c = isDigit c 
    || (('a' <= c) && (c <= 'f')) 
    || (('A' <= c) && (c <= 'F'))
isControl c = (n <= 0x1F) || ((0x7F <= n) && (n <= 0x9F)) 
    where n = fromEnum c
isWordSep c = isSpace c || ('[' == c) || (']' == c)
    || ('(' == c) || ('|' == c) || (')' == c)
isWordStart c = isWordCont c && 
    not (isDigit c || ('-' == c) || ('%' == c) || ('@' == c))
isWordCont c = not (isWordSep c || ('"' == c) || isControl c)
isTokenChar c = not (('{' == c) || ('}' == c) || ('\n' == c))

-- Words in AO are pretty flexible. Words are separated by SP, LF,
-- block [], or amb (|) characters. Words cannot start as a number
-- (digit or '-') nor as inline ABC ('%'), nor with '@' to simplify
-- unambiguous streaming of entries. Other than these, just a few 
-- restrictions apply: no C0 or C1 control characters, no DEL, 
-- no double quote.
--
parseWord :: (P.Stream s m Char) => P.ParsecT s u m W
parseWord = parser P.<?> "word" where
    parser =
        P.satisfy isWordStart >>= \ c1 ->
        P.manyTill wordCont wordEnd >>= \ cs ->
        return (T.pack (c1:cs))
    wordCont = P.satisfy isWordCont P.<?> "continuing word character"
    wordEnd = (P.lookAhead (P.satisfy isWordSep)) P.<?> "word separator"

-- An AO action is word, text, number, block, amb, or inline ABC.
-- Whitespace is preserved as inline ABC.
parseAction :: (P.Stream s m Char) => P.ParsecT s u m Action
parseAction = parser P.<?> "word, text, number, block, amb, or inline ABC" where
    parser = spaces P.<|> prim P.<|> text P.<|> 
             number P.<|> block P.<|> amb P.<|> word
    prim = P.char '%' >> (invocation P.<|> inlineABC)
    invocation =
        P.char '{' >>
        P.manyTill (P.satisfy isTokenChar) (P.char '}') >>= \ txt ->
        requireWordSep ("%{" ++ txt ++ "}") >>
        return (Primitive [Invoke (T.pack txt)])
    inlineABC = 
        P.many1 (P.oneOf inlineOpCodeList) >>= \ ops ->
        requireWordSep ("%" ++ ops) >>
        return (Primitive (map Op ops))
    word = parseWord >>= return . Word
    text = 
        (parseInlineText P.<|> parseMultiLineText) >>= \ txt ->
        requireWordSep "text literal" >>
        return (Literal txt)
    number = 
        parseNumberUnits >>= \ (r,u) -> 
        requireWordSep "number literal" >>
        return (Number r u)
    requireWordSep sMsg = P.lookAhead (P.satisfy isWordSep) 
        P.<?> ("word separator required after " ++ sMsg)
    block =
        P.char '[' >>
        P.manyTill parseAction (P.char ']') >>= \ blockActions ->
        return (Block blockActions)
    amb =
        P.char '(' >>
        P.sepBy1 (P.many parseAction) (P.char '|') >>= \ options ->
        P.char ')' >>
        return (Ambiguous options)
    spaces = -- spaces are preserved as inlined ABC
        P.many1 (P.satisfy isSpace) >>= return . Primitive . map Op


-- end of entry or end of input
endOfEntry :: (P.Stream s m Char) => P.ParsecT s u m ()
endOfEntry = entrySep P.<|> P.eof where
    entrySep = (atLineStart >> P.char '@' >> return ()) P.<?> "end of entry"

-- AO and the AO dictionary is sensitive to line starts.
-- Here, I leverage that Parsec tracks Column info. 
atLineStart, notAtLineStart :: (Monad m) => P.ParsecT s u m ()
atLineStart = P.getPosition >>= \ pos -> when (P.sourceColumn pos > 1) mzero
notAtLineStart = P.getPosition >>= \ pos -> unless (P.sourceColumn pos > 1) mzero 
        
parseMultiLineText, parseInlineText 
    :: (P.Stream s m Char) => P.ParsecT s u m Text

parseMultiLineText = parser P.<?> "multi-line text" where
    parser = 
        atLineStart >>
        P.char '"' >>
        lineOfText >>= \ firstLine ->
        P.manyTill contLine (P.char '~') >>= \ moreLines ->
        return (T.intercalate (T.singleton '\n') (firstLine : moreLines))
    contLine = (P.char ' ' >> lineOfText) P.<?> "continuing line of text"
    lineOfText = 
        P.manyTill (P.satisfy (/= '\n')) (P.char '\n') >>= 
        return . T.pack

parseInlineText = parser P.<?> "inline text" where
    parser = 
        notAtLineStart >>
        P.char '"' >>
        P.manyTill (P.satisfy tc) (P.char '"') >>= \ s ->
        return (T.pack s)
    tc c = not (c == '"' || c == '\n')

-- Numbers may have units in AO. A unit string is marked with '`'.
-- A number must be followed by a word separator. 
parseNumberUnits :: P.Stream s m Char => P.ParsecT s u m (Rational, Units)
parseNumberUnits = 
    parseNumber >>= \ n ->
    P.option [] (P.char '`' >> parseUnitString) >>= \ u ->
    return (n,u)

-- Numbers in AO are intended to be convenient for human users. The
-- cost is that there are many formats to parse. The following
-- formats are supported:
--   integral (e.g. 42)
--   decimal  (e.g. 12.3)
--   fractional (e.g. 2/3)
--   scientific (e.g. 3.4e5)
--   percentile (e.g. 98.7%)
--   hexadecimal (for natural numbers only)
-- These are similar to some extent. This parser attempts to 
-- reuse partial matches. 
parseNumber :: P.Stream s m Char => P.ParsecT s u m Rational
parseNumber = parser P.<?> "number" where
    parser = P.try parseDecimal P.<|> parseHexadecimal
    parseHexadecimal =
        P.char '0' >> P.char 'x' >> 
        P.many1 (P.satisfy isHexDigit) >>= \ ds ->
        return (fromIntegral (hexToNum ds))
    parseDecimal = 
        P.option False (P.char '-' >> return True) >>= \ bNeg ->
        parseUnsignedIntegral >>= \ n ->
        parseFragment n >>= \ r ->
        return (if bNeg then (negate r) else r)
    parseUnsignedIntegral = zero P.<|> positive
    zero = P.char '0' >> return 0
    positive = 
        P.satisfy isNZDigit >>= \ d ->
        P.many (P.satisfy isDigit) >>= \ ds ->
        return (decToNum (d:ds))
    parseFragment n =
        (P.char '/' >> fractional n) P.<|>
        (P.char '.' >> decimalDot n) P.<|>
        (postDecFragment (fromIntegral n))
    fractional num = positive >>= \ den -> return (num % den)
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
                else return (r * fromIntegral factor)

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
parseUnitString = error "todo: parseUnitString"



