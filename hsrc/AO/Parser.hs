{-# LANGUAGE FlexibleContexts #-} -- needed for Parsec

-- | Parser for AO code.
--
-- This is just the code parser. It does not parse full dictionary
-- files. Uses Parsec to ensure users have good error information,
-- and also to help track column (for inline vs. multi-line text).
--
module AO.Parser
    ( parseAO
    , parseWord
    , parseNumber
    ) where

import Control.Applicative
import Control.Monad
import Data.Ratio
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Parsec as P

import AO.Code
import AO.Char

-- Test existence of a word separator without consuming it.
-- End of input is also acceptable as a word separator.
expectWordSep :: (P.Stream s m Char) => P.ParsecT s u m ()
expectWordSep = (wordSep P.<|> P.eof) P.<?> "word separator" where
    wordSep = P.lookAhead (P.satisfy isWordSep) >> return ()


-- | parse a regular AO word
-- 
-- Words may not contain [](|)" SP LF C0 or C1. Words may not start
-- with @. A word starting with + . or - must not be followed by a
-- digit.
-- 
parseWord :: P.Stream s m Char => P.ParsecT s u m Word
parseWord = ((P.try mathyWord) P.<|> normalWord) P.<?> "word" where
    -- words starting with plus, minus, or dot (PMD)
    -- cannot follow with a digit, to avoid confusion with numbers
    isPMD c = ('+' == c) || ('-' == c) || ('.' == c)
    mathyWord = 
        P.satisfy isPMD >>= \ c1 ->
        P.many wc >>= \ cs ->
        expectWordSep >> 
        if (startsWithDigit cs) then P.unexpected pmdReqMsg else
        return (T.pack (c1:cs))
    startsWithDigit [] = False
    startsWithDigit (c:_) = isDigit c
    pmdReqMsg = "digit; word starting with '+','-','.' could be confused with number"
    isNPMD c = isWordStart c && not (isPMD c)
    normalWord =
        P.satisfy isNPMD >>= \ c1 ->
        P.many wc >>= \ cs ->
        expectWordSep >>
        return (T.pack (c1:cs))
    wc = P.satisfy isWordCont

-- parse some text, either inline or multi-line
parseText :: (P.Stream s m Char) => P.ParsecT s u m String
parseText = multiLineText P.<|> inlineText where
    atLineStart bExpected = 
        P.getPosition >>= \ pos -> 
        let bAtLineStart = (1 == P.sourceColumn pos) in
        when (bExpected /= bAtLineStart) mzero
    multiLineText = 
        atLineStart True >>
        P.char '"' >> lineOfText >>= \ l0 ->
        P.manyTill (sp >> lineOfText) mlTerm >>= \ ls ->
        expectWordSep >> return (L.intercalate "\n" (l0:ls))
    lineOfText = P.manyTill P.anyChar lf
    sp = P.char ' ' P.<?> "SP to escape prior LF"
    lf = P.char '\n' P.<?> "LF for multi-line text"
    mlTerm = P.char '~' P.<?> "~ to end multi-line text"
    inlineText =
        atLineStart False >>
        P.char '"' >> P.manyTill ilchr (P.char '"') >>= \ s ->
        expectWordSep >> return s
    ilchr = (P.satisfy isInlineTextChar P.<?> "inline text characters")

-- parse '{token}' of a known class
parseTokenText :: (P.Stream s m Char) => P.ParsecT s u m String
parseTokenText = 
    P.char '{' >> 
    P.manyTill (P.satisfy isTokenChar) (P.char '}') >>= \ tok ->
    let badTokMsg = "unrecognized token: {" ++ tok ++ "}" in
    unless (isValidToken tok) (P.unexpected badTokMsg) >>
    expectWordSep >> return tok

isValidToken :: String -> Bool
isValidToken ('&':_) = True -- annotation
isValidToken (':':_) = True -- sealer
isValidToken ('.':_) = True -- unsealer
isValidToken _ = False


-- | Numbers in AO are intended to be convenient for human users. The
-- cost is that there are many formats to parse. The following
-- formats are supported:
--   integral (e.g. 42)
--   decimal  (e.g. 12.3)
--   fractional (e.g. 2\/3)
--   scientific (e.g. 3.4e5)
--   percentile (e.g. 98.7%)
--   hexadecimal (e.g. 0x221E; natural numbers only)
-- These are similar to some extent. This parser attempts to 
-- reuse partial matches. All numbers are converted to exact
-- rationals in the end.
--
-- note: numbers must be parsed AFTER words (to address the mathyWords)
parseNumber :: (P.Stream s m Char) => P.ParsecT s u m Rational
parseNumber = parser P.<?> "number" where
    parser = anyNumber >>= \ n -> expectWordSep >> return n
    anyNumber = parseHexadecimal P.<|> parseDecimal
    parseHexadecimal =
        P.try (P.char '0' >> P.char 'x') >> 
        P.many1 parseHexDigit >>=
        return . fromIntegral . hexToNum
    parseHexDigit = P.satisfy isHexDigit P.<?> "hexadecimal digit"
    parseDecimal = 
        P.option False (P.char '-' >> return True) >>= \ bNeg ->
        parseUnsignedIntegral >>= \ n ->
        parseFragment n >>= \ r ->
        when ((0 == r) && bNeg) rejectNegZero >>
        let result = if bNeg then (negate r) else r in
        return result
    rejectNegZero = P.unexpected "negative zero"
    parseDecimalDigit = P.satisfy isDigit P.<?> "decimal digit"
    parseUnsignedIntegral = (zeroInt P.<|> posInt) P.<?> "digits"
    zeroInt = P.char '0' >> return 0
    posInt = 
        P.satisfy isNZDigit >>= \ c1 ->
        P.many parseDecimalDigit >>= \ cs ->
        return (decToNum (c1:cs))
    parseFragment n =
        (P.char '/' >> fractional n) P.<|>
        (P.char '.' >> decimalDot n) P.<|>
        (postDecFragment (fromIntegral n))
    fractional num = posInt >>= \ den -> return (num % den)
    decimalDot n = 
        P.many1 parseDecimalDigit >>= \ ds ->
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
        when ((0 == n) && bNeg) rejectNegZero >>
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

parseAOp :: (P.Stream s m Char) => P.ParsecT s u m AOp
parseAOp = asAOp <$> P.oneOf inlineABCList where
    inlineABCList = fmap snd aopCharList
    charAOpList = fmap (\(a,b)->(b,a)) aopCharList
    asAOp = fromJust . flip L.lookup charAOpList

parseInlineABC :: (P.Stream s m Char) => P.ParsecT s u m [AOp]
parseInlineABC = 
    P.char '%' >> P.many1 parseAOp >>= \ ops ->
    expectWordSep >> return ops

-- | AO code is almost a trivial sequence of actions. This function
-- expects to parse the entire input, and will preserve spaces by
-- encoding AO_ABC Op_SP or AO_ABC Op_LF.
parseAO :: (P.Stream s m Char) => P.ParsecT s u m [AO_Action]
parseAO = actionsToEOF P.<?> "AO code until end of input" where
    actionsToEOF = parseActions >>= \ ao -> P.eof >> return ao 

parseBlock :: (P.Stream s m Char) => P.ParsecT s u m [AO_Action]
parseBlock = P.char '[' >> parseActions >>= \ ao -> P.char ']' >> return ao

parseActions :: (P.Stream s m Char) => P.ParsecT s u m [AO_Action]
parseActions = L.concat <$> P.many actions where
    actions = spaces P.<|> abc P.<|> (pure<$>action) 
    spaces = (P.many1 (P.satisfy isSpace) >> return []) P.<?> "spaces"
    abc = fmap AO_ABC <$> parseInlineABC
    action = word P.<|> block P.<|> text P.<|> number P.<|> token
    word = AO_Word <$> parseWord
    block = AO_Block <$> parseBlock
    number = AO_Num <$> parseNumber
    text = AO_Text <$> parseText
    token = AO_Tok <$> parseTokenText
