{-# LANGUAGE FlexibleContexts #-} -- needed for Parsec

-- | Parser for AO code.
--
-- This is just the code parser. It does not parse full dictionary
-- files. Uses Parsec to ensure users have good error information,
-- and also to help track column (for inline vs. multi-line text).
--
module AO.Parser
    ( parseCode
    , parseWord
    , parseNumber
    ) where

import Control.Applicative
import Control.Monad
import Data.Ratio
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Parsec as P

import AO.Code
import ABC.Operators (opCharList)

-- test existence of a word separator without consuming it
expectWordSep :: (P.Stream s m Char) => P.ParsecT s u m ()
expectWordSep = (wordSep P.<|> P.eof) P.<?> "separator" where
    wordSep = P.lookAhead (P.satisfy isWordSep) >> return ()

-- most word separators are spaces, but [] and (|) are also okay
-- (this parser doesn't actually support ambiguous (foo|bar) code)
isWordSep :: Char -> Bool
isWordSep c = sp || block || amb where
    sp = (' ' == c) || ('\n' == c)
    block = ('[' == c) || (']' == c)
    amb = ('(' == c) || ('|' == c) || (')' == c)

-- | parse a regular AO word
-- 
-- To avoid confusion with numbers, words starting with '+', '-', or '.'
-- may not be immediately followed by a digit. 
--
-- To avoid confusion with entry separators in '.ao' files or streams,
-- words may not start with '@'.
--
-- In general, words cannot contain [ ] ( | ) " SP LF C0 or C1.
-- 
parseWord :: P.Stream s m Char => P.ParsecT s u m Word
parseWord = (P.try mathyWord) P.<|> normalWord where
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
    pmdReqMsg = "words starting with + - . may not follow with digit"
    isNPMD c = isWordStart c && not (isPMD c)
    normalWord =
        P.satisfy isNPMD >>= \ c1 ->
        P.many wc >>= \ cs ->
        expectWordSep >>
        return (T.pack (c1:cs))
    wc = P.satisfy isWordCont

isWordStart, isWordCont :: Char -> Bool
isWordCont c = not (sep || ctl || txt || tok) where
    sep = isWordSep c
    ctl = isControl c
    txt = '"' == c
    tok = '{' == c || '}' == c
isWordStart c = isWordCont c && not (isDigit c || '%' == c || '@' == c)

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
        P.manyTill (P.char ' ' >> lineOfText) (P.char '~') >>= \ ls ->
        expectWordSep >> return (L.intercalate "\n" (l0:ls))
    lineOfText = P.manyTill P.anyChar (P.char '\n')
    inlineText =
        atLineStart False >>
        P.char '"' >> P.manyTill ilchr (P.char '"') >>= \ s ->
        expectWordSep >> return s
    ilchr = P.satisfy isInlineTextChar

isInlineTextChar :: Char -> Bool
isInlineTextChar c = not ('"' == c || '\n' == c)

-- parse '{token}' of a known class
parseTokenText :: (P.Stream s m Char) => P.ParsecT s u m String
parseTokenText = 
    P.char '{' >> 
    P.manyTill (P.satisfy isTokenChar) (P.char '}') >>= \ tok ->
    let badTokMsg = "illegal token: {" ++ tok ++ "}" in
    unless (isValidToken tok) (P.unexpected badTokMsg) >>
    expectWordSep >> return tok

isTokenChar :: Char -> Bool
isTokenChar c = not (lf || cb) where
    lf = ('\n' == c)
    cb = ('{' == c) || ('}' == c)

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
    anyNumber = (P.try parseHexadecimal) P.<|> parseDecimal
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

parseInlineABC :: (P.Stream s m Char) => P.ParsecT s u m [OpC]
parseInlineABC = P.char '%' >> parser where
    parser = P.many1 abcOp >>= \ ops -> expectWordSep >> return ops
    abcOp = (P.satisfy isInlineABC >>= getOp) P.<?> inlineABCOps
    isInlineABC = flip L.elem inlineABCOps
    getOp c = case L.lookup c charOpList of
        Nothing -> P.unexpected (c : " is no longer a recognized ABC operator")
        Just op -> return op

inlineABCOps :: [Char]
inlineABCOps = "lrwzvcLRWZVC^%+-*/Q$?'okfDFMK>"
charOpList :: [(Char,OpC)]
charOpList = fmap swap2 opCharList where
    swap2 (a,b) = (b,a)

-- | AO code is almost a trivial sequence of actions. This function
-- expects to parse the entire input, and will preserve spaces by
-- encoding AO_ABC Op_SP or AO_ABC Op_LF.
parseCode :: (P.Stream s m Char) => P.ParsecT s u m [AO_Action]
parseCode = actionsToEOF P.<?> "AO code until end of input" where
    actionsToEOF = parseActions >>= \ ao -> P.eof >> return ao 

parseBlock :: (P.Stream s m Char) => P.ParsecT s u m [AO_Action]
parseBlock = P.char '[' >> parseActions >>= \ ao -> P.char ']' >> return ao

parseActions :: (P.Stream s m Char) => P.ParsecT s u m [AO_Action]
parseActions = L.concat <$> P.many actions where
    actions = abcActions P.<|> (pure<$>action)
    abcActions = fmap AO_ABC <$> (preserveSpaces P.<|> parseInlineABC)
    preserveSpaces = P.many1 space
    space = (P.char ' '  >> return Op_SP) P.<|> 
            (P.char '\n' >> return Op_LF)
    action = word P.<|> block P.<|> number P.<|> text P.<|> token
    word = AO_Word <$> parseWord
    block = AO_Block <$> parseBlock
    number = AO_Num <$> parseNumber
    text = AO_Text <$> parseText
    token = AO_Tok <$> parseTokenText

isControl, isDigit, isNZDigit, isHexDigit :: Char -> Bool
isControl c = isC0 || isC1orDEL where
    n = fromEnum c
    isC0 = n <= 0x1F
    isC1orDEL = n >= 0x7F && n <= 0x9F
isDigit c = ('0' <= c) && (c <= '9')
isNZDigit c = isDigit c && not ('0' == c)
isHexDigit c = isDigit c || smallAF || bigAF where
    smallAF = ('a' <= c) && (c <= 'f')
    bigAF = ('A' <= c) && (c <= 'F')


