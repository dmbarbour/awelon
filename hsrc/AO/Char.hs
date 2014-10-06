{-# LANGUAGE CPP #-}
-- just a few classifiers for characters
-- (I use this instead of Data.Char for stability and control of AO)
module AO.Char
    ( isWordSep, isWordStart, isWordCont
    , isTokenChar, isInlineTextChar
    , isSpace, isControl, isDigit, isNZDigit, isHexDigit
    ) where

import qualified Data.List as L

isWordSep :: Char -> Bool
isWordSep = flip L.elem " \n[](|)"

isWordStart, isWordCont :: Char -> Bool
isWordCont c = not (bl || ctl) where
    bl  = L.elem c " []{}\"(|)⦃⦄⦅⦆〚〛"
    ctl = isControl c
isWordStart c = not (d || bl) && isWordCont c where
    bl = ('%' == c) || ('@' == c)
    d = isDigit c

-- a token {foo} the token text 'foo' 
-- may not contain newline characters or curly braces. 
isTokenChar :: Char -> Bool
isTokenChar c = not (lf || cb) where
    lf = ('\n' == c)
    cb = ('{' == c) || ('}' == c)

-- inline text may not contain '"' or '\n'
isInlineTextChar :: Char -> Bool
isInlineTextChar c = not (lf || qu) where
    lf = ('\n' == c)
    qu = ('"' == c)

isSpace, isControl, isDigit, isNZDigit, isHexDigit :: Char -> Bool
isSpace c = (' ' == c) || ('\n' == c) -- spaces recognized by Awelon project
isControl c = isC0 || isC1orDEL where
    n = fromEnum c
    isC0 = n <= 0x1F
    isC1orDEL = n >= 0x7F && n <= 0x9F
isDigit c = ('0' <= c) && (c <= '9')
isNZDigit c = isDigit c && not ('0' == c)
isHexDigit c = isDigit c || smallAF || bigAF where
    smallAF = ('a' <= c) && (c <= 'f')
    bigAF = ('A' <= c) && (c <= 'F')

