{-# LANGUAGE PatternGuards  #-}

-- | pure description of ABC operators
--   plus useful Show and Read instances
module ABC.Operators
    ( OpC(..), Op(..)
    , opCharList
    , abcQuoteNum
    ) where

import Control.Applicative ((<$>))
import Text.Read (Read(..))
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import Data.Ratio
import qualified Data.List as L

-- | ABC's single character operators
data OpC
    = Op_l | Op_r | Op_w | Op_z | Op_v | Op_c -- basic data plumbing
    | Op_L | Op_R | Op_W | Op_Z | Op_V | Op_C -- sum-type data plumbing
    | Op_copy | Op_drop -- '^' and '%'
    | Op_add | Op_neg | Op_mul | Op_inv | Op_divMod -- basic math
    | Op_ap | Op_cond | Op_quote | Op_rel | Op_aff | Op_comp -- higher order
    | Op_distrib | Op_factor | Op_merge | Op_assert -- working with sums
    | Op_gt -- value observations
    | Op_introNum -- '#'
    | Op_0 | Op_1 | Op_2 | Op_3 | Op_4 -- '0'-'4'
    | Op_5 | Op_6 | Op_7 | Op_8 | Op_9 -- '5'-'9'
    | Op_SP | Op_LF -- ' ' and '\n' (identity)
    deriving (Eq, Ord)

-- Op: pure ABC operations
data Op
    = OpC !OpC    -- single character op
    | BL  [Op]    -- block literals, e.g.  `[r[^'wol]wo^'wol]`
    | TL  String  -- text literal (with escapes removed)
    | Tok String  -- {foo}; invoke capability with given token
    deriving (Eq, Ord)
-- Note: ABC token strings may not contain '{', '}', or LF ('\n')
--   tokens are typically prefixed to indicate interpretation

opCharList :: [(OpC,Char)]
opCharList =
    [(Op_l,'l'),(Op_r,'r'),(Op_w,'w'),(Op_z,'z'),(Op_v,'v'),(Op_c,'c')
    ,(Op_L,'L'),(Op_R,'R'),(Op_W,'W'),(Op_Z,'Z'),(Op_V,'V'),(Op_C,'C')
    ,(Op_copy,'^'),(Op_drop,'%')
    ,(Op_add,'+'),(Op_neg,'-'),(Op_mul,'*'),(Op_inv,'/'),(Op_divMod,'Q')
    ,(Op_ap,'$'),(Op_cond,'?'),(Op_quote,'\''),(Op_rel,'k'),(Op_aff,'f'),(Op_comp,'o')
    ,(Op_distrib,'D'),(Op_factor,'F'),(Op_merge,'M'),(Op_assert,'K')
    ,(Op_introNum,'#')
    ,(Op_0,'0'),(Op_1,'1'),(Op_2,'2'),(Op_3,'3'),(Op_4,'4')
    ,(Op_5,'5'),(Op_6,'6'),(Op_7,'7'),(Op_8,'8'),(Op_9,'9')
    ,(Op_SP,' '),(Op_LF,'\n')
    ]

type QuoteOps = [Op] -> [Op]

qOp :: Op -> QuoteOps
qOp = (:)

qOpC :: OpC -> QuoteOps
qOpC = qOp . OpC

-- | translate a number into ABC's pseudo-literal syntax
abcQuoteNum :: Rational -> [Op] -> [Op]
abcQuoteNum r = quoteNum . quoteDen where
    quoteNum  = qi (numerator r)
    quoteDen  = if (1 == denominator r) then id else quoteDen'
    quoteDen' = qi (denominator r) . qOpC Op_inv . qOpC Op_mul

-- quote an integer into ABC, building from right to left
qi :: Integer -> QuoteOps
qi n | (n > 0) = let (q,r) = n `divMod` 10 in qi q . (qOpC (opd r))
     | (0 == n) = qOpC Op_introNum
     | otherwise = qi (negate n) . qOpC Op_neg

opd :: Integer -> OpC
opd 0 = Op_0
opd 1 = Op_1
opd 2 = Op_2
opd 3 = Op_3
opd 4 = Op_4
opd 5 = Op_5
opd 6 = Op_6
opd 7 = Op_7
opd 8 = Op_8
opd 9 = Op_9
opd _ = error "invalid digit"

instance Show OpC where 
    showsPrec _ opc = case L.lookup opc opCharList of
        Nothing -> error "opCharList is missing an operator"
        Just c -> (c:)
    showList = showConcatList
instance Show Op where
    showsPrec _ (OpC opc) = shows opc
    showsPrec _ (BL ops) =
        showChar '[' .
        showList ops .
        showChar ']'
    showsPrec _ (TL txt) =
        showChar '"' .
        showEscaped txt .
        showChar '\n' . showChar '~'
    showsPrec _ (Tok txt) = showChar '{' . showString txt . showChar '}'
    showList = showConcatList

-- show a list without the square brackets or commas
-- requires 'showsPrec' to be defined, for best performance
showConcatList :: (Show a) => [a] -> ShowS
showConcatList (x:xs) = shows x . showConcatList xs
showConcatList [] = id

-- escape text for showing a text literal
-- in ABC, only the '\n' characters need be escaped
-- does not include wrappers
showEscaped :: String -> ShowS
showEscaped ('\n':ss) = showChar '\n' . showChar ' ' . showEscaped ss
showEscaped (c:ss) = showChar c . showEscaped ss
showEscaped [] = id

instance Read OpC where
    readPrec = RP.lift readOpC
    readListPrec = RP.lift (R.many readOpC)
instance Read Op where
    readPrec = RP.lift readOp
    readListPrec = RP.lift (R.many readOp)

swap2 :: (a,b) -> (b,a)
swap2 (a,b) = (b,a)

readOpC :: R.ReadP OpC
readOpC =
    R.get >>= \ c ->
    case L.lookup c (fmap swap2 opCharList) of
        Nothing -> R.pfail
        Just opc -> return opc

readOp :: R.ReadP Op
readOp = 
    (BL <$> readBlock) R.<++
    (TL <$> readText) R.<++
    (Tok <$> readTok) R.<++
    (OpC <$> readOpC) 

readBlock :: R.ReadP [Op]
readBlock =
    R.char '[' >>
    R.many readOp >>= \ ops ->
    R.char ']' >>
    return ops

-- readText will remove the escapes (only LF is escaped)
readText :: R.ReadP String
readText = start where
    start = 
        R.char '"' >>
        textLine >>= \ t0 ->
        R.manyTill (R.char ' ' >> textLine) (R.char '~') >>= \ ts ->
        return $ L.concat (t0 : map ('\n':) ts)
    textLine = R.manyTill R.get (R.char '\n')

readTok :: R.ReadP String
readTok = R.char '{' >> R.manyTill (R.satisfy isTokChr) (R.char '}') 

isTokChr :: Char -> Bool
isTokChr c = not $ ('{' == c || '\n' == c || '}' == c)
