{-# LANGUAGE PatternGuards  #-}

-- | pure description of ABC operators
--   plus useful Show and Read instances
module ABC.Operators
    ( OpC(..), Op(..)
    , opCharList
    , abcQuoteNum
    ) where

--import Text.Show
-- import qualified Text.Read
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
    | Op_N0 -- '#'
    | Op_d0 | Op_d1 | Op_d2 | Op_d3 | Op_d4 -- '0'-'4'
    | Op_d5 | Op_d6 | Op_d7 | Op_d8 | Op_d9 -- '5'-'9'
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
    ,(Op_N0,'#')
    ,(Op_d0,'0'),(Op_d1,'1'),(Op_d2,'2'),(Op_d3,'3'),(Op_d4,'4')
    ,(Op_d5,'5'),(Op_d6,'6'),(Op_d7,'7'),(Op_d8,'8'),(Op_d9,'9')
    ,(Op_SP,' '),(Op_LF,'\n')
    ]

-- | translate a number into ABC's pseudo-literal syntax
abcQuoteNum :: Rational -> [Op]
abcQuoteNum r =
    let denOps = 
            if (1 == denominator r) then [] else
            qi (denominator r) [OpC Op_inv, OpC Op_mul]
    in qi (numerator r) denOps

-- quote an integer into ABC, building from right to left
qi :: Integer -> [Op] -> [Op]
qi n ops | (n > 0) =
    let (q,r) = n `divMod` 10 in
    qi q (OpC (opd r) : ops)
qi n ops | (0 == n) = (OpC Op_N0 : ops)
qi n ops = qi (negate n) (OpC Op_neg : ops)

opd :: Integer -> OpC
opd 0 = Op_d0
opd 1 = Op_d1
opd 2 = Op_d2
opd 3 = Op_d3
opd 4 = Op_d4
opd 5 = Op_d5
opd 6 = Op_d6
opd 7 = Op_d7
opd 8 = Op_d8
opd 9 = Op_d9
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
