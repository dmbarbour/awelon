{-# LANGUAGE PatternGuards  #-}

-- | pure description of ABC operators
module ABC.Operators
    ( OpC(..), Op(..)
    , opCharList, showOps
    , abcQuoteNum, abcEscapeText
    ) where

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

-- Op: pure ABC operations
data Op
    = OpC !OpC   -- single character op
    | BL  [Op]    -- block literals, e.g.  `[r[^'wol]wo^'wol]`
    | TL  String  -- text literal (with escapes removed)
    | Tok String -- {foo}; invoke capability with given token
    deriving (Eq, Ord)
-- Note: ABC token strings may not contain '{', '}', or LF ('\n')
--   tokens are typically prefixed to indicate interpretation

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

instance Show OpC where show = (:[]) . showOpC
instance Show Op where show = showOp

showOps :: [Op] -> String
showOps = L.concatMap showOp

showOp :: Op -> String
showOp (OpC opc) = [showOpC opc]
showOp (BL ops) = "[" ++ showOps ops ++ "]"
showOp (TL txt) = abcEscapeText txt
showOp (Tok txt) = "{" ++ txt ++ "}" 

showOpC :: OpC -> Char
showOpC op = 
    case L.lookup op opCharList of
        Nothing -> error "opCharList is missing an operator"
        Just c -> c

-- | Escape text for use as an ABC literal
abcEscapeText :: String -> String
abcEscapeText = wrap . L.intercalate "\n " . lines1 where
    wrap s = "\"" ++ s ++ "\n~"

-- lines1 guarantees at least one line and returns a blank line if
-- (unlike Data.List.lines, which returns no lines for empty string)
lines1 :: String -> [String]
lines1 = l1 where
    l1 s = 
        let (hd,more) = L.span (/= '\n') s in
        hd : l2 more
    l2 ('\n':s) = l1 s
    l2 [] = []
    l2 _ = error "illegal state in lines1"
