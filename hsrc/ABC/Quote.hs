-- | 'Quotable' is like 'Show' except it targets ABC code.
module ABC.Quote 
    ( Quotable(..), quote, QuoteS
    , concatQuotes 
    ) where

import Data.Ratio
import ABC.Operators

-- | 'Quotable' serves a role similar to 'show' except it 
-- targets ABC instead of text. Used for serializing to ABC.
--
-- To recover a value from ABC requires computing a structure then
-- translating it into the target type. (I.e. ABC is not parsed in
-- the conventional sense; it is executed.)
class Quotable v where quotes :: v -> QuoteS
type QuoteS = [Op] -> [Op]

-- | quote a quotable value into ABC
quote :: (Quotable v) => v -> [Op]
quote = flip quotes []

instance Quotable OpC where quotes = quotes . OpC
instance Quotable Op where quotes = (:)
instance Quotable Integer where quotes = qi'
instance (Integral i) => Quotable (Ratio i) where 
    quotes r | (r < 0) = quotes (negate r) . quotes Op_neg
             | (1 == denominator r) = qi (numerator r)
             | (1 == numerator r) = qi (denominator r) . quotes Op_inv
             | otherwise = qi (numerator r) . qi (denominator r) . 
                           quotes Op_inv . quotes Op_mul

qi :: (Integral i) => i -> QuoteS
qi = qi' . fromIntegral

qi' :: Integer -> QuoteS
qi' n | (n > 0) = let (q,r) = n `divMod` 10 in qi q . quotes (opd r)
      | (0 == n) = quotes Op_introNum
      | otherwise = qi (negate n) . quotes Op_neg

-- quote an integer into ABC, building from right to left
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
opd _ = error "invalid digit!"

concatQuotes :: (Quotable a) => [a] -> QuoteS
concatQuotes (x:xs) = quotes x . concatQuotes xs
concatQuotes [] = id
