{-# LANGUAGE ViewPatterns #-}

-- | a dynamic value type for imperative or functional interpretation
-- of ABC (but not so great for reactive). By 'dynamic' I mean that
-- values here are unityped (just 'V') and operations handle cases.
--
-- I've had difficulty expressing ABC's ad-hoc structural types
-- within Haskell's type system, so it is likely we'll be using
-- dynamic values indefinitely (until we bootstrap, anyway).
module ABC.Imperative.Value
    ( V(..), Prog, Block(..)
    , copyable, droppable
    , divModQ
    , valToText, textToVal
    ) where

import Control.Applicative
import Data.Monoid
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T

import ABC.Operators
import ABC.Quote

-- | a value in imperative context cx
data V cx
    = N {-# UNPACK #-} !Rational -- numbers
    | P (V cx) (V cx) -- product
    | R (V cx) -- in right
    | L (V cx) -- in left
    | U -- unit
    | B (Block cx)  -- block
    | S !Text (V cx) -- sealed value

-- | an imperative program with context 'cx' 
-- 'cx' should be Monadic and Applicative
type Prog cx = cx (V cx) -> cx (V cx)

data Block cx = Block
    { b_aff :: Bool
    , b_rel :: Bool
    , b_code :: [Op]
    , b_prog :: Prog cx
    }

instance Monoid (Block cx) where
    mempty = Block { b_aff = False, b_rel = False, b_code = [], b_prog = id }
    mappend xy yz = xz where
        xz = Block { b_aff = aff', b_rel = rel', b_code = code', b_prog = prog' }
        aff' = b_aff xy || b_aff yz
        rel' = b_rel xy || b_rel yz
        code' = b_code xy ++ b_code yz
        prog' = b_prog yz . b_prog xy

-- | divModQ :: dividend -> divisor -> (quotient, remainder)
divModQ :: Rational -> Rational -> (Integer, Rational)
divModQ x y =
    let n = numerator x * denominator y in
    let d = denominator x * numerator y in
    let dr = denominator x * denominator y in
    let (q,r) = n `divMod` d in
    (q, r % dr)

valToText :: V cx -> Maybe String
valToText (R (P c l)) = (:) <$> valToChar c <*> valToText l 
valToText (L U) = Just []
valToText _ = Nothing

valToChar :: V cx -> Maybe Char
valToChar (N r) | (validChar r) = (Just . toEnum . fromInteger . numerator) r
valToChar _ = Nothing

validChar :: Rational -> Bool
validChar r =
    let n = numerator r in
    let d = denominator r in
    (1 == d) && (0 <= n) && (n <= 0x10ffff)

textToVal :: String -> V cx
textToVal [] = L U
textToVal (c:cs) = R (P cv (textToVal cs)) where
    cv = (N . fromIntegral . fromEnum) c

instance Show (V cx) where
    showsPrec _ (N r) = showNumber r
    showsPrec _ (L U) = showString "false"
    showsPrec _ (R U) = showString "true"
    showsPrec _ U = showString "unit"
    showsPrec _ (valToText -> Just txt) = shows (TL txt)
    showsPrec _ (P a b) = showChar '(' . shows a . showChar '*' . shows b . showChar ')'
    showsPrec _ (L a) = showChar '(' . shows a . showString "+_)"
    showsPrec _ (R b) = showString "(_+" . shows b . showChar ')'
    showsPrec _ (B block) = shows (BL (b_code block))
    showsPrec _ (S sealer v) = shows v . shows tok where
        tok = Tok (':' : T.unpack sealer)

showNumber :: Rational -> ShowS
showNumber r | (1 == denominator r) = shows (numerator r)
showNumber r = shows (numerator r) . showChar '/' . shows (denominator r)

instance Quotable (V cx) where
    quotes (valToText -> Just txt) = quotes (TL txt)
    quotes (N r) = quotes r
    quotes (P a b) = quotes a . quotes b . quotes Op_w . quotes Op_l
    quotes (R b) = quotes b . qinR where
        qinR = concatQuotes [Op_V, Op_V, Op_R, Op_W, Op_L, Op_C]
    quotes (L a) = quotes a . qinL where
        qinL = quotes Op_V
    quotes U = quotes Op_v . qswap where
        qswap = concatQuotes [Op_v, Op_w, Op_r, Op_l, Op_c]
    quotes (B b) = code . k . f where
        code = quotes (BL (b_code b))
        k = if (b_rel b) then quotes Op_rel else id
        f = if (b_aff b) then quotes Op_aff else id
    quotes (S s v) = quotes v . sealer . quotes Op_ap where
        sealer = quotes (BL [invoke])
        invoke = Tok (':' : T.unpack s) 

copyable :: V cx -> Bool
copyable (N _) = True
copyable (P a b) = copyable a && copyable b
copyable (R b) = copyable b
copyable (L a) = copyable a
copyable U = True
copyable (B b) = not (b_aff b)
copyable (S _ v) = copyable v

droppable :: V cx -> Bool
droppable (N _) = True
droppable (P a b) = droppable a && droppable b
droppable (R b) = droppable b
droppable (L a) = droppable a
droppable U = True
droppable (B b) = not (b_rel b)
droppable (S _ v) = droppable v

