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
    , valToText, textToVal
    , listToVal, valToList
    , simplEQ, assertEQ
    , isSum, isInL, isInR
    ) where

import Control.Applicative
import Control.Monad ((>=>))
import Data.Ord
import Data.Monoid
import Data.Ratio
import qualified Data.Sequence as S
import qualified Data.Foldable as S

import ABC.Operators
import ABC.Simplify
import ABC.Quote

-- | a value in imperative context cx
data V cx
    = N {-# UNPACK #-} !Rational -- numbers
    | P (V cx) (V cx) -- product
    | R (V cx) -- in right
    | L (V cx) -- in left
    | U -- unit
    | B (Block cx)  -- block
    | S String (V cx) -- sealed value
    deriving (Eq, Ord)

-- NOTE: Sealed values can also be used as a special class of token
-- for value encapsulation. Essentially, the sealer token can support
-- an 'unforgeable' lookup when used in an invocation. It could also
-- be used together with the value, of course.
--
-- Different uses of sealers may be distinguished by a prefix on the
-- sealer token. For example, discretionary sealers start with ':'.
-- This prefix is not stripped.

-- | an imperative program with context 'cx' 
-- 'cx' should be Monadic and Applicative
type Prog cx = (V cx) -> cx (V cx)

-- | A block is simply a finite sequence of ABC code. However, for
-- performance reasons, the block includes a 'b_prog' field that
-- should be equivalent to the code.
--
-- Unfortunately, this AO library does not enforce equivalence. A
-- client must be careful and disciplined when working with the 
-- b_prog field, otherwise the program may behave unexpectedly in
-- contexts such as JIT or distribution.
--
data Block cx = Block
    { b_aff  :: Bool     -- ^ is this block affine (no copy)?
    , b_rel  :: Bool     -- ^ is this block relevant (no drop)?
    , b_code :: S.Seq Op -- ^ raw operations, in sequence for cheap compose.
    , b_prog :: Prog cx  -- ^ be careful! must behave equivalently to b_code.
    } 

-- structural equality for blocks
-- implies behavioral equality (but not vice versa)
instance Eq (Block cx) where
    (==) b1 b2 = (b_code b1 == b_code b2)
              && (b_rel b1 == b_rel b2)
              && (b_aff b1 == b_aff b2)

-- structural comparison for blocks... 
-- not very semantic, but useful for memoization, maps, etc. 
instance Ord (Block cx) where
    compare b1 b2 = 
        comparing b_code b1 b2 `mappend`
        comparing b_rel  b1 b2 `mappend`
        comparing b_aff  b1 b2

instance (Monad cx) => Monoid (Block cx) where
    mempty = Block { b_aff = False, b_rel = False, b_code = S.empty, b_prog = return }
    mappend xy yz = xz where
        xz = Block { b_aff = aff', b_rel = rel', b_code = code', b_prog = prog' }
        aff' = b_aff xy || b_aff yz
        rel' = b_rel xy || b_rel yz
        code' = b_code xy S.>< b_code yz
        prog' = b_prog xy >=> b_prog yz

valToList :: (V cx -> Maybe a) -> V cx -> Maybe [a]
valToList f (L (P a as)) = (:) <$> f a <*> valToList f as
valToList _ (R U) = Just []
valToList _ _ = Nothing

listToVal :: (a -> V cx) -> [a] -> V cx
listToVal f (a:as) = (L (P (f a) (listToVal f as)))
listToVal _ [] = (R U)

valToText :: V cx -> Maybe String
valToText = valToList valToChar

textToVal :: String -> V cx
textToVal = listToVal (N . fromIntegral . fromEnum)

valToChar :: V cx -> Maybe Char
valToChar (N r) | (validChar r) = (Just . toEnum . fromInteger . numerator) r
valToChar _ = Nothing

validChar :: Rational -> Bool
validChar r =
    let n = numerator r in
    let d = denominator r in
    (1 == d) && (0 <= n) && (n <= 0x10ffff)

instance Show (V cx) where
    showsPrec _ (N r) = showNumber r
    showsPrec _ (L U) = showString "false"
    showsPrec _ (R U) = showString "true"
    showsPrec _ U = showString "unit"
    showsPrec _ (valToText -> Just txt) = shows (TL txt)
    showsPrec _ (P a b) = showChar '(' . shows a . showChar '*' . shows b . showChar ')'
    showsPrec _ (L a) = showChar '(' . shows a . showString "+_)"
    showsPrec _ (R b) = showString "(_+" . shows b . showChar ')'
    showsPrec _ b@(B _) = (shows . quote) b -- show as block literal + kf
    showsPrec _ (S seal v) = shows v . shows tok where
        tok = Tok (seal)

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
    quotes (B b) = code b . k . f where
        code = quotes . BL . S.toList . b_code 
        k = if (b_rel b) then quotes Op_rel else id
        f = if (b_aff b) then quotes Op_aff else id
    quotes (S s v) = quotes v . quotes sealer . quotes Op_ap where
        sealer = BL [Tok s]

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

-- default impl for `{&≡}` annotation
assertEQ :: (Monad cx) => Prog cx
assertEQ v@(P a (P a' _e)) | simplEQ a a' = return v
assertEQ v = fail $ "{&≡} (assertEQ) @ " ++ show v

-- block-simplifying equality test
simplEQ :: V a -> V b -> Bool
simplEQ (N a) (N b) = (a == b)
simplEQ (P a1 a2) (P b1 b2) = simplEQ a1 b1 && simplEQ a2 b2
simplEQ (L a) (L b) = (simplEQ a b)
simplEQ (R a) (R b) = (simplEQ a b)
simplEQ U U = True
simplEQ (B a) (B b) =
    -- compare simplified code
    let codeA = simplify (S.toList (b_code a)) in
    let codeB = simplify (S.toList (b_code b)) in
    (codeA == codeB) &&
    (b_aff a == b_aff b) &&
    (b_rel a == b_rel b)
simplEQ (S sa va) (S sb vb) = (sa == sb) && (simplEQ va vb)
simplEQ _ _ = False

-- to support code generation, I need a lot of utility functions.
isSum, isInL, isInR :: V a -> Bool
isSum (L _a) = True
isSum (R _b) = True
isSum _ = False
isInL (L _a) = True
isInL _ = False
isInR (R _b) = True
isInR _ = False

