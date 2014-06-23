
-- | primary include file and utility functions for JIT plugins
--
-- Note: the utility functions provided here may change when the
-- JIT implementations change. This is rather ad-hoc at the moment,
-- since the JIT code itself is not yet part of this library. I'm
-- at a "please, just make it work" stage in the JIT development.
--
-- For the moment, I'll be using Haskell's laziness and 'error'
-- values instead of 'Maybe' types to encode computations off the
-- main path. This is a horrible hack, and I feel awful about it, 
-- but it's also very simple and can generate decent error info.
--
module ABC.Imperative.Prelude
    ( module ABC.Operators
    , module ABC.Imperative.Value
    , module ABC.Imperative.Resource
    , module ABC.Imperative.Runtime
    , module ABC.Imperative.Operations
    , (%), (>=>)
    , rtAssert

    , exProd, exSum3, sum3toV
    , exNum, exSeal, exBlock, exBKF
    , voidVal

    , divModR
    , bcomp
    , blockVal
    , quoteVal
    , condAp
    , mergeSum3
    ) where

-- import Prelude (negate,recip,(*),(+),(/=))
import Control.Monad
import Data.Ratio ((%))
import Data.Monoid
import ABC.Operators
import ABC.Quote
import ABC.Imperative.Value
import ABC.Imperative.Resource
import ABC.Imperative.Runtime
import ABC.Imperative.Operations
import qualified Data.Sequence as S

-- | compose two blocks
bcomp :: (Monad cx) => Block cx -> Block cx -> Block cx
bcomp = mappend

-- | create an initial block value (i.e. from JIT)
blockVal :: String -> (V cx -> cx (V cx)) -> Block cx
blockVal abc prog = Block False False (S.fromList (read abc)) prog

-- | create a block that adds a literal value to the environment
quoteVal :: (Monad cx) => V cx -> Block cx
quoteVal val = block where
    block = Block { b_aff = aff, b_rel = rel, b_code = code, b_prog = prog }
    aff = not (copyable val)
    rel = not (droppable val)
    code = S.fromList (quote val)
    prog = return . P val

-- | divMod for rationals
divModR :: Rational -> Rational -> (Rational,Rational)
divModR a b = let (nq,nr) = divModQ a b in (fromIntegral nq, nr)

rtAssert :: (Monad m) => String -> Bool -> m ()
rtAssert _ True = return ()
rtAssert msg False = fail $ "assertion failure: " ++ msg

-- | expecting a product
exProd :: (Monad cx) => V cx -> (V cx, V cx)
exProd (P a b) = (a,b)
exProd val | isVoid val = (voidVal, voidVal)
exProd val = error $ "product expected @ " ++ show val

-- | a representation for a void value
--
-- This is not a value that can normally be generated or processed
-- by AO. It is currently necessary because we can try to elaborate
-- dead branch values, especially if we inline some operations.
voidVal :: V cx
voidVal = (S "void" U)

isVoid :: V cx -> Bool
isVoid (S "void" U) = True
isVoid _ = False

-- | expecting a sum
-- 
-- in this case, we extract a sum into three values, one of which is
-- a void (indicating dead code). The boolean False|True corresponds
-- to the Left|Right positions.
--
-- It is possible that we'll extract other values from void.
exSum3 :: (Monad cx) => V cx -> (Bool, V cx, V cx)
exSum3 (L a) = (False, a, voidVal)
exSum3 (R b) = (True,  voidVal, b)
exSum3 val | isVoid val = (error "sum from void", voidVal, voidVal)
exSum3 val = error $ "sum expected @ " ++ show val

-- | obtain a sum from a pair of values and condition
sum3toV :: Bool -> V cx -> V cx -> V cx
sum3toV False a _ = (L a)
sum3toV True _ b  = (R b)

-- | merge from a sum3
mergeSum3 :: Bool -> V cx -> V cx -> V cx
mergeSum3 False a _ = a
mergeSum3 True  _ b = b

-- | conditional application; note that at this point
-- the boolean is directional, we'll apply the program
-- if the condition is true.
condAp :: (Monad cx) => Bool -> Prog cx -> Prog cx
condAp True p = p
condAp False _ = const (return voidVal)

-- | expect a number
exNum :: (Monad cx) => V cx -> Rational
exNum (N n) = n
exNum val = error $ "number expected @ " ++ show val

-- | expect a block
exBlock :: (Runtime cx) => V cx -> Block cx
exBlock (B b) = b
exBlock val = error $ "block expected @ " ++ show val

-- | extract a block together with relevance and affine attributes
exBKF :: (Runtime cx) => V cx -> (Block cx, Bool, Bool)
exBKF val = let b = exBlock val in (b, b_rel b, b_aff b)

-- | expect a sealed value, and unseal it
exSeal :: (Monad cx) => String -> V cx -> V cx
exSeal s (S s' val) | s == s' = val
exSeal s val = error $ "value sealed by {" ++ s ++ "} expected @ " ++ show val


