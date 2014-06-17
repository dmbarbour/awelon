
-- | primary include file and utility functions for JIT plugins
--
-- Note: the utility functions provided here may change when the
-- JIT implementations change. This is rather ad-hoc at the moment,
-- since the JIT code itself is not yet part of this library. I'm
-- at a "just make it work" stage in the JIT development.
--
module ABC.Imperative.Prelude
    ( module ABC.Operators
    , module ABC.Imperative.Value
    , module ABC.Imperative.Resource
    , module ABC.Imperative.Runtime
    , module ABC.Imperative.Operations
    , (>=>), return

    , exProd, exSum3, sum3toV, condAp
    , exNum, exUnit, exBlock, exSeal
    ) where

import Control.Monad
import ABC.Operators
import ABC.Imperative.Value
import ABC.Imperative.Resource
import ABC.Imperative.Runtime
import ABC.Imperative.Operations
import qualified Data.Sequence as S


-- | expecting a product
exProd :: (Monad cx) => V cx -> cx (V cx, V cx)
exProd (P a b) = return (a,b)
exProd val | isVoid val = return (voidVal,voidVal)
exProd val = fail $ "product expected @ " ++ show val

-- | a representation for a void value
--
-- This is not a value that can normally be generated or processed
-- by AO. It is currently necessary because we can try to elaborate
-- dead branch values, especially if we inline some operations.
voidVal :: V cx 
voidVal = S "V" U

isVoid :: V cx -> Bool
isVoid (S "V" U) = True
isVoid _ = False

-- | expecting a sum
-- 
-- in this case, we extract a sum into three values, one of which is
-- a void (indicating dead code). The boolean False|True corresponds
-- to the Left|Right positions.
--
-- It is possible that we'll extract other values from void.
exSum3 :: (Monad cx) => V cx -> cx (Bool, V cx, V cx)
exSum3 (L a) = return (False, a, voidVal)
exSum3 (R b) = return (True,  voidVal, b)
exSum3 val | isVoid val = return (False, voidVal, voidVal)
exSum3 val = fail $ "sum expected @ " ++ show val

sum3toV :: Bool -> V cx -> V cx -> V cx
sum3toV False a _ = (L a)
sum3toV True _ b  = (R b)

-- | condAp is intended to work together with sum3. This applies a 
-- function if the given boolean is False, indicating Left branch.
condAp :: (Monad cx) => Bool -> Prog cx -> Prog cx
condAp False p = p
condAp True  _ = const (return voidVal) 

-- | expect a number
exNum :: (Monad cx) => V cx -> cx (Rational)
exNum (N n) = return n
exNum val | isVoid val = return (-1/12) -- arbitrary... 
exNum val = fail $ "number expected @ " ++ show val

-- | expect unit
exUnit :: (Monad cx) => V cx -> cx ()
exUnit U = return ()
exUnit val | isVoid val = return ()
exUnit val = fail $ "unit expected @ " ++ show val

-- | expect a block
exBlock :: (Runtime cx) => V cx -> cx (Block cx)
exBlock (B b) = return b
exBlock val | isVoid val = return deadBlock
exBlock val = fail $ "block expected @ " ++ show val

--
-- A dead block should never be executed, and only has a temporary
-- existence within a JIT in case of inlining some operations that 
-- expect a block. The token, in practice, will not escape unless 
-- there is a serious bug in the runtime or JIT.
deadBlock :: (Runtime cx) => Block cx
deadBlock = Block False False deadCode deadProg where
    deadCode = S.singleton (Tok deadTok)
    deadProg = invoke deadTok
    deadTok  = "~~this should be dead code~~"


-- | expect a sealed value, and unseal it
exSeal :: (Monad cx) => String -> V cx -> cx (V cx)
exSeal s (S s' val) | s == s' = return val
exSeal _ val | isVoid val = return voidVal
exSeal s val = fail $ "value sealed by {" ++ s ++ "} expected @ " ++ show val


