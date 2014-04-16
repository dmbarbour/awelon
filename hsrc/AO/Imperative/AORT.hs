{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ViewPatterns #-}

-- | Utilities for imperative AO or ABC computations. 
--
-- Features shall include:
-- 
--   pure parallelism (by annotation)
--   concurrency (channels, publish-subscribe, shared state)
--   just-in-time and tracing compilation (mostly by annotation)
--   configurable powers and annotations (see 'newRuntimeContext')
--   runtime plugins support, preferably - extend powers via plugins
-- 
-- Access an AO or ABC runtime is achieved via `{tokens}` in byte code. 
-- Tokens cannot be forged, and can be embedded within a block to form
-- a `[{secure capability}]`. In AORT, the actual token text will be
-- generated only when it must be serialized (e.g. for distribution,
-- just-in-time compilation, or the 'aoi' display). Modulo annotations,
-- AORT strongly favors linear (single-use) tokens to simplify reasoning
-- about garbage collection and concurrency.
-- 
-- The main responsibility of AORT is to provide (via those tokens) safe,
-- efficient, and useful models for resources, concurrency, and behavior.
-- 
-- Safety includes ABC requirements for causal commutativity, spatial
-- idempotence, and progress. The conventional imperative process model
-- is an infinite wait-do loop. However, such a loop would violate all
-- of the safety properties. AORT instead models long-running behavior
-- via use of managed time and a multi-agent concurrency concept.
-- 
-- AORT's resource model is built around the RDP resource concept: all
-- resources are 'external' to the runtime, at least conceptually. The
-- AO code will observe and influence those resources. The difference
-- from RDP is that AORT is imperative, and thus models influence and
-- observation in terms of discrete reads and writes over time.
--
module AO.Imperative.AORT
    ( RtVal, readRtVal
    , rtProd, rtUnit, rtInL, rtInR, rtText, rtNum
    , expectProd, expectUnit, expectSum, expectText, expectNum

    , AORT, AORT_CX
    , readRT, liftRT
    , aort_ecx -- , aort_anno
    , newRuntimeContext
    , module ABC.Imperative.Value
    ) where

import Control.Applicative
import Control.Monad.IO.Class 
import Control.Monad.Trans.Reader

import Data.Typeable

import ABC.Imperative.Runtime
import ABC.Imperative.Value

-- | AORT is intended to be a primary runtime monad for executing
-- AO or ABC programs, at least for imperative modes of execution.
-- 
newtype AORT c a = AORT (ReaderT (AORT_CX c) IO a)
    deriving (Monad, MonadIO, Functor, Applicative, Typeable)

-- | read the runtime context
readRT :: (AORT_CX c -> a) -> AORT c a
readRT = AORT . asks

-- | perform effectful operations within the runtime.
liftRT :: (AORT_CX c -> IO a) -> AORT c a
liftRT = AORT . ReaderT

-- | AORT_CX mostly keeps information needed to manage concurrency,
-- side-effects, and resources. It can support some simple extensions
-- to the environment.
data AORT_CX c = AORT_CX
    { aort_ecx  :: !c -- extended context
    -- , aort_anno :: !(Powers c)
    }

-- | Build a new runtime context for AO and ABC executions.
-- This will build state for safe concurrent interaction, and
-- a resource context for useful caching.
newRuntimeContext :: (Typeable c) => c -> IO (AORT_CX c)
newRuntimeContext c = 
    return $ AORT_CX c 

-- I'll eventually need a lot of logic, here, to support powers,
-- annotations, and so on. 
instance Runtime (AORT c) where
    invoke = invokeFails -- for now 
      --  readRT (IORef.readIORef . aort_powers) >>=
      --  return . maybe (invokeFails s) id . ($ s)

-- | AORT uses the ABC.Imperative.Value type, but encapsulates uses
-- smart constructors with respect to blocks and some sealed values.
newtype RtVal c = RtVal { readRtVal :: V (AORT c) }

rtProd :: RtVal c -> RtVal c -> RtVal c
rtProd a b = RtVal (P (readRtVal a) (readRtVal b))

rtUnit :: RtVal c
rtUnit = RtVal U

rtInL, rtInR :: RtVal c -> RtVal c
rtInL = RtVal . L . readRtVal
rtInR = RtVal . R . readRtVal

rtText :: String -> RtVal c
rtText = RtVal . textToVal

rtNum :: Rational -> RtVal c
rtNum = RtVal . N 

expectProd :: RtVal c -> AORT c (RtVal c, RtVal c)
expectProd (RtVal (P a b)) = return (RtVal a, RtVal b)
expectProd (RtVal v) = fail $ "product expected; received " ++ show v

expectUnit :: RtVal c -> AORT c ()
expectUnit (RtVal U) = return ()
expectUnit (RtVal v) = fail $ "unit value expected; received " ++ show v

expectSum :: RtVal c -> AORT c (Either (RtVal c) (RtVal c))
expectSum (RtVal (L a)) = return (Left (RtVal a))
expectSum (RtVal (R b)) = return (Right (RtVal b))
expectSum (RtVal v) = fail $ "sum value expected; received " ++ show v

expectText :: RtVal c -> AORT c String
expectText (RtVal (valToText -> Just text)) = return text
expectText (RtVal v) = fail $ "text expected; received " ++ show v

expectNum :: RtVal c -> AORT c Rational
expectNum (RtVal (N r)) = return r
expectNum (RtVal v) = fail $ "number expected; received " ++ show v




    -- eventually it will do all those things...

-- NOTES:
--
--  I probably want annotations to be supplied by the client.
--
--  I'd also like the initial powerblock to be supplied by the
--  client. 

-- Powers are computed from text. Usually, this is a simple 'match
-- the string', but it might be useful to recognize path-based powers
-- or cases where the power is partially matched and the rest becomes
-- an argument. So, AORT provides freedom to experiment.
--
-- Note that these powers are associated with the ABC token text:
--
--     {token}
--
-- AO only exposes a small subset of tokens: annotations, sealers,
-- and unsealers. However, at runtime, more tokens may be generated
-- from a central powerblock. AORT requires a strong relationship
-- between tokens and powers because AORT may 'serialize' powers
-- to support just-in-time compilation.
--
-- That said, AORT also supports a concept of 'single-use' powers. 
--
-- 
--
-- The type of 'Prog' (from ABC.Imperative.Value) is:
--
--     AORT (V (AORT c)) ->  AORT (V (AORT c))
--
-- i.e. a function from one 'promised' value to another. 
--
-- In general, powers must always be kept accessible by string due
-- to the challenges surrounding 
-- type Powers = String -> Maybe (Prog (AORT c))

-- | obtain a powers object from a map.
-- powerMap :: M.Map String (Prog (AORT c)) -> Powers c
-- powerMap = flip M.lookup

-- | Build the AO standard environment, given the powerblock's
-- capability text. Note that the powerblock is responsible for
-- returning a new instance of itself when invoked, after which 
-- point the lookup step might be removed.
-- 
{-
aoStdEnv :: String -> V (AORT c)
aoStdEnv pbTok = (P s (P h (P pb (P (P sn ns) ex)))) where
    s = U -- empty stack
    h = U -- initial hand
    pb = B $ Block { b_code = pbCode, b_prog = pbProg
                   , b_aff = True, b_rel = True }
    pbCode = Tok pbTok
    pbProg = invoke pbTok
    sn = textToVal ""
    ns = U -- named stacks
    ex = U -- extended environment
-}

-- A simple runtime implementation for AORT.

-- DESIGN CONSIDERATIONS:
--
--  I would like to externally configure how 'debug' annotations will be
--  processed. 
--
--  Should I have any introspection of the dictionary?
--
--    Pros: could be convenient for metaprogramming
--    Cons: update of dictionary via AOI could be problematic wrgt. time
--
--  I think that I'll need a proper model of the dictionary as a
--  resource, which might change independently of program execution.
--    
