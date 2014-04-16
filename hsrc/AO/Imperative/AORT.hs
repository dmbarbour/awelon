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
--   persistent state... AO is ideally persistent by default
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
    , loadRuntime
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
    , aort_home :: !String -- for persistence
    -- , aort_anno :: !(Powers c)
    }

-- | Load a persistent runtime context with the given identifier.
loadRuntime :: String -> IO (AORT_CX ())
loadRuntime home = 
    let rt = AORT_CX { aort_ecx = (), aort_home = home } in
    return rt

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

-- | REGARDING EFFECTS AND CAPABILITIES
--
-- AO was designed for reactive demand programming, where resources
-- are external to the process... and often persistent in nature.
-- Adapting AO to an imperative context is feasible, but requires
-- careful consideration of how to integrate side-effects.
--
-- One consideration is how tokens should interact with persistence.
--
-- I expect I'll need tokens to be divided between the volatile and 
-- the persistent. Volatile powers will be 'lost' after a restart or
-- other expiration event. Such restarts should be logically modeled,
-- and inherent in the 'static type' of the token and code.
--
-- Persistent tokens, OTOH, must contain or compose supplementary 
-- information to identify and recover target resources. This could
-- include plugin-based resources, persistent state, and more. It
-- might be acceptable for such powers to be directly provided by 
-- a client of AORT, assuming there is some stable identification
-- across resets.
--
-- If I can ensure that volatile tokens are never persisted, then it
-- is also feasible to use volatile tokens without failures. A good
-- question is whether to seek this assurance at the Haskell level or
-- at an AO layer (via assertions). 
--
-- Anyhow, my decision to encapsulate `RtVal` was to protect the
-- relationships between block code and the corresponding program,
-- and to support persistence and safety features.

