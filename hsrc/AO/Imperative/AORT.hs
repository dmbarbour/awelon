{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ViewPatterns #-}

-- | Utilities for imperative AO or ABC computations. 
--
-- Features:
-- 
--   pure parallelism (by annotation)
--   concurrency (channels, publish-subscribe, shared state)
--   just-in-time and tracing compilation (mostly by annotation)
--
-- Desiderata:
--
--   configurable powers and annotations
--   plugins support; extend features externally
--   persistent state; stable state resources
--
-- AntiFeatures: discipline is required to
--
--   protect progress, avoid infinite loops
--   protect causal commutativity, spatial idempotence 
--   avoid persisting blocks in stateful resources
--   ensure integrity of blocks for serialization or JIT
-- 
-- Access an AO or ABC runtime is achieved via `{tokens}` in byte code. 
-- Tokens cannot be forged, and can be embedded within a block to form
-- a `[{secure capability}]`. In AORT, token text will be generated only
-- when it must be serialized - e.g. for display, distribution, or just-
-- -in-time compilation.
--
-- Modulo annotations, AORT favors linear (single use) tokens to simplify
-- reasoning about GC and concurrency. The cost of linear tokens is that
-- 'undo' can become difficult to express. 
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
    ( AORT, readRT, liftRT, runRT
    , AORT_CX, newDefaultRuntime
    ) where

import Control.Applicative
import Control.Monad.IO.Class 
import Control.Monad.Trans.Reader
import Data.Typeable
import ABC.Imperative.Runtime

-- | AORT is intended to be a primary runtime monad for executing
-- AO or ABC programs, at least for imperative modes of execution.
newtype AORT c a = AORT (ReaderT (AORT_CX c) IO a)
    deriving (Monad, MonadIO, Functor, Applicative, Typeable)

-- | AORT_CX provides a global context for runtime operations.
--
-- At the moment, it's a place holder. But I expect I'll eventually
-- need considerable context to manage concurrency and resources.
data AORT_CX c = AORT_CX
    { aort_ecx  :: !c -- extended context for client-specific features
    -- , aort_anno :: 
    }

-- | run an arbitrary AORT program.
runRT :: AORT_CX c -> AORT c a -> IO a
runRT cx (AORT op) = runReaderT op cx

-- | read the runtime context
readRT :: (AORT_CX c -> a) -> AORT c a
readRT = AORT . asks

-- | perform effectful operations within the runtime.
liftRT :: (AORT_CX c -> IO a) -> AORT c a
liftRT = AORT . ReaderT

-- | a new runtime with default settings
newDefaultRuntime :: c -> IO (AORT_CX c)
newDefaultRuntime c = 
    let rt = AORT_CX { aort_ecx = c } in
    return rt

-- | create a new linear capability (a block with a one-use token).
--
-- Unlike a hand-crafted block, this one will be relatively safe for
-- JIT or serialization. 
--
-- newLinearCap :: Prog (AORT c) -> AORT c (Block (AORT c))


-- I'll eventually need a lot of logic, here, to support powers,
-- annotations, and so on. For now, I just want enough to get
-- started.
instance Runtime (AORT c) where
    invoke = invokeFails -- for now 
      --  readRT (IORef.readIORef . aort_powers) >>=
      --  return . maybe (invokeFails s) id . ($ s)

--
-- Thoughts: it may be worth adding yet another layer, an 'agent' concept,
-- that does not permit direct access to the active value. Instead, a dev
-- will send batches of ABC code at the agent, causing it to update and
-- indirectly manage other resources. The main issue here would be how an
-- agent advances through time when not being delivered any commands.
--
-- For now, such a feature isn't critical. I should just make the new
-- runtime work, first.


-- REGARDING PERSISTENCE AND PROCESSING
--
-- Persistence can become a challenge when we start throwing first
-- class functions around, or even blocks containing capabilities.
-- 
-- To avoid problems, we should simply avoid persisting blocks. If
-- persistent code is desired, we instead deliver a program that the
-- remote resource can process into ABC. Blocks may be communicated,
-- but must be 'volatile' in some sense - i.e. tied to the lifetime
-- of a connection or other resource.
--
-- This matches the same requirement in RDP, where blocks are always
-- considered revocable capabilities. I was thinking anyway that, to
-- ease transition, it would be wise for AORT to closely follow the 
-- resource model of RDP.
--
-- Anyhow, this means developers don't need to worry about 'persistent'
-- vs. 'volatile' blocks. Blocks and tokens - those we communicate - are 
-- always volatile, at least logically. (Caching might allow a generated
-- block to be kept persistently.) In general, only stateful resources 
-- may be persistent. 
--

