{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Some utilities for executing AO code. 
module AORT
    ( module AO.Code
    , module AO.Dict
    , module ABC.Operators
    , AORT, AORT_CX
    , aort_ecx, aort_powers
    , newRuntimeContext
    ) where

import Control.Applicative
import Control.Monad.IO.Class 
--import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import qualified Data.Map as M

import AO.Code
import AO.Dict
import ABC.Operators
import ABC.Imperative.Runtime
import ABC.Imperative.Value

-- | AORT is intended to be the primary runtime monad for executing
-- AO or ABC programs, at least for the imperative modes of execution.
newtype AORT cx a = AORT (ReaderT (AORT_CX cx) IO a)
    deriving (Monad, MonadIO, Functor, Applicative)
-- The primary design constraints on AORT arise from its concurrency
-- model: use of a State monad would hinder transparent asynchronous 
-- computations AND we need a shared concept of time across multiple
-- resources. So, communication primarily occurs via IO, but using a
-- runtime context (rather than global Haskell state).

-- | AORT_CX mostly keeps information needed to manage concurrency
-- and resources. It may be stateful, but all state manipulations 
-- must be performed via IO.
--
-- Developers can extend the runtime a little, and add powers or
-- annotations that access this context. There may be constraints
-- on the ecx (typeclass requirements) to install certain powers.
--
data AORT_CX ecx = AORT_CX
    { aort_ecx :: !ecx -- extended context
    , aort_powers :: !(Powers ecx)
    }

type RtV ecx = V (AORT ecx)           -- a runtime value
type PRtV ecx = AORT ecx (RtV ecx)    -- a promised runtime value
type Power ecx = PRtV ecx -> PRtV ecx -- a function over promised values
type Powers ecx = M.Map String (Power ecx)


-- | Build a new runtime context for AO and ABC executions.
-- This will build state for safe concurrent interaction, and
-- a resource context for useful caching.
newRuntimeContext :: ecx -> Powers ecx -> IO (AORT_CX ecx)
newRuntimeContext ecx power = return cx where
    cx = AORT_CX ecx power
    -- err... we'll do all that eventually


-- DESIGN CONSIDERATIONS:
--
--  I really need two layers to support AOI.
--
--    one layer to handle history, undo, completions, dictionary updates
--    a second layer to address asynchronous behavior, etc.
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
