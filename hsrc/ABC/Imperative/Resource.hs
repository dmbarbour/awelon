{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}

-- | To support JIT via System.Plugins, it's useful to ensure 
-- a common type having a single symbol. Resource can serve 
-- this role.
--
-- Typically, this implementation resource will correspond to
-- a given ResourceToken (from ABC.Resource). Named resources 
-- are an effective basic unit for separate compilation, reuse,
-- and cache.
module ABC.Imperative.Resource 
    ( Resource(..), Prog, asProg
    ) where

import Data.Typeable
import ABC.Imperative.Runtime
import ABC.Imperative.Value

-- | intended for use with JIT via System.Plugins
newtype Resource = Resource (forall m . (Runtime m) => Prog m)
    deriving Typeable -- in case we want to use 'dynload' or similar

-- | quickly access the program held within a JIT'd resource
asProg :: (Runtime m) => Resource -> Prog m
asProg (Resource prog) = prog

