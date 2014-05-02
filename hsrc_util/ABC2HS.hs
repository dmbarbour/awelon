{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- a mediocre, but not entirely naive, translation 
-- from Awelon bytecode into Haskell code.

module ABC2HS (abc2hs, abc2prog) where

import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.List as L
import ABC.Operators

type ModuleName = String 
type ErrorMsg = String
-- import Control.Applicative

abc2hs :: ModuleName -> [Op] -> Either ErrorMsg String
abc2hs modName ops = hsResource modName <$> abc2prog ops

hsResource :: ModuleName -> String -> String
hsResource modName innerProg = prefix innerProg where
    p = showChar '\n'
    prefix  = showMod.p.showImps.p.showRsc.p
    showMod = 
        showString "module " .
        showString modName .
        showString " ( resource ) where "
    showImps = 
        showString "import ABC.Operators\n\
                   \import ABC.Imperative.Value\n\
                   \import ABC.Imperative.Resource\n\
                   \import ABC.Imperative.Runtime\n\
                   \import Control.Monad ((>=>))
                   \"
    showRsc =
        showString "resource :: Resource\n\
                   \resource = Resource prog\n\
                   \"

type MkProg a = StateT ProgCX (ErrorT ErrorMsg Identity) a

runMkProg :: ProgCX -> MkProg a -> (Either ErrorMsg (a, ProgCX))
runMkProg cx op = runIdentity $ runErrorT $ runStateT op cx

-- Generate a set of toplevel Haskell names, with the main program
-- as 'prog' (of type (Runtime m) => V m -> m (V m)). 
--    (Runtime m) => V m -> m (V m)
-- This is the toplevel command to get started. 
abc2prog :: [Op] -> Either ErrorMsg String
abc2prog ops = Right "prog = error \"todo!\"\n"

-- create one big expression of type:
--   (Runtime m) => V m -> m (V m)
-- using 'let' expressions as desired.
--
--
data ProgCX = ProgCX
    { pcx_progs :: [([Op],ProgName)] -- to help detect cycles!
    }
type ProgName = String

pcx0 :: ProgCX
pcx0 = ProgCX 
    { pcx_progs = []
    }








-- let's try a "make it work, quick!" approach for now
-- we'll make it 'right' later. 
--
-- Consider the following desing 
--
--   (a) duplication of non-variable patterns requ
data Expr
    = Var String
    | Prod Expr Expr | Unit
    | Sum  Expr Expr | Void
    | Merged Expr Expr
    | Sealed String Expr




-- DESIGN:
--
-- We start by assuming the input is a simple variable "v0". 
--
--   to unify two patterns, possibly generating new variables
--   to create an intermediate variable (let..in)
--   to invoke an object...
--
-- 
--   
--  
--   to store an intermediate v0
--   
--
-- Conceptually, the initial input to the program is a *pattern*.
-- We build up this input pattern from step to step, while generating
-- an output pattern. The idea is to eventually generate code of the
-- form:
--
--   prog :: (Runtime m) => Prog m
--   prog (P (P (N a) (P (N b) c)) = return (P (N (a+b)) c)
--   prog v = error $ code ++ " @ " ++ show v
--
-- ... and do this while ensuring some minimal set of computations.
--
-- Potentially, we could also have pure functions of the form:
--
--   fn1 :: V m -> Either String (V m)
--   fn1 (P (P (N a) (P (N b) c)) = Right (P (N (a+b)) c)
--   fn1 v = Left (code ++ " @ " ++ show v)
--
-- Then use these for the pure parts, without involving the toplevel
-- monad before necessary.

     
indent,indent' :: String -> String -> String
indent ws ss = ws ++ indent' ws ss
indent' ws ('\n':ss) = '\n' : indent ws ss
indent' ws (c:ss) = c : indent' ws ss
indent' _ [] = ""
