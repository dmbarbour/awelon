{-# LANGUAGE PatternGuards #-}
-- | Class for an imperative ABC runtime
module ABC.Imperative.Runtime 
    ( Runtime(..)
    , invokeDefault
    ) where

import Control.Applicative
import ABC.Imperative.Value

-- | an ABC environment must handle capability tokens.
-- (Though, it is free to fail if a token is unrecognized.)
--   todo: consider switch to `String -> cx (Prog cx)` for staging.
class (Monad cx, Applicative cx) => Runtime cx where
    invoke :: String -> Prog cx
    invoke = invokeDefault

instance Runtime IO

-- | invokeFails is a suitable call for unrecognized tokens
--
-- This will pass unrecognized annotations, handle some standard
-- annotations, and also handle discretionary seal and unseal 
-- actions. Otherwise, it will fail with appropriate message.
invokeDefault :: (Monad cx) => String -> Prog cx
invokeDefault ('&':anno) = invokeAnno anno
invokeDefault s@(':':_) = return . (S s)
invokeDefault ('.':s) = unseal s
invokeDefault tok = fail . emsg where
    emsg v = "{" ++ tok ++ "} token not recognized @ " ++ show v

unseal :: (Monad cx) => String -> V cx -> cx (V cx)
unseal s (S (':':s') v) | (s == s') = return v
unseal s v = fail $ "{." ++ s ++ "} (unseal) @ " ++ show v

-- default annotations... only one right now
invokeAnno :: (Monad cx) => String -> Prog cx
invokeAnno ('≡':[]) = assertEQ
invokeAnno _ = return
