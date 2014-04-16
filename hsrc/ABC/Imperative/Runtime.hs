{-# LANGUAGE PatternGuards #-}
-- | Class for an imperative ABC runtime
module ABC.Imperative.Runtime 
    ( Runtime(..)
    , invokeFails
    ) where

import Control.Applicative
import ABC.Imperative.Value

-- | an ABC environment must handle capability tokens.
-- (Though, it is free to fail if a token is unrecognized.)
class (Monad cx, Applicative cx) => Runtime cx where
    invoke :: String -> Prog cx
    invoke = invokeFails

instance Runtime IO

-- | invokeFails is a suitable call for unrecognized tokens
--
-- This will pass unrecognized annotations, and also handle the
-- discretionary seal and unseal actions from AO. Otherwise, it
-- will fail with a 'token not recognized' message.
invokeFails :: (Monad cx, Functor cx) => String -> Prog cx
invokeFails ('&':_) = id
invokeFails s@(':':_) = fmap (S s)
invokeFails ('.':s) = (=<<) (unseal s)
invokeFails tok = fail $ "{" ++ tok ++ "} token not recognized"

unseal :: (Monad cx) => String -> V cx -> cx (V cx)
unseal s (S (':':s') v) | (s == s') = return v
unseal s v = fail $ "{." ++ s ++ "} (unseal) @ " ++ show v

