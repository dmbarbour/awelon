
-- | Class for an imperative ABC runtime
module ABC.Imperative.Runtime 
    (Runtime(..)
    ,invokeFails
    ) where

import Control.Applicative
import ABC.Imperative.Value

-- | an ABC environment must handle capability tokens.
-- (Though, it is free to fail if a token is unrecognized.)
class (Monad cx, Applicative cx) => Runtime cx where
    invoke :: String -> Prog cx
    invoke = invokeFails

-- | suitable call for an unrecognized token
-- will pass unrecognized annotations `{&anno}`
-- otherwise will fail with appropriate message
invokeFails :: (Monad cx) => String -> Prog cx
invokeFails ('&':_) = id -- 
invokeFails tok = fail $ "{" ++ tok ++ "} token not recognized"
