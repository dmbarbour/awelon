-- generic utilities
module Util
    ( try, tryJust, tryIO, tryJustIO
    , indent
    ) where

import Control.Applicative
import qualified Control.Exception as Err

tryIO :: IO a -> IO (Either Err.IOException a)
tryIO = Err.try

tryJustIO :: IO a -> IO (Maybe a)
tryJustIO op = either (const Nothing) (Just) <$> tryIO op

try :: IO a -> IO (Either Err.SomeException a)
try = Err.try -- type forced

tryJust :: IO a -> IO (Maybe a)
tryJust op = either (const Nothing) (Just) <$> try op


indent, indent' :: String -> String -> String
indent ws ss = ws ++ indent' ws ss 
indent' ws ('\n':ss) = '\n' : indent ws ss
indent' ws (c:ss) = c : indent' ws ss
indent' _ [] = []




