
-- generic utilities
module Util
    ( try, tryJust, tryIO, tryJustIO
    , asyncIO
    , indent
    ) where

import Control.Applicative
import Control.Concurrent
--import Control.Concurrent.MVar
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Control.Exception as Err

asyncIO :: IO a -> IO a
asyncIO op = 
    newEmptyMVar >>= \ v ->
    forkIO (op >>= putMVar v) >>
    unsafeInterleaveIO (takeMVar v)

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




