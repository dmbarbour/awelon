{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ViewPatterns, CPP #-}

-- | A runtime monad for the 'ao' and 'aoi' executables. At the top
-- level, 'ao' and 'aoi' handle streaming ABC operations without any
-- direct effects.
--
-- Long-term behavior will tentatively be modeled by installation of
-- RDP behaviors to manage resources, as per Awelon project.
--
-- Desiderata at this layer:
--
-- * support for 'undo' in the aoi REPL
-- * support for persistent sessions (maybe)
--
-- The primary goal is performance. Performance has been an obstacle
-- to development of applications in AO. If JIT and other features can
-- eliminate this obstacle, AO will be better situated for success.
-- 
module AORT
    ( AORT, AORT_CX, readRT, liftRT, runRT, liftIO

    -- eventually, I might support some configuration
    --  but isn't needed while 'ao' and 'aoi' are only
    --  clients and one goal is equivalent behavior
    , newDefaultRuntime
    , newDefaultEnvironment
    , newPowerBlock, aoStdEnv
    ) where

import Control.Applicative
import Control.Monad.IO.Class 
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Typeable

-- import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.IO as Sys
import qualified System.Entropy as Entropy
import qualified System.Environment as Env
import qualified System.IO.Error as Err
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS

import Data.Ratio
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.ByteString.Base64.URL as B64

import ABC.Operators
import ABC.Imperative.Value
import ABC.Imperative.Runtime

import PureM -- speeds up 'pure' blocks or subprograms (but might remove after JIT)

-- | AORT is intended to be a primary runtime monad for executing
-- AO or ABC programs, at least for imperative modes of execution.
newtype AORT a = AORT (PureM (ReaderT AORT_CX IO) a)
    deriving (Monad, MonadIO, Functor, Applicative, Typeable)

-- | AORT_CX is the runtime context, global to each instance of the
-- AORT runtime. The runtime context supports:
--
--   * token management across JIT or serialization
--   * coordination of shared resources
--   * configuration of annotation behaviors
--
-- At the moment, this is a place holder. But I expect I'll eventually
-- need considerable context to manage concurrency and resources.
data AORT_CX = AORT_CX
    { aort_anno   :: String -> Maybe (Prog AORT)
    , aort_power  :: String -> Maybe (Prog AORT)
    , aort_secret :: String -- a random string
    } 

-- | run an arbitrary AORT program.
runRT :: AORT_CX -> AORT a -> IO a
runRT cx (AORT op) = runReaderT (runPureM op) cx

-- | read the runtime context
readRT :: (AORT_CX -> a) -> AORT a
readRT = AORT . lift . asks

-- | perform effectful operations within the runtime.
liftRT :: (AORT_CX -> IO a) -> AORT a
liftRT = AORT . lift . ReaderT

bytesToB64 :: ByteString -> String
bytesToB64 = B.unpack . B64.encode

-- | a new runtime with default settings
newDefaultRuntime :: IO AORT_CX
newDefaultRuntime = 
    bytesToB64 <$> Entropy.getEntropy 12 >>= \ secret ->
    let cx = AORT_CX { aort_anno   = defaultAnno
                     , aort_power  = defaultPower
                     , aort_secret = secret
                     }
    in return cx

-- default annotations support for AORT
--
-- To add: 
--
-- * JIT (immediate, lazy, parallel, tracing)
-- * asynchronous evaluation
-- * memoization? (with exponential decay?)
-- 
defaultAnno :: String -> Maybe (Prog AORT)
defaultAnno = flip M.lookup $ M.fromList $
    [("debug print raw", mkAnno debugPrintRaw)
    ,("debug print text", mkAnno debugPrintText)
    ]

mkAnno :: (V AORT -> AORT ()) -> Prog AORT
mkAnno fn v = fn v >> return v

debugPrintRaw, debugPrintText :: V AORT -> AORT ()
debugPrintRaw v =  liftIO $ Sys.hPutStrLn Sys.stderr (show v)
debugPrintText (valToText -> Just txt) = liftIO $ Sys.hPutStr Sys.stderr txt
debugPrintText v = fail $ "{&debug print text} @ " ++ show v

-- | an AO 'environment' is simply the first value passed to the
-- program. The AO standard environment has the form:
--
--    (stack * (hand * (power * ((stackName * namedStacks) * ext)
--
-- which provides some useful scratch spaces for a running program. 
--
-- The normal use case is that this environment is initially empty
-- except for a powerblock, and inputs are primarily supplied by 
-- side-effects. However, a few initial arguments might be placed on
-- the stack in some non-standard use cases.
--
aoStdEnv :: Block cx -> V cx
aoStdEnv pb = (P U (P U (P (B pb) (P (P sn U) U))))
    where sn = textToVal "" -- L U

-- | create a standard environment with a default powerblock
newDefaultEnvironment :: AORT (V AORT)
newDefaultEnvironment = aoStdEnv <$> newPowerBlock

-- | obtain a block representing access to default AORT powers.
newPowerBlock :: AORT (Block AORT) 
newPowerBlock = 
    readRT aort_secret >>= \ s ->
    let pbTok = '!':s in
    return $  Block { b_code = S.singleton (Tok pbTok)
                    , b_prog = invoke pbTok -- keeping it simple
                    , b_aff = True, b_rel = True }


-- Default powers use the command string, rather than the token.
-- Toplevel powers are mostly used for one-off reads and writes.
defaultPower :: String -> Maybe (Prog AORT)
defaultPower = flip M.lookup $ M.fromList $
    [("randomBytes", getRandomBytes)
    ,("destroy", const (return U))
    ,("getOSEnv", getOSEnv)
    ,("readFile", aoReadFile)
    ,("writeFile", aoWriteFile)
    ]

getRandomBytes :: (MonadIO m, Applicative m) => V m -> m (V m)
getOSEnv, aoReadFile, aoWriteFile :: (MonadIO m, Applicative m) => V m -> m (V m)

getRandomBytes (N r) | ((r >= 0) && (1 == denominator r)) =
    let nBytes = fromInteger $ numerator r in
    let getBytes = Entropy.getEntropy nBytes in
    textToVal . B.unpack <$> liftIO getBytes
getRandomBytes v = fail $ "randomBytes @ " ++ show v

getOSEnv (valToText -> Just var) = textToVal <$> liftIO gv where
    gv = maybe "" id <$> tryIO (Env.getEnv var)
getOSEnv v = fail $ "getOSEnv @ " ++ show v

aoReadFile (valToText -> Just fname) =
    let fp = FS.fromText (T.pack fname) in
    let rf = T.unpack <$> FS.readTextFile fp in
    maybe (L U) (R . textToVal) <$> liftIO (tryIO rf)
aoReadFile v = fail $ "readFile @ " ++ show v

aoWriteFile (P (valToText -> Just fname) (valToText -> Just content)) =
    let fp = FS.fromText (T.pack fname) in
    let wOp = FS.createTree (FS.directory fp) >>
              FS.writeTextFile fp (T.pack content) 
    in
    let asBoolean = maybe (L U) (const (R U)) in
    asBoolean <$> liftIO (tryIO wOp)
aoWriteFile v = fail $ "writeFile @ " ++ show v
    
tryIO :: IO a -> IO (Maybe a)
tryIO op = Err.catchIOError (Just <$> op) (const (return Nothing))

execPower :: V AORT -> AORT (V AORT)
execPower (P (valToText -> Just cmd) arg) = 
    execCmd cmd arg >>= \ result ->
    newPowerBlock >>= \ pb ->
    return (P (B pb) result)
execPower v = fail $ "power not recognized: " ++ show v

execAnno :: String -> Prog AORT
execAnno s arg = 
    readRT aort_anno >>= \ annoFn ->
    case annoFn s of
        Just fn -> fn arg
        Nothing -> return arg

-- command and arg, returning result
execCmd :: String -> V AORT -> AORT (V AORT)
execCmd ('&':s) arg = execAnno s arg -- every annotation is a command
execCmd cmd arg = 
    readRT aort_power >>= \ hasCmd ->
    case hasCmd cmd of
        Just op -> op arg
        Nothing -> fail $ "command not recognized: " ++ cmd

execPowerTok :: String -> Prog AORT
execPowerTok s arg =
    readRT aort_secret >>= \ secret ->
    if (s == secret) then execPower arg else
    fail $ "token not recognized: {!" ++ s ++ "}"

-- ... enough to get started for now ...
instance Runtime AORT where
    invoke ('&':s) = execAnno s
    invoke ('!':s) = execPowerTok s
    invoke s = invokeFails s


