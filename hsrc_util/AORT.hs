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
    , scrubABC

    -- eventually, I might support some configuration
    --  but isn't needed while 'ao' and 'aoi' are only
    --  clients and one goal is equivalent behavior
    , newDefaultRuntime
    , newDefaultEnvironment
    , aoStdEnv

    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class 
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Typeable

import Control.Concurrent
import Data.IORef

import qualified System.IO as Sys
import qualified System.Entropy as Entropy
import qualified System.Environment as Env
import qualified System.IO.Error as Err
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS

import Data.Ratio
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import ABC.Simplify
import ABC.Operators
import ABC.Imperative.Value
import ABC.Imperative.Runtime

import PureM -- speeds up 'pure' blocks or subprograms (but might remove after JIT)
import Util
import JIT

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

-- | a new runtime with default settings
newDefaultRuntime :: IO AORT_CX
newDefaultRuntime = 
    let cx = AORT_CX { aort_anno   = defaultAnno
                     , aort_power  = defaultPower
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
    ,("compile", compileBlock)
    ,("compile trace", compileTraceBlock 3)
    ,("simplify", simplifyBlock)
    ]

mkAnno :: (V AORT -> AORT ()) -> Prog AORT
mkAnno fn v = fn v >> return v

debugPrintRaw, debugPrintText :: V AORT -> AORT ()
debugPrintRaw v =  liftIO $ Sys.hPutStrLn Sys.stderr (show v)
debugPrintText (valToText -> Just txt) = liftIO $ Sys.hPutStr Sys.stderr txt
debugPrintText v = fail $ "{&debug print text} @ " ++ show v

-- compile a block {&compile}
-- (this is asynchronous, in case it helps)
compileBlock :: Prog AORT
compileBlock (B b) = B <$> liftIO (asyncIO $ compileBlock' b)
compileBlock v = fail $ "{&compile} @ " ++ show v

compileBlock' :: (Runtime m) => Block m -> IO (Block m)
compileBlock' b = 
    let abc = simplify $ S.toList $ b_code b in
    try (abc_jit abc) >>= \ errOrProg ->
    case errOrProg of 
        Left err -> 
            Sys.hPutStrLn Sys.stderr (show err) >> 
            return b -- the original
        Right prog ->
            return (b { b_code = S.fromList abc, b_prog = prog })

-- compile a block for simple counting trace and a hot-swap
compileTraceBlock :: Int -> Prog AORT
compileTraceBlock n (B b) =
    liftIO (newIORef (n, b)) >>= \ rf -> 
    let b' = b { b_prog = tracedBlock rf } in
    return (B b')
compileTraceBlock _ v = fail $ "{&compile trace} @ " ++ show v

getAndDec :: (Integral n) => IORef (n, a) -> IO (n, a)
getAndDec = flip atomicModifyIORef update where
    update (n,a) = let s = (n-1,a) in (s,s)

getTracedProg :: (Runtime m) => IORef (Int, Block m) -> IO (Prog m)
getTracedProg rf = 
    liftIO (getAndDec rf) >>= \ (n,b) ->
    when (0 == n) (forkCompile rf) >> -- c
    return (b_prog b)

forkCompile :: (Runtime m) => IORef (a, Block m) -> IO ()
forkCompile rf = void $ forkIO $ 
    readIORef rf >>= \ (_,b0) -> -- access block
    compileBlock' b0 >>= \ bf -> -- compile
    atomicModifyIORef rf (\(n,_) -> ((n,bf),()))

tracedBlock :: (MonadIO m, Runtime m) => IORef (Int, Block m) -> Prog m
tracedBlock rf arg = liftIO (getTracedProg rf) >>= \ prog -> prog arg 

simplifyBlock :: Prog AORT
simplifyBlock (B b) = return (B b') where
    code' = S.fromList $ simplify $ S.toList $ b_code b
    b' = b { b_code = code' }
simplifyBlock v = fail $ "{&simplify} @ " ++ show v


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
    where sn = textToVal "" -- R U

-- | create a standard environment with a default powerblock
newDefaultEnvironment :: AORT (V AORT)
newDefaultEnvironment = pure (aoStdEnv aortPowerBlock)

-- For now, just using a fixed powers token. In case of open systems,
-- it might be useful to use a randomized token... OTOH, it should be
-- okay to translate tokens at the boundary (e.g. using HMACs on local
-- tokens, and tagging/rewriting remote tokens as they arrive). I plan
-- to try the rewrite-at-VM-boundaries method, first.
powerTok :: String
powerTok = "!"

-- obtain a block representing access to default AORT powers.
aortPowerBlock :: Block AORT
aortPowerBlock = Block { b_code = S.singleton (Tok powerTok)
                      , b_prog = invoke powerTok -- keeping it simple
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
    ,("newTryCap", newTryCap)
    ]

getRandomBytes :: (MonadIO m, Applicative m) => V m -> m (V m)
getOSEnv, aoReadFile, aoWriteFile :: (MonadIO m, Applicative m) => V m -> m (V m)
newTryCap :: V AORT -> AORT (V AORT)

getRandomBytes (N r) | ((r >= 0) && (1 == denominator r)) =
    let nBytes = fromInteger $ numerator r in
    let getBytes = Entropy.getEntropy nBytes in
    textToVal . B.unpack <$> liftIO getBytes
getRandomBytes v = fail $ "randomBytes @ " ++ show v

getOSEnv (valToText -> Just var) = textToVal <$> liftIO gv where
    gv = maybe "" id <$> tryJustIO (Env.getEnv var)
getOSEnv v = fail $ "getOSEnv @ " ++ show v

aoReadFile (valToText -> Just fname) =
    let fp = FS.fromText (T.pack fname) in
    let rf = T.unpack <$> FS.readTextFile fp in
    maybe (L U) (R . textToVal) <$> liftIO (tryJustIO rf)
aoReadFile v = fail $ "readFile @ " ++ show v

aoWriteFile (P (valToText -> Just fname) (valToText -> Just content)) =
    let fp = FS.fromText (T.pack fname) in
    let wOp = FS.createTree (FS.directory fp) >>
              FS.writeTextFile fp (T.pack content) 
    in
    let asBoolean = maybe (L U) (const (R U)) in
    asBoolean <$> liftIO (tryJustIO wOp)
aoWriteFile v = fail $ "writeFile @ " ++ show v

-- try a subprogram, but allow returning with failure
--   1 → [(Block * Arg) → ((ErrorMsg*Arg) + Result)]
-- this is protected as a capability in AO (for many reasons)
newTryCap U = return (B b) where
    b = Block { b_aff = False, b_rel = False
              , b_code = S.singleton (Tok tryTok)
              , b_prog = invoke tryTok }
newTryCap v = fail $ "newTryCap @ " ++ show v

-- for now, let's just do a simple implementation...
tryTok :: String
tryTok = "try"

tryAORT :: V AORT -> AORT (V AORT)
tryAORT (P (B b) a) = 
    liftRT pure >>= \ cx ->
    let op = Err.tryIOError $ runRT cx $ b_prog b a in
    liftIO op >>= \ eb ->
    case eb of
        Right v -> return $  R v
        Left e -> return $ L (P (textToVal (show e)) a)
tryAORT v = fail $ "{try} @ " ++ show v

execPower :: V AORT -> AORT (V AORT)
execPower (P (valToText -> Just cmd) arg) = 
    execCmd cmd arg >>= \ result ->
    return (P (B aortPowerBlock) result)
execPower v = fail $ "{!} expecting (command*arg) @ " ++ show v

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

-- ... enough to get started for now ...
instance Runtime AORT where
    invoke ('&':s) = execAnno s
    invoke s | (s == powerTok) = execPower
    invoke s | (s == tryTok) = tryAORT
    invoke s = invokeFails s

-- | For a raw ABC input stream, we want to forbid tokens that are
-- not supported by AO. This ensures equivalence for expressiveness
-- and security between `ao exec` and `ao exec.abc` for example. 
--
-- Currently, this is accomplished by escaping non-AO tokens with
-- the '~' character. This ensures clients cannot install a new
-- power block (under normal conditions). 
scrubABC :: [Op] -> [Op]
scrubABC = fmap scrubTok where
    scrubTok (Tok t) | not (okayInAO t) = Tok (scrub t)
    scrubTok (BL ops) = BL (scrubABC ops)
    scrubTok op = op
    scrub = ('~':)
    okayInAO ('&':_anno) = True
    okayInAO (':':_sealer) = True
    okayInAO ('.':_unseal) = True
    okayInAO _ = False
