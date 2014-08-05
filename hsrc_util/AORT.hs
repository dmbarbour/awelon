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
    , AORT_ERRORS(..) -- error aggregation
    , asynch  

    -- eventually, I might support some configuration
    --  but isn't needed while 'ao' and 'aoi' are only
    --  clients and one goal is equivalent behavior
    , newDefaultRuntime
    , newDefaultEnvironment

    , aoStdEnv
    ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
import Control.Monad.IO.Class 
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Typeable

import Control.Concurrent
import qualified Control.Exception as Err
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified System.IO as Sys
import qualified System.Entropy as Entropy
import qualified System.Environment as Env
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS

import Data.Ratio
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Base64.URL as B64

import ABC.Simplify
import ABC.Operators
import ABC.Imperative.Value
import ABC.Imperative.Runtime
import ABC.Imperative.Interpreter
import ABC.Resource
import AO.AOFile

import PureM -- speeds up 'pure' blocks or subprograms (but might remove after JIT)
import Util
import WorkerPool
import KeyedSched

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
    { aort_secret   :: B.ByteString -- random value per AORT instance
    , aort_powerSec :: String
    , aort_trySec   :: String
    , aort_anno     :: String -> Maybe (Prog AORT)  -- for {&anno}
    , aort_power    :: String -> Maybe (Prog AORT)  -- argument to power
    -- , aort_linker   :: MVar AORT_LN -- ABC resources & caches
    , aort_tasks    :: MVar [MVar [Err.SomeException]] -- entry per asynch
    , aort_asynch   :: IO () -> IO () -- using a worker pool
    , aort_ksynch   :: String -> IO () -> IO () -- one op per key; same worker pool
    } 

-- | AORT_LN is the code linker for AORT.
--
-- Currently, it is quite trivial, just loading the raw bytecode. 
-- But it should eventually integrate just-in-time compilation for
-- each loaded resource.
--
-- type AORT_LN = M.Map ResourceToken [Op]

newtype AORT_ERRORS = AORT_ERRORS [Err.SomeException] 
    deriving (Show, Typeable)
instance Err.Exception AORT_ERRORS

-- | run an arbitrary AORT program.
--
-- This will wait on all asynchronous operations, and will aggregate
-- any errors and (if any errors) throw an AORT_ERRORS exception. 
--
-- In aoi, runRT corresponds to a single command. 
-- In ao exec, runRT corresponds to a paragraph.
-- 
-- Resources loaded during a run may be unloaded afterwards. 
-- 
runRT :: AORT_CX -> AORT a -> IO a
runRT cx op = 
    try (runRT' cx op) >>= \ result ->
    taskWait cx >>= \ childErrors ->
    -- todo: clear loaded resources, etc.
    case (result,childErrors) of
        (Right r, []) -> return r
        (Right _, es) -> Err.throwIO (AORT_ERRORS es)
        (Left e, es) -> Err.throwIO (AORT_ERRORS (e:es))

-- simplistic run
runRT' :: AORT_CX -> AORT a -> IO a
runRT' cx (AORT op) = runReaderT (runPureM op) cx 

-- | read the runtime context
readRT :: (AORT_CX -> a) -> AORT a
readRT = AORT . lift . asks

-- | perform effectful operations within the runtime.
liftRT :: (AORT_CX -> IO a) -> AORT a
liftRT = AORT . lift . ReaderT

-- | a new runtime with default settings
newDefaultRuntime :: IO AORT_CX
newDefaultRuntime = 
    getNumCapabilities >>= \ nOSThreads ->
    let nWorkers = 2 + nOSThreads in -- arbitrary; tunable
    newWorkerPool nWorkers >>= \ sched ->
    newKeyedSched sched >>= \ ksched ->
    newMVar [] >>= \ taskList ->
    Entropy.getEntropy 48 >>= \ secret ->
    let cx = AORT_CX { aort_secret = secret
                     , aort_powerSec = mac secret "power"
                     , aort_trySec   = mac secret "try"
                     , aort_anno   = defaultAnno
                     , aort_power  = defaultPower
                     , aort_tasks  = taskList 
                     , aort_asynch = sched
                     , aort_ksynch = ksched
                     }
    in return cx

-- not quite a full HMAC, but good enough for now
mac :: B.ByteString -> String -> String
mac sec = 
    T.pack >>> T.encodeUtf8 >>> B.append sec >>> 
    secureHash >>> B.take 24 >>> B64.encode >>>
    T.decodeUtf8 >>> T.unpack

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
    ,("asynch", asynchBlock)
    ,("≡", assertEQ)
    ]

mkAnno :: (V AORT -> AORT ()) -> Prog AORT
mkAnno fn v = fn v >> return v

putErrLn :: String -> AORT ()
putErrLn = ksynch "stderr" . liftIO . Sys.hPutStrLn Sys.stderr

debugPrintRaw, debugPrintText :: V AORT -> AORT ()
debugPrintRaw = putErrLn . show
debugPrintText (valToText -> Just txt) = putErrLn txt
debugPrintText v = fail $ "{&debug print text} @ " ++ show v

-- compile a block
--
--    {&compile} :: (Block * e) → (Block * e)
--
-- Takes a dynamic block of ABC and returns an equivalent block that
-- hopefully computes more efficiently. This may rewrite the block 
-- to use ABC resources. Some blocks, e.g. those that are recognized
-- as already in an optimal compiled form, may be returned unchanged.
--
-- At the moment, {&compile} rewrites the block into an ABC resource,
-- which may then implicitly be compiled and loaded separately. 
-- 
compileBlock :: Prog AORT
compileBlock (P (B b) e) =
    let abc = (simplify . S.toList . b_code) b in
    if not (shouldCompile abc) then return (B b) else
    ksynch "rsc" (makeResource saveRscFile abc) >>= \ rscTok ->
    let b' = b { b_code = S.singleton (Tok rscTok)
               , b_prog = invoke rscTok
               }
    in 
    return (P (B b') e)
compileBlock v = fail $ "{&compile} @ " ++ show v -- type error

shouldCompile :: [Op] -> Bool
shouldCompile [] = False -- nothing to compile
shouldCompile [Tok ('#':_)] = False -- already compiled
shouldCompile _ = True -- otherwise, don't be picky

-- prepare a block to compute asynchronously
asynchBlock :: Prog AORT
asynchBlock (P (B b) e) = pure (P (B b') e) where
    prog' = asynch . b_prog b
    b' = b { b_prog = prog' }
asynchBlock v = fail $ "{&asynch} @ " ++ show v

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
newDefaultEnvironment = aoStdEnv <$> mkPowerBlock

mkPowerBlock :: AORT (Block AORT)
mkPowerBlock =
    readRT aort_powerSec >>= \ s ->
    let powerTok = ('!':s) in
    let powerBlock = Block { b_code = S.singleton (Tok powerTok)
                           , b_prog = invoke powerTok
                           , b_aff = True, b_rel = True }
    in
    return powerBlock

-- Default powers use the command string, rather than the token.
-- Toplevel powers are mostly used for one-off reads and writes.
defaultPower :: String -> Maybe (Prog AORT)
defaultPower = flip M.lookup $ M.fromList $
    [("randomBytes", getRandomBytes)
    ,("destroy", aoDestroy)
    ,("duplicate", aoDuplicate)
    ,("getOSEnv", getOSEnv)
    ,("readTextFile", aoReadTextFile)
    ,("readBinaryFile", aoReadBinaryFile)
    ,("readABCFile", aoReadABCFile)
    ,("writeTextFile", aoWriteTextFile)
    ,("writeBinaryFile", aoWriteBinaryFile)
    ,("writeABCFile", aoWriteABCFile)
    ,("listDirectory", aoListDirectory)
    ,("newTryCap", newTryCap)
    ]

aoDestroy,aoDuplicate :: Prog AORT
aoDestroy _ = return U
aoDuplicate v = return (P v v)

getRandomBytes :: Prog AORT
getOSEnv :: Prog AORT
aoReadTextFile, aoReadBinaryFile, aoReadABCFile :: Prog AORT
aoWriteTextFile, aoWriteBinaryFile, aoWriteABCFile :: Prog AORT
aoListDirectory :: Prog AORT
newTryCap :: Prog AORT

getRandomBytes (N r) | ((r >= 0) && (1 == denominator r)) =
    let nBytes = fromInteger $ numerator r in
    let getBytes = Entropy.getEntropy nBytes in
    liftIO $ binaryToVal <$> getBytes 
getRandomBytes v = fail $ "randomBytes @ " ++ show v

getOSEnv (valToText -> Just var) = textToVal <$> liftIO gv where
    gv = maybe "" id <$> tryJustIO (Env.getEnv var)
getOSEnv v = fail $ "getOSEnv @ " ++ show v

aoReadTextFile (valToText -> Just fname) =
    let fp = FS.fromText (T.pack fname) in
    let rf = T.unpack <$> FS.readTextFile fp in
    let toVal = maybe (L U) (R . textToVal) in
    fsynch fp $ liftIO $ toVal <$> tryJustIO rf
aoReadTextFile v = fail $ "readTextFile @ " ++ show v

aoReadBinaryFile (valToText -> Just fname) =
    let fp = FS.fromText (T.pack fname) in
    let rf = FS.readFile fp in
    let toVal = maybe (L U) (R . binaryToVal) in
    fsynch fp $ liftIO $ toVal <$> tryJustIO rf
aoReadBinaryFile v = fail $ "readBinaryFile @ " ++ show v

aoReadABCFile (valToText -> Just fname) = 
    let fp = FS.fromText (T.pack fname) in
    let rf = FS.readFile fp in
    let toVal = maybe (L U) (R . B . opsToBlock) . (>>= decodeABC) in
    fsynch fp $ liftIO $ toVal <$> tryJustIO rf
aoReadABCFile v = fail $ "readABCFile @ " ++ show v

opsToBlock :: (Runtime m) => [Op] -> Block m
opsToBlock ops = b where
    b = Block { b_aff = False, b_rel = False, b_code = code, b_prog = prog }
    code = S.fromList ops
    prog = interpret ops

aoWriteTextFile (P (valToText -> Just fname) (valToText -> Just content)) =
    let fp = FS.fromText (T.pack fname) in
    let wOp = FS.createTree (FS.directory fp) >>
              FS.writeTextFile fp (T.pack content) 
    in
    let asBoolean = maybe (L U) (const (R U)) in
    fsynch fp $ liftIO $ asBoolean <$> tryJustIO wOp
aoWriteTextFile v = fail $ "writeTextFile @ " ++ show v

aoWriteBinaryFile (P (valToText -> Just fname) (valToBinary -> Just content)) =
    let fp = FS.fromText (T.pack fname) in
    let wOp = FS.createTree (FS.directory fp) >>
              FS.writeFile fp content 
    in
    let asBoolean = maybe (L U) (const (R U)) in
    fsynch fp $ liftIO $ asBoolean <$> tryJustIO wOp
aoWriteBinaryFile v = fail $ "writeBinaryFile @ " ++ show v

aoWriteABCFile (P (valToText -> Just fname) (B block)) =
    let fp = FS.fromText (T.pack fname) in
    let content = encodeABC $ S.toList $ b_code block in
    let wOp = FS.createTree (FS.directory fp) >>
              FS.writeFile fp content
    in
    let asBoolean = maybe (L U) (const (R U)) in
    fsynch fp $ liftIO $ asBoolean <$> tryJustIO wOp
aoWriteABCFile v = fail $ "writeABCFile @ " ++ show v

aoListDirectory (valToText -> Just dirname) =
    let fp = FS.fromText (T.pack dirname) in
    let lsDir = maybe [] id <$> tryJustIO (FS.listDirectory fp) in
    asynch $ liftIO $ listToVal (textToVal . FS.encodeString) <$> lsDir
aoListDirectory v = fail $ "listDirectory @ " ++ show v

-- All operations on a given filename are serialized. This
-- ensures that reads and writes won't overlap, and that all
-- writes are properly ordered. There may be error in case
-- of filesystem links, and there may be loss of parallelism
-- if some files use the same name in different directories.
fsynch :: FS.FilePath -> AORT a -> AORT a
fsynch = ksynch . FS.encodeString . FS.filename


-- convert between values and bytestrings
binaryToVal :: B.ByteString -> V cx
binaryToVal b = case B.uncons b of
    Nothing -> (R U)
    Just (o,b') -> (L (P (N (fromIntegral o)) (binaryToVal b')))

valToBinary :: V cx -> Maybe B.ByteString
valToBinary = valToL >=> return . B.pack where
    valToL (L (P (N n) l')) | isByte n =
        let w8val = fromInteger (numerator n) in
        (w8val :) <$> valToL l' 
    valToL (R U) = pure []
    valToL _ = Nothing
    isByte n = (1 == denominator n) && (byteRange (numerator n))
    byteRange n = (0 <= n) && (n < 256)

-- try a subprogram, but allow returning with failure
--   1 → [(Block * Arg) → ((ErrorMsg*Arg) + Result)]
-- this is protected as a capability in AO (for many reasons)
newTryCap U = 
    readRT aort_trySec >>= \ s ->
    let tryTok = "try!" ++ s in
    return $ B $ Block { b_aff = False, b_rel = False
                       , b_code = S.singleton (Tok tryTok)
                       , b_prog = invoke tryTok
                       }
newTryCap v = fail $ "newTryCap @ " ++ show v


tryAORT :: String -> V AORT -> AORT (V AORT)
tryAORT s v = 
    readRT aort_trySec >>= \ s' ->
    if (s == s') then tryAORT' v else
    fail $ "invalid secret for {try} token"

tryAORT' :: V AORT -> AORT (V AORT)
tryAORT' (P (B b) a) = 
    liftRT pure >>= \ cx ->
    let op = try $ runRT cx $ b_prog b a in
    liftIO op >>= \ eb ->
    case eb of
        Right v -> return $  R v
        Left _err -> 
            let msg = "{try} aborted with: " ++ show _err in
            let msgV = S "tryFail" (textToVal msg) in 
            putErrLn msg >> 
            return (L (P msgV a))
tryAORT' v = fail $ "{try} @ " ++ show v

execPower :: String -> V AORT -> AORT (V AORT)
execPower s v = 
    readRT aort_powerSec >>= \ s' ->
    if (s == s') then execPower' v else
    fail $ "{!" ++ s ++ "} is invalid powerblock"

execPower' :: V AORT -> AORT (V AORT)
execPower' (P (valToText -> Just cmd) arg) =
    execCmd cmd arg >>= \ result ->
    mkPowerBlock >>= \ pb ->
    return (P (B pb) result)
execPower' v = fail $ "{!powerblock} expects (command*arg) @ " ++ show v

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
-- thoughts: it might be useful to support two-stage powers,
-- i.e. instead of String -> Val -> m Val
--      consider   String -> m (Val -> m Val)
-- This would allow some dispatching to be performed ahead of time.
instance Runtime AORT where
    invoke ('&':s) = execAnno s
    invoke s@('#':_) = invokeResource s
    invoke ('!':secret) = execPower secret
    invoke ('t':'r':'y':'!':s) = tryAORT s
    invoke s = invokeDefault s

-- | Execute a subprogram asynchronously.
--
-- aort_tasks will contain one entry for each asynchronous operation
-- that is a direct child of the current thread. The `runRT` operation
-- will wait for all children to complete... and each child will wait
-- for all its children before considering itself to be complete.
-- 
-- Technically, Awelon can allow a much greater degree of parallelism, 
-- but for now I wish to discourage incorrect use of 'asynch' for long
-- running behaviors. Its scope is limited to one step. 
--
asynch :: AORT a -> AORT a
asynch op =
    readRT aort_asynch >>= \ sched ->
    runAsynch sched op

ksynch :: String -> AORT a -> AORT a
ksynch k op =
    readRT aort_ksynch >>= \ ksched ->
    runAsynch (ksched k) op

runAsynch :: (IO () -> IO ()) -> AORT a -> AORT a
runAsynch sched op =
    liftRT pure >>= \ cx ->
    liftIO $
        newEmptyMVar >>= \ vStatus ->
        modifyMVar_ (aort_tasks cx) (pure . (vStatus:)) >>
        newMVar [] >>= \ childTaskList ->
        newEmptyMVar >>= \ vResult ->
        let cx' = cx { aort_tasks = childTaskList } in
        let asyncOp =
                try (runRT' cx' op) >>= \ result ->  -- always succeeds
                putMVar vResult result >> -- result is available ASAP
                taskWait cx' >>= \ es -> -- wait for children!
                let es' = either (:es) (const es) result in
                putMVar vStatus es' -- report final status with error info  
        in 
        let getResult = readMVar vResult >>= either Err.throwIO return in
        sched asyncOp >>
        unsafeInterleaveIO getResult
  

-- | wait for all active tasks to finish, and aggregate errors!
taskWait :: AORT_CX -> IO [Err.SomeException]
taskWait cx =
    let mv = aort_tasks cx in 
    takeMVar mv >>= \ tasks ->
    putMVar mv [] >>
    mapM takeMVar tasks >>= 
    return . L.concat

-- | for now, invokeResource will simply load and apply the resource.
-- I should add a cache, at least on a per-paragraph basis. Further
-- using a staged constructor could be useful (perhaps using unsafe
-- IO?) to process invokeResource before receiving the argument.
invokeResource :: ResourceToken -> Prog AORT
invokeResource rscTok v =
    -- load from file (todo: use local cache)
    loadResource loadRscFile rscTok >>= \ ops ->
    
    -- for now, just interpret the ops
    interpret ops v
    



