{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ViewPatterns #-}

-- | A runtime monad for the 'ao' and 'aoi' executables.
--
-- At the top level, 'ao' and 'aoi' have an imperative process model
-- for the streaming input. Long running behavior is in development,
-- but will likely lean towards Awelon project: installation of RDP
-- subprograms, with a live-programming style, and live programming
-- modeled within the toplevel environment. 
--
-- Originally, I was pursuing an imperative model for AO to support
-- bootstrap. However, I think this isn't critical. I can presumably
-- support bootstrap without a process model at all, with a little 
-- introspection (e.g. to access block code).
--
-- Meanwhile, I'm not fond of the imperative process models. It has
-- poor features for extensibility and live update. I'd rather not be
-- stuck with the imperative model as a 'local optima' of the design
-- space. Perhaps better to skip it entirely in favor of RDP, even if
-- it means more work in Haskell for now.
--
-- The immediate goal with AORT is to support high performance in the
-- toplevel via JIT compilation and asynchronous evaluation and perhaps
-- some special support for matrices or GPGPU computation. I want no 
-- severe performance limitations on applications. 
-- 
module AORT
    ( AORT, AORT_CX, readRT, liftRT, runRT, liftIO
    , newDefaultRuntime
    , newDefaultEnvironment, newDefaultPB, aoStdEnv
    , newLinearCap
    ) where

import Control.Applicative
import Control.Monad.IO.Class 
import Control.Monad.Trans.Reader
import Data.Typeable

import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.IO as Sys

import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

import ABC.Operators
import ABC.Imperative.Value
import ABC.Imperative.Runtime
import ABC.Imperative.Interpreter

-- | AORT is intended to be a primary runtime monad for executing
-- AO or ABC programs, at least for imperative modes of execution.
newtype AORT a = AORT (ReaderT AORT_CX IO a)
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
    { aort_lcaps  :: MVar (M.Map Token (Prog AORT)) 
    , aort_anno   :: String -> Maybe (Prog AORT)
    , aort_gensym :: IO Token
    } 
type Token = String

-- | run an arbitrary AORT program.
runRT :: AORT_CX -> AORT a -> IO a
runRT cx (AORT op) = runReaderT op cx

-- | read the runtime context
readRT :: (AORT_CX -> a) -> AORT a
readRT = AORT . asks

-- | perform effectful operations within the runtime.
liftRT :: (AORT_CX -> IO a) -> AORT a
liftRT = AORT . ReaderT

-- | a new runtime with default settings
newDefaultRuntime :: IO AORT_CX
newDefaultRuntime = 
    MVar.newMVar M.empty >>= \ mvCaps ->
    newDefaultGenSym >>= \ gensym ->
    let cx = AORT_CX { aort_lcaps  = mvCaps
                     , aort_anno   = defaultAnno
                     , aort_gensym = gensym
                     }
    in return cx

-- | Create a linear capability that will execute a particular program
-- when applied. Effects can be modeled through use of the capability.
-- This capability should be safe for serialization, e.g. JIT. At the
-- moment, however, it is not secure (because AO gensym is not secure).
newLinearCap :: String -> Prog AORT -> AORT (Block AORT)
newLinearCap debugHint prog =
    liftRT aort_gensym >>= \ tok0 -> 
    let showHint = (showString . clnTok) debugHint in
    let token = (showChar '!' . showHint . showChar ' ') tok0 in
    let b = Block { b_aff = True, b_rel = True
                  , b_code = S.singleton (Tok token)
                  , b_prog = invoke token }
    in
    installLinearCap token prog >>
    return b

-- add a linear capability to the search map
installLinearCap :: String -> Prog AORT -> AORT ()
installLinearCap tok prog = liftRT (mod ins . aort_lcaps) where
    mod fn mv = MVar.takeMVar mv >>= MVar.putMVar mv . fn
    ins = M.insert tok prog

extractLinearCap :: String -> AORT (Prog AORT)
extractLinearCap tok =
    readRT aort_lcaps >>= \ mv ->
    liftIO (MVar.takeMVar mv) >>= \ lcs ->
    case M.lookup tok lcs of
        Nothing -> 
            liftIO (MVar.putMVar mv lcs) >> 
            fail ("unrecognized token: {" ++ show tok ++ "}")
        Just prog ->
            liftIO (MVar.putMVar mv (M.delete tok lcs)) >>
            return prog

-- forbid use of '{', '\n', and '}' in token debug hints
clnTok :: String -> String
clnTok = fmap mc where
    mc '{' = '('
    mc '}' = ')'
    mc '\n' = ';'
    mc c = c

-- | create a new unique-symbol generator
-- (todo: create SECURE symbol generator)
newDefaultGenSym :: IO (IO Token)
newDefaultGenSym = incsym <$> MVar.newMVar 0 where
    incsym :: MVar Integer -> IO Token
    incsym mv = 
        MVar.takeMVar mv >>= \ n ->
        let tok = show n in
        let n' = (n+1) in
        n' `seq` MVar.putMVar mv n' >>
        return tok

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
newDefaultEnvironment = aoStdEnv <$> newDefaultPB

-- | obtain a block representing access to default AORT powers.
--
-- default powers are in development; cf doc ProcessModel.md
--
newDefaultPB :: AORT (Block AORT) 
newDefaultPB = newLinearCap "AORT" ((=<<) run) where
    run v = fail $ "AORT has no effects yet: " ++ show v

-- | default annotations support for AORT
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
mkAnno fn getV = getV >>= \ v -> fn v >> return v

debugPrintRaw, debugPrintText :: V AORT -> AORT ()
debugPrintRaw v =  liftIO $ Sys.hPutStrLn Sys.stderr (show v)
debugPrintText (valToText -> Just txt) = liftIO $ Sys.hPutStr Sys.stderr txt
debugPrintText v = fail $ "{&debug print text} @ " ++ show v

-- ... enough to get started for now ...
instance Runtime AORT where
    invoke ('&':s) = \ arg ->
        readRT aort_anno >>= \ annoFn ->
        case annoFn s of
            Just fn -> fn arg
            Nothing -> id arg
    invoke lc@('!':_) = \ arg ->
        extractLinearCap lc >>= \ prog ->
        prog arg
    invoke s = invokeFails s

