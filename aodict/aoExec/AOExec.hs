{-# LANGUAGE ViewPatterns #-}

-- | `aoExec` is a command line utility that will execute any one word
-- from the precompiled AO dictionary (the `aodict` module in Haskell).
-- Usage is:
--
--    aoExec word arg1 arg2 ... argN [+RTS options]
--
-- Extra arguments are provided on the AO stack as a list of strings.
-- Only the first word determines the program to be executed. A more
-- flexible aoExec usage will rely on the first word parsing the extra
-- arguments into an interesting program (e.g. using the dictionary).
--
-- The initial word must terminate. AO and ABC do not permit infinite
-- loops be expressed directly (an infinite fixpoint loop is always a
-- bug). To express long-running behaviors in aoExec, developers use 
-- the initial word to construct AO processes with implicit loops.
--
-- The details of process construction are not fully determined.
--
module Main where

import qualified Data.List as L
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Sys (getArgs)

import AOPrelude
import AODict (allWords)
import AOPower (newDefaultContext, processContext, executivePowers)

helpMsg :: String
helpMsg = 
    "USAGE: aoExec word arg1 arg2 .. argN [+RTS options]\n\
    \ \n\
    \  aoExec may only use words from the precompiled dictionary\n\
    \  additional arguments become a list of text on the stack\n\
    \ \n\
    \Peruse exec.ao and doc.aoExec for information, idioms, and utilities\n\
    \See GHC documentation for RTS options (multi-threading, profiling, etc.)\n\
    \"

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

main :: IO ()
main = Sys.getArgs >>= aoExec

aoExec :: [String] -> IO ()
aoExec [] = putErrLn helpMsg >> Sys.exitFailure
aoExec (word:args) = 
    case L.lookup word allWords of
        Nothing ->
            let emsg = "word '" ++ word ++ "' not found in static 'aodict' dictionary" in
            putErrLn emsg >> Sys.exitFailure
        Just action ->
            -- add arguments to stack to get full program
            let argv = (listToV . fmap textToV) args in
            let prog = fmap (P argv) >>> op_l >>> action in
            execute prog

listToV :: [V] -> V
listToV [] = L U
listToV (v:vs) = R (P v (listToV vs))

-- run the initial program in the standard environment
execute :: Program -> IO ()
execute prog =
    newDefaultContext >>= \ cx ->
    let pb  = executivePowers cx in
    let env = stdEnvWithPB pb in
    runAO env prog >> -- ignore result
    processContext cx -- potentially runs forever
