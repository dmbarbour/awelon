{-# LANGUAGE ViewPatterns #-}

-- | `aoExec` is a command line utility that will execute any one word
-- from the precompiled AO dictionary (the `aodict` module in Haskell).
-- Usage is:
--    aoExec word arg1 arg2 arg3  (in addition to RTS opts)
--
-- Extra arguments are provided on the AO stack as a list of strings,
-- so only the first word determines the program to be executed.
--
module Main where

import Control.Monad
import qualified Data.List as L
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.Environment as Sys (getArgs)
import AODict

helpMsg :: String
helpMsg = 
    "USAGE: aoExec flag* word arg1 arg2 .. argN\n\
    \\n\
    \  aoExec may only use words from the precompiled dictionary\n\
    \  additional arguments become a list of text on the stack\n\
    \\n\
    \FLAGS: any argument starting with '-'\n\
    \  -p    print top element on the stack when finished\n\
    \\n\
    \aoExec is not very flexible, but could become more flexible if\n\
    \the AO dictionary is developed appropriately. See 'exec.ao' for\n\
    \information and utilities. Try `aox` for flexible interaction.\n\
    \"

flagPrint :: String
flagPrint = "-p"

exitBadArgs :: IO a
exitBadArgs = putErrLn helpMsg >> Sys.exitFailure

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

main :: IO ()
main = getOpts >>= aoExec

-- trivial (flags, word, args)
type Opts = ([String],String,[String]) 

getOpts :: IO Opts
getOpts =
    Sys.getArgs >>= \ cmdLineArgs ->
    let (flags,nonFlags) = L.span isFlag cmdLineArgs in
    case nonFlags of
        [] -> exitBadArgs
        (word:args) -> return $ (flags,word,args)

-- a flag is any argument that starts with a dash
isFlag :: String -> Bool
isFlag ('-':_s) = True
isFlag _ = False

aoExec :: Opts -> IO ()
aoExec (flags, word, args) = run prog where
    prog = L.lookup word allWords
    run Nothing = 
        let emsg = "word '" ++ word ++ "' not found in dictionary" in
        putErrLn emsg >> Sys.exitFailure
    run (Just action) = mkEnv >>= \ env -> runAO env action >>= finish
    mkEnv =
        mkExecPB >>= \ pb ->
        let env0 = stdEnvWithPB pb in
        let env = pushStack (argsToV args) env0 in
        return env
    finish v = maybePrint v
    maybePrint v = when (L.elem flagPrint flags) $ doPrint v
    doPrint (P (P x _s) _e) = Sys.putStrLn (summaryV maxBound x)
    doPrint v =
        putErrLn "('-p' error: stack not recognized)" >>
        putErrLn (summaryV 3 v)

argsToV :: [String] -> V
argsToV = listToV . map textToV

listToV :: [V] -> V
listToV [] = L U
listToV (v:vs) = R (P v (listToV vs))

pushStack :: V -> V -> V
pushStack v (P s e) = (P (P v s) e)
pushStack _ v = error ("invalid stack: " ++ show v)


-------------------------------------------------
-- POWER BLOCK
-- 
-- This is where the majority of 'aoExec' logic will be,
-- to support flexible side-effects. 'aoExec' is aimed at
-- imperative programs, including:
--
--   - bootstrap AO compiler
--   - web services, for UI and IDE development
--
-- This shall involve the first 'real' implementation for
-- side-effects. I'll most likely separate some of these
-- into other libraries (eventually...)
--
mkExecPB :: IO Block
mkExecPB = return $ Block True True ((=<<) pbTodo) where
    pbTodo v = fail $ "TODO: handle " ++ show v

