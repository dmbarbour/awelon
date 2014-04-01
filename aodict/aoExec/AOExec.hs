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

import qualified Data.List as L
import System.Environment (getArgs)
import AODict

helpMsg :: String
helpMsg = 
    "USAGE: aoExec word arg1 arg2 ... argN\n\
    \n\
    \  'word' must have been pre-compiled into 'aodict' package\n\
    \  arguments become a list of strings on the stack\n\
    \n\
    \In practice, the word must be designed for this usage!\n\
    \Peruse 'exec.ao' for information and utilities.\n"

main :: IO ()
main = getArgs >>= execArgs

execArgs :: [String] -> IO ()
execArgs ((gw->Just action):args) = aoExec action args
execArgs _ = putStrLn helpMsg

aoExec :: Program -> [String] -> IO ()
aoExec action args =
    mkExecPB >>= \ pb ->
    let env0 = stdEnvWithPB pb in
    let env = pushStack (argsToV args) env0 in
    runAO env action >> return ()

-- select a program from the dictionary
gw :: String -> Maybe Program
gw = flip L.lookup allWords

argsToV :: [String] -> V
argsToV = listToV . map textToV

listToV :: [V] -> V
listToV [] = L U
listToV (v:vs) = R (P v (listToV vs))

pushStack :: V -> V -> V
pushStack v (P s e) = (P (P v s) e)
pushStack _ v = error ("invalid stack: " ++ show v)

mkExecPB :: IO Block
mkExecPB = return $ Block True True ((=<<) pbTodo) where
    pbTodo v = fail $ "TODO: handle " ++ show v

