{-# LANGUAGE ViewPatterns, PatternGuards #-}

module Main where

import Control.Exception
import Data.IORef
import System.IO
import AODict

main :: IO ()
main = mapM_ (uncurry runIfTest) allWords

runIfTest :: String -> Program -> IO ()
runIfTest word prog | ("test." == take 5 word) = runTest word prog
                    | otherwise = return ()


type TestCX = IORef Results
data Results = Results
    { warnings :: [String]
    , failure :: Bool
    , expect_failure :: Bool
    } 

testPass, testWarn :: Results -> Bool
testPass r | null (warnings r) && not (failure r) = True
           | failure r && expect_failure r = True
           | otherwise = False
testWarn r | not (failure r) && not (null (warnings r)) = True
           | otherwise = False


addWarning :: String -> Results -> Results
addWarning s r = r { warnings = (s:warnings r) }

addFailure :: Results -> Results
addFailure r = r { failure = True }

mkTestCX :: IO TestCX
mkTestCX = newIORef (Results [] False False)

-- run a single test
runTest :: String -> Program -> IO ()
runTest word prog = 
    -- indicate which test is running
    putStr word >> 
    putStr ": " >> 
    hFlush stdout >>
    -- run the test
    mkTestCX >>= \ cx ->
    let testEnv = stdEnvWithPB (testPB cx) in
    let testAction = runAO testEnv prog >> return () in
    (catch testAction (reportIOError cx)) >>
    -- print the result
    readIORef cx >>= \ r ->
    let passMsg = "pass" in
    let lw = unlines $ reverse $ map (indent "  ") $ warnings r in
    let warnMsg = "Warn\n" ++ lw in
    let failMsg = "FAIL\n" ++ lw in
    let msg = if testPass r then passMsg else
              if testWarn r then warnMsg else
              failMsg
    in
    putStrLn msg >>
    hFlush stdout

indent :: String -> String -> String
indent pad = unlines . map (pad ++) . lines

reportIOError :: TestCX -> SomeException -> IO ()
reportIOError cx e = modifyIORef cx (addWarning (show e) . addFailure)

-- Tests are mostly pure at the moment, but developers can emit
-- warnings or errors.
testPB :: TestCX -> Block
testPB cx = Block True True (run =<<) where
    run (P (vToText -> Just cmd) args) = 
        runCmd cmd args >>= \ result ->
        return (P (B (testPB cx)) result) -- linear fixpoint powerblock
    run v = error ("unrecognized power: " ++ show v)
    runCmd "warn" (vToText -> Just msg) =
         modifyIORef cx (addWarning msg) >>
         return U
    runCmd "error" (vToText -> Just msg) =
        modifyIORef cx (addWarning msg . addFailure) >>
        fail "'error' invoked"
    runCmd cmd arg = error $ 
        "unrecognized command: " ++ cmd ++ " arg: " ++ show arg
