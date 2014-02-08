{-# LANGUAGE FlexibleContexts #-}

-- | The `ao` command line executable
--
-- `ao` follows the modern convention of taking a second word
-- as its command, followed by any options for that word.
--
--     ao test dict   -- run all 'test.' words in named dict
--
-- currently, there aren't many options! :)
--
-- I'm just using Parsec to handle the command line.
module Main 
    ( main, runMode
    , parseCmdLine, cmdLineHelp
    ) where

import Control.Applicative
-- import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Sequence as S
import qualified Text.Parsec as P
-- import qualified Text.Parsec.Pos as P
-- import qualified Text.Parsec.Token as P
import qualified System.IO as Sys
import qualified System.Environment as Env
import qualified System.IO.Error as Err
import AO.AO

-- not many modes to parse at the moment.
data Mode 
    = Test Import
    | Help

cmdLineHelp :: String
cmdLineHelp = 
    "ao test [dict]          run tests in dictionary\n" ++
    "ao help (or -?)         these options\n"

main :: IO ()
main = getMode >>= runMode

getMode :: IO Mode
getMode =
    Env.getArgs >>= \ args ->
    case P.parse parseCmdLine "" args of
        Left parseErr ->
            putErrLn (show parseErr) >>
            return Help -- default to help mode
        Right mode -> return mode

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

runMode :: Mode -> IO ()
runMode Help = runHelp
runMode (Test dict) = runTests dict

runHelp :: IO ()
runHelp = putErrLn cmdLineHelp >> return ()

runTests :: Import -> IO ()
runTests d0 =
    loadDictionary d0 >>= \ dict ->
    let dc = compileDictionary dict in
    let tests = M.filterWithKey isTestWord dc in
    mapM runTest (M.toList tests) >>=
    mapM_ reportTest

isTestWord :: W -> a -> Bool
isTestWord w _ = T.isPrefixOf (T.pack "test.") w

type TestArg = (W, S.Seq Op)
type TestResult = (W, Either Fail Pass)
type Fail = Text
type Pass = V IO

runTest :: TestArg -> IO TestResult
runTest (testWord, testCode) = run where
    run = toResult <$> Err.tryIOError mkTest
    toResult (Left e) = (testWord, Left $ T.pack $ show e)
    toResult (Right v) = (testWord, Right v)
    mkTest = undefined testCode

reportTest :: TestResult -> IO ()
reportTest (testWord, Left failure) =
    Sys.putStrLn ("(FAIL) " ++ T.unpack testWord) >>
    Sys.putStrLn (indent "  " (T.unpack failure)) >>
    return ()
reportTest (testWord, Right _) =
    Sys.putStrLn ("(pass) " ++ T.unpack testWord) >>
    return ()

indent :: String -> String -> String
indent spaces = L.unlines . L.map (spaces ++) . L.lines

---------------------------------------
-- Command Line Parser (Tok is command line argument)
---------------------------------------
type Tok = String

tok :: (P.Stream s m Tok) => (Tok -> Bool) -> P.ParsecT s u m Tok
tok match = P.tokenPrim id nextPos matchTok where
    nextPos pos _ _ = P.incSourceColumn pos 1
    matchTok t = if (match t) then Just t else Nothing

anyOneArg :: (P.Stream s m Tok) => P.ParsecT s u m Tok
anyOneArg = tok (const True)

parseTestMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseTestMode =
    tok (== "test") >>
    anyOneArg >>= 
    return . Test . T.pack

parseHelpMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseHelpMode = tok anyHelp >> P.eof >> return Help where
    anyHelp s = ("help" == s) || ("-help" == s) || ("-?" == s)

parseCmdLine :: P.Stream s m Tok => P.ParsecT s u m Mode
parseCmdLine = 
    parseTestMode P.<|> 
    parseHelpMode
