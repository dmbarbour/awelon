
-- | AOI describes a simplistic, imperative REPL for language AO.
-- AOI will start by importing the "aoi" dictionary unless a .ao 
-- file is specified on the command line, in which case the specified
-- dictionary is loaded. AOI has a trivial effects model, documented
-- in the standard aoi dictionary file. 
--
-- Interactive AO is intended to be reactive like spreadsheets, with
-- more pure functions or RDP behaviors in test environments. AOI is
-- only intended to help gain confidence with libraries and support
-- bootstrap. 
--
-- This REPL keeps its value from step to step, so the session 
-- becomes one long word. The dictionary is provided as a command
-- line argument, or by AOI_DICT.
-- define words. It isn't very usable, but it will be enough
-- to gain some confidence in the dictionary.
module AOI
    ( runIOApp
    , main
    ) where

import qualified System.IO as Sys
import qualified System.IO.Error as Err
import qualified System.Environment as Env
import qualified Data.Text as T
import qualified Data.List as L
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import qualified Text.Parsec as P
import Data.Text (Text)
import AO
import ABC

-- AOI will load just one dictionary. This can be configured as a
-- command line argument (a '.ao' file) or will default to loading
-- the 'aoi' dictionary.
aoiDict :: IO DictC
aoiDict =
    Env.getArgs >>= \ args ->
    let aoFiles = (L.filter ((T.pack ".ao") `T.isSuffixOf`) . L.map T.pack) args in
    let loadAction = case aoFiles of
            [] -> importDictC [T.pack "aoi"]
            (fn:[]) -> loadDictC (FS.fromText fn)
            _ -> fail ("did not understand arguments: " ++ show args)
    in
    loadAction >>= \ (errors, dictC) ->
    mapM_ (Sys.hPutStrLn Sys.stderr . T.unpack) errors >>
    return dictC

main :: IO ()
main = Sys.putStrLn "okay, it compiles"
   

-- | For quick bootstrap purposes, I've created a simplified 'ioapp'
-- application model. This doesn't contain much more than is needed
-- to bootstrap.
--
-- AO's frame annotations are supported so we can more readily find
-- and debug errors. However, no other annotations are supported.
runIOApp :: ABC -> IO V
runIOApp abc = _runIOApp ioApp [] >>= return . fst where
    ioApp = runABC ioAppInvoker ioAppPower abc

-- ioAppPower is simply [{!}]
ioAppPower :: V
ioAppPower = B (BT True True) (ABC [Invoke (T.singleton '!')])

type Frame = [Text]
newtype IOAPP a = IOAPP { _runIOApp :: Frame -> IO (a, Frame) }
instance Monad IOAPP where
    return point = IOAPP $ \ frame -> return (point,frame)
    (>>=) action continuation = IOAPP $ \ frame -> 
        _runIOApp action frame >>= \ (result, frame') ->
        _runIOApp (continuation result) frame'
    fail msg = IOAPP $ \ frames ->
        let trace = T.intercalate (T.pack "\n \x2192 ") (L.reverse frames) in
        let msg' = msg ++ "\nWORD TRACE:\n" ++ (T.unpack trace) in
        fail msg' 

popFrame :: IOAPP ()
popFrame = IOAPP pf where
    pf (_:fs) = return ((),fs)
    pf [] = fail "empty frame stack, cannot pop frame"

pushFrame :: Text -> IOAPP ()
pushFrame t = IOAPP pf where
    pf fs = return ((),t:fs)

-- the main ioAppMethod, with some error handling.
ioAppMethod :: (V -> IO V) -> V -> IOAPP V
ioAppMethod method message = IOAPP $ \ frames -> 
    Err.tryIOError (method message) >>= \ result ->
    case result of
        Right response -> return ((P ioAppPower response), frames)
        Left err -> _runIOApp (fail (show err)) frames

ioAppInvoker :: Text -> V -> IOAPP V
ioAppInvoker t v =
    case T.uncons t of
        Just ('&', anno) ->
            case (T.uncons anno) of
                Just ('@', tFrame) ->
                    case (T.uncons tFrame) of
                        Just ('-', _) -> popFrame >> return v
                        _ -> pushFrame tFrame >> return v
                _ -> return v -- ignoring other annotations
        Just ('!', t') | T.null t' -> ioAppMethod ioSwitch v
        _ -> let emsg = "invalid power {" ++ T.unpack t ++ "}" in
             ioAppMethod (const (fail emsg)) v

ioSwitch :: V -> IO V
ioSwitch (P l v) =
    case fromABCV l of
        Just label -> ioSwitch' (T.unpack label) v
        Nothing -> fail "unrecognized label"
ioSwitch _ = fail "expected: (textLabel * value)"

ioSwitch' :: String -> V -> IO V
ioSwitch' "readFile" fileName = error "todo: readFile"
ioSwitch' "writeFile" (P fileName fileText) = error "todo: writeFile"
ioSwitch' "getEnv" envVar = error "todo: getEnv"
ioSwitch' "getArgs" U = error "todo: getArgs"
ioSwitch' _ _ = fail "unknown operation"


