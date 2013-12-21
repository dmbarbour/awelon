{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances #-}

-- | AOI describes a simplistic, imperative REPL for language AO.
-- AOI will start by importing the "aoi" dictionary unless a .ao 
-- file is specified on the command line, in which case the specified
-- dictionary is loaded.
--
--   TODO: leverage haskeline, possibly ansi-terminal
-- 
-- AOI has a trivial effects model, documented in the standard aoi 
-- dictionary file. AOI also enables reprogramming the interpreter
-- from within. If a word 'prelude.aoi' is found, it will run before
-- handing the interpreter to the user.
--
-- AOI does keep track of historical states, using an exponential
-- decay algorithm. In addition, it keeps track of frames in the
-- current state. Between these attributes, AOI should be decent 
-- for debugging (providing both a stack trace and a history). 
--
-- Interactive AO is intended to be reactive like spreadsheets, with
-- more pure functions or RDP behaviors in test environments. AOI is
-- only intended to help gain confidence with libraries and support
-- bootstrap. That is, AOI is considered a useful scaffolding but 
-- is not intended to become a final product. 
module AOI
    ( AOI, runAOI
    , main
    -- stuff that should be in separate modules
    , HLS(..), hls_init, hls_get, hls_put
    ) where

import qualified System.IO as Sys
import qualified System.IO.Error as Err
import qualified System.Environment as Env
import qualified System.Random as R
import qualified Data.Text as T
import qualified Data.List as L
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import qualified Text.Parsec as P
import Data.Text (Text)
import AO
import ABC

-- states with exponential decay of history. 
-- defaults to a halflife of 6 steps.
data HLS s = HLS 
    { hls_halflife :: Double
    , hls_rgen     :: R.StdGen
    , hls_states   :: [s]
    , hls_join     :: s {-newer-} -> s {-older-} -> s {-joined-}
    }
hls_init :: s -> HLS s
hls_init s = HLS
    { hls_halflife = 6
    , hls_rgen = mkStdGen 8675309
    , hls_states = [s]
    , hls_join = const
    }
hls_put :: s -> HLS s -> HLS s
hls_put s hls =
    let (r,g') = R.random (hls_rgen hls) in
    let rDrop = (1.0 - r) / (negate (hls_halflife hls)) in
    let nDrop = ceiling rDrop in
    let ss = dropHLS (hls_join hls) nDrop (hls_states hls) in
    hls { hls_rgen = g', hls_states = s:ss }

-- dropHLS will join two items in a list at the given position
-- (or, if that position does not exist, the original list is returned)
dropHLS :: (s -> s -> s) -> Integer -> [s] -> [s]
dropHLS 

hls_get :: HLS s -> s
hls_get = head . hls_states -- partial function, but hls_states is never empty
 

-- AOI
data AOI_STATE = AOI_STATE
    { aoi_dictC :: DictC
    -- Debugging
    , aoi_frames  :: [Text]
    , aoi_hist_hl :: Rational    -- halflife for history
    -- AOI's programmable interpreter [e0 text -- e0' text']
    , aoi_text   :: Text -- leftover text (to be parsed)
    , aoi_envU   :: V -- user's operating environment
    , aoi_ifn    :: ABC -- must be a block
    , aoi_ifn_bt :: BT -- (to recover block)
    , aoi_envI   :: V -- environment for interpretation
    -- AOI default interpreter (action on {aoi}). 
    , aoi_ifn0   :: AOI_STATE -> IO AOI_STATE
    -- AOI powerblock behavior (action on `{!secret}`).
    , aoi_secret :: Text
    , aoi_power  :: AOI_STATE -> V -> IO (AOI_STATE, V)
    }
newtype AOI a = AOI { _runAOI :: AOI_STATE -> IO (AOI_STATE, a) }
instance Monad AOI where
    return a = AOI $ \ s -> return (s,a)
    (>>=) m f = AOI $ \ s ->
        _runAOI m s >>= \ (s', a) -> 
        _runAOI (f a) s' 
    fail msg = AOI $ \ s -> fail (msg ++ "\n" ++ fullTrace s)

fullTrace, stackTrace, envTrace, interpreterTrace :: AOI_STATE -> String
fullTrace s = stackTrace s 
            ++ "\n" ++ envTrace s
            ++ "\n" ++ interpreterTrace s
stackTrace
stackTrace s = trace where
    trace = stackTrace ++ envTrace ++ interpreterTrace
    stackTrace = "STACKTRACE

        let msg ++ stackTrace s in
        let traceMsg = T.intercalate (T.pack "\n → ") (aoi_frames s) in
        let msg' = msg ++ "\n @ " ++ (T.unpack traceMsg) 
                       ++ "\n TEXT: " ++ (T.unpack (aoi_text s))
in
        
        fail 

-- AOI will load just one dictionary. This can be configured as a
-- command line argument (a '.ao' file) or will default to loading
-- the 'aoi' dictionary. Developers CANNOT update the aoi dictionary
-- at runtime, at least not directly. (Clever use of switchAOI could
-- 
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
main = 
    aoiDict >>= \ d0 ->
    Sys.putStrLn "okay, it compiles"
   

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


