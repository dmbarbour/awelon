{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances #-}

-- | AOI describes a simplistic, imperative REPL for language AO.
-- AOI will start by importing the "aoi" dictionary unless a .ao 
-- file is specified on the command line. Developers cannot modify
-- definitions from within aoi. However, they may tweak original 
-- files and use 'reload' (or 'reset'). 
--
--   TODO: leverage haskeline, possibly ansi-terminal
--         consider support for acid-state persistent sessions
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
    ( main
    -- stuff that should probably be in separate modules
    , HLS(..), hls_init, hls_get, hls_put
    , randomBytes, randomText
    ) where

import Control.Monad
import qualified System.IO as Sys
import qualified System.IO.Error as Err
import qualified System.Environment as Env
import qualified System.Random as R
import qualified Crypto.Random.AESCtr as CR
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import qualified Text.Parsec as P
import Data.Text (Text)
import AO
import ABC

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- AOI_STATE holds state for the execution and debugging 
data AOI_STATE = AOI_STATE
    { aoi_frames  :: [Text] -- current frame stack
    , aoi_text    :: Text   -- text to be parsed
    , aoi_envU    :: V      -- user environment
    , aoi_envI    :: V      -- interpreter environment
    , aoi_ifn     :: ABC    -- must be a block
    , aoi_ifn_bt  :: BT     -- (to recover block)
    }
-- AOI_CONTEXT holds mutable state plus runtime constants
data AOI_CONTEXT = AOI_CONTEXT 
    { aoi_dict    :: DictC  -- loaded dictionary
    , aoi_secret  :: Text   -- secret held by powerblock
    , aoi_power   :: M.Map Text (V -> AOI V) -- common powers
    , aoi_source  :: Either [Import] FS.FilePath -- for reloads
    , aoi_state   :: HLS AOI_STATE -- mutable state and history
    }
type Error = Text
newtype AOI a = AOI { runAOI :: AOI_CONTEXT -> IO (AOI_CONTEXT, Either Error a) }

instance Monad AOI where
    return a = AOI $ \ cx -> return (cx, Right a)
    (>>=) m f = AOI $ \ cx ->
        runAOI m cx >>= \ r ->
        case snd r of
            Left e -> return r
            Right a -> runAOI (f a) (fst r)
    fail msg = AOI $ \ cx -> return (cx, Left (T.pack msg))

-- run IO while catching IOError exceptions. 
aoiIOAction :: IO a -> AOI a 
aoiIOAction m = AOI $ \ cx ->
    Err.tryIOError m >>= \ ea ->
    case ea of
        Left e -> (cx, Left (T.pack (show e)))
        Right a -> (cx, Right a)

aoiInvoker :: Text -> V -> AOI V
aoiInvoker t v = 
    case T.uncons t of
        Just ('&',anno) -> aoiAnno anno >> return v
        Just ('!',s) -> aoiTrySecret s v
        Just ('i',m) | T.null m -> aoiInterperet v
        _ -> fail ("unknown invocation {" ++ T.unpack t ++ "}")

-- aoi ignores most annotations, except for frames
aoiAnno :: Text -> AOI ()
aoiAnno t = 
    case T.uncons t of
        Just('@',frame) | frame == (T.singleton '-') = popFrame
                        | otherwise = pushFrame frame
        _ -> return ()

aoiTrySecret :: Text -> V -> AOI V 
aoiTrySecret s message = 
    aoiGetSecret >>= \ secret ->
    let badMsg = "illegal power: {!" ++ T.unpack s ++ "}" in
    if (s /= secret) then fail badMsg else
    aoiCommandDispatch message >>= \ response ->
    return (pb secret `P` response)

aoiCommandDispatch :: V -> AOI V
aoiCommandDispatch v = case fromABCV v of
    Nothing -> fail ("expecting (label * msg), got: " ++ show msg)
    Just (label, msg) -> 
        aoiGetPowers >>= \ mp ->
        case M.lookup label mp of
            Nothing -> fail ("unknown action: " ++ T.unpack label)
            Just action -> action msg

aoiGetSecret :: AOI Text
aoiGetSecret = AOI $ \ cx -> (cx, (Right . aoi_secret) cx)

aoiGetPowers :: AOI (M.Map Text (V -> AOI V))
aoiGetPowers = AOI $ \ cx -> (cx, (Right . aoi_power) cx)

aoiGetState :: AOI AOI_STATE
aoiGetState = AOI $ \ cx -> 
    let st = (hls_state . aoi_state) cx in
    return (cx, Right st)

aoiPutState :: AOI_STATE -> AOI ()
aoiPutState s = AOI $ \ cx ->
    let hls' = hls_put s (aoi_state cx) in
    let cx' = cx { aoi_state = hls' } in
    return (cx', Right ())

aoiModifyState :: (AOI_STATE -> AOI_STATE) -> AOI ()
aoiModifyState f = aoiGetState >>= aoiPutState . f

pushFrame :: Text -> AOI ()
pushFrame = aoiModifyState . addFrame where
    addFrame txt s = s { aoi_frames = txt:(aoi_frames s) }

popFrame :: AOI ()
popFrame =
    aoiGetState >>= \ s ->
    case aoi_frames s of
        [] -> fail "debug frames underflow"
        (_:fs) -> aoiPutState $ s { aoi_frames = fs }

aoiSourceFromArgs :: IO (Either [Import] FS.FilePath)
aoiSourceFromArgs = 
    Env.getArgs >>= \ args ->
    let textArgs = L.map T.pack args in
    let aoSuffix = T.pack ".ao" in
    let aoFileArgs = L.filter (aoSuffix `T.isSuffixOf`) textArgs in
    case aoFileArgs of
        [] -> Left [T.pack "aoi"]
        (f:[]) -> 
            FS.canonicalizePath (FS.fromText f) >>= \ fc ->
            return (Right fc)
        _ -> fail ("args not understood: " ++ show args)

randomBytes :: Int -> IO ByteString
randomBytes n = liftM toBytes CR.makeSystem where 
    toBytes = B.pack . L.take n . randoms

-- AOI context secret (in case of open systems)
newSecret :: IO Text
newSecret = liftM toText (randomBytes 18) where
    toText = T.decodeUtf8 . B64.encode

pb :: Text -> V
pb secret = B (BT True True) (ABC [Invoke (T.cons '!' secret)])

-- initial environment, given powerblock
initEnvI :: Text -> V
initEnvI p = s `P` (h `P` (p `P` (rns `P` ex))) where
    s = U -- initial stack
    h = U -- initial hand
    rns = (toABCV T.empty) `P` U -- record of named stacks
    ex = U -- unused (available for user extensions)
    
aoiLoadDict :: Either [Import] FS.FileName -> IO ([Error], DictC)
aoiLoadDict src = 
    either importDictC loadDictC src >>= \ (errors,dictC) ->
    mapM_ (printErrLn . T.unpack) errors >>
    return dictC

-- load dictionary from file; print errors
--  (AOI word 'reload' will also repeat this)
aoiReload :: AOI ()
aoiReload = source >>= load >>= set where
    source = liftM aoi_source aoiGetState
    load = aoiIOAction . aoiLoadDict 
    set dc = AOI $ \ cx ->
        let cx' = cx { aoi_dict = dc } in
        return (cx', ())

-- 
newDefaultContext :: IO AOI_CONTEXT
newDefaultContext =
    newSecret >>= \ secret ->
    aoiSourceFromArgs >>= \ source ->
    aoiLoadDict source >>= \ dictC ->
    let s0 = AOI_STATE
            { aoi_frames = [T.pack "aoi"]
            , aoi_text = T.empty -- nothing to do
            , aoi_envU = initEnvU
            , aoi_envI = initEnvI secret
            , aoi_ifn = ABC [Invoke (T.singleton 'i')]
            , aoi_ifn_bt = BT False False
            }
    in
    let cx0 = AOI_CONTEXT
            { aoi_dict = dictC
            , aoi_secret = secret
            , aoi_power = aoiDefaultPower
            , aoi_source = source
            , aoi_state = hls_init s0
            }
    in
    return cx0

main :: IO ()
main = 
    newDefaultContext >>= \ cx ->
    Sys.putStrLn "okay, it compiles"



-- states with exponential decay of history. 
-- defaults to a halflife of 6 steps.
data HLS s = HLS 
    { hls_halflife :: Double
    , hls_rgen     :: R.StdGen
    , hls_state    :: s
    , hls_hist     :: [s]
    , hls_join     :: s {-newer-} -> s {-older-} -> s {-joined-}
    }

hls_init :: s -> HLS s
hls_init s0 = HLS
    { hls_halflife = 6.283185 -- arbitrary (~2π) default halflife
    , hls_rgen = mkStdGen 8675309
    , hls_state = s
    , hls_hist = []
    , hls_join = const
    }

hls_getHist :: HLS s -> [s]
hls_getHist hls = hls_state hls : hls_hist hls

hls_put :: s -> HLS s -> HLS s
hls_put s hls =
    let (r,g') = R.random (hls_rgen hls) in
    let n = negate $ round $ log (1.0 - r) * hls_halflife hls in
    let h' = dropHLS (hls_join hls) n (hls_state hls : hls_hist hls) in
    hls { hls_rgen = g', hls_state = s, hls_hist = h' }

-- dropHLS will join two items in a list at the given position
-- (or, if that position does not exist, the original list is returned)
dropHLS :: (s -> s -> s) -> Integer -> [s] -> [s]
dropHLS jf n (sNewer:sOlder:ss) | (n < 1) = jf sNewer sOlder : ss

dropHLS _ _ [] = []
dropHLS _ _ l@(x:[]) = l
dropHLS jf n (s:l@(s':ss)) | n > 0 = s : dropHLS jf (n-1) l
                           | otherwise = jf s s' : ss



