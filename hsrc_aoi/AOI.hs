{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances #-}

-- | AOI describes a simplistic, imperative REPL for language AO.
-- AOI will start by importing the "aoi" dictionary unless a .ao 
-- file is specified on the command line. Developers cannot modify
-- definitions from within aoi. However, they may tweak original 
-- files and use 'reload'.
--
--   TODO: leverage haskeline, possibly ansi-terminal
--         consider support for acid-state persistent sessions
-- 
-- AOI has a trivial effects model documented in the standard aoi 
-- dictionary file. AOI also enables reprogramming the interpreter
-- from within, with the 'switchAOI' word and command.
--
-- AOI does keep track of historical states, using an exponential
-- decay algorithm. In addition, it keeps track of frames in the
-- current state. Between these attributes, AOI should be decent 
-- for debugging (providing both a stack trace and a history). 
--
-- Note: AOI does not represent the intended vision for AO interactive
-- programming. AOI is simply a stopgap for pre-bootstrap testing.
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

-- AOI_CONTEXT holds mutable state plus runtime constants
data AOI_CONTEXT = AOI_CONTEXT 
    { aoi_dict    :: DictC  -- loaded dictionary
    , aoi_secret  :: Text   -- secret held by powerblock
    , aoi_power   :: M.Map Text (V -> AOI V) -- common powers
    , aoi_source  :: Either [Import] FS.FilePath -- for reloads
    , aoi_frames  :: HLS [Text] -- stack frame history (for debugging)
    , aoi_steps   :: HLS V -- starting values history (for recovery) 
    , aoi_ifn     :: IFN 
    }

type Error = Text
newtype AOI a = AOI { runAOI :: AOI_CONTEXT -> IO (AOI_CONTEXT, Either Error a) }

instance Monad AOI where
    return a = AOI $ \ cx -> (cx, return (Right a))
    (>>=) m f = AOI $ \ cx ->
        runAOI m cx >>= \ r ->
        case snd r of
            Left e -> return r
            Right a -> runAOI (f a) (fst r)
    fail msg = AOI $ \ cx -> return (cx, Left (T.pack msg))

-- AOI supports a reprogrammable interpreter to support bootstrap.
-- The interpreter is really an incremental compiler of rough type:
--
--    (text * eIC) -> (error + ([eU -> eU'] * eIC')
-- 
-- This is probably very inefficient, but it's useful. The 
data IFN = IFN
    { ifn_action :: ABC
    , ifn_bt     :: BT
    , ifn_eIC    :: V
    }

defaultIFN :: IFN
defaultIFN = IFN abc btRel U where
    abc = ABC [Invoke (T.singleton 'i')]
    btRel = BT { bt_rel = True, bt_aff = False }

instance ToABCV IFN where 
    toABCV ifn = B (ifn_bt ifn) (ifn_action ifn) `P` ifn_eIC ifn
instance FromABCV IFN where
    fromABCV (P (B bt act) eIC) = Just $ IFN act bt eIC
    fromABCV _ = Nothing


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
        Just ('!',s) -> aoiTrySecret s (aoiCommand v)
        Just ('i',m) | T.null m -> aoiInterperet v
        _ -> fail ("unknown invocation {" ++ T.unpack t ++ "}")

-- aoi ignores most annotations, except for frames
aoiAnno :: Text -> AOI ()
aoiAnno t = 
    case T.uncons t of
        Just('@',f) | f == fDrop -> popFrame
                    | otherwise -> pushFrame frame
        _ -> return ()
    where fDrop = T.singleton '-'

-- aoiInterpret corresponds to an ABC function of type:
--    (text * eIC) -> (errorText + ((eU -> eU') * eIC)
-- consequently, parse failures are reported through the
-- in-band error result, but other errors are reported out of band.
aoiInterpret :: V -> AOI V
aoiInterpret iInput = case iInput of
    Nothing -> fail "AOI interpreter requires (text * env) pair!"
    Just (text, eIC) -> readAO text >>= either parseErr build
  where
    parseErr = toABCV . Left . T.pack . show
    build ao = aoiGetDict >>= \ dc -> either berr bok (compileAO dc ao)
    bok = B (BT { bt_rel = False, bt_aff = False }) 
    missingWords =  T.pack "WORDS NOT UNDERSTOOD:"
    berr = toABCV . Left . T.unwords . (missingWords:) . L.nub

-- execute only if a secret matches, fail otherwise
aoiTrySecret :: Text -> AOI a -> AOI a 
aoiTrySecret s action = 
    aoiGetSecret >>= \ secret ->
    let badMsg = "invalid secret " ++ T.unpack s in
    if (s /= secret) then fail badMsg else
    action

aoiCommand :: V -> AOI V
aoiCommand msg = case fromABCV msg of
    Nothing -> fail badCommand
    Just (label, message) -> 
        aoiDispatch label message >>= \ response ->
        aoiGetSecret >>= \ secret ->
        secret `seq` 
        return (pb secret `P` response)

aoiDispatch :: Text -> V -> AOI V
aoiDispatch label msg =
    aoiGetPowers >>= \ mpw ->
    case M.lookup label mpw of
        Nothing -> fail ("unknown action: " ++ T.unpack label)
        Just action -> action msg

aoiGetSecret :: AOI Text
aoiGetSecret = AOI $ \ cx -> return (cx, (Right . aoi_secret) cx)

aoiGetPowers :: AOI (M.Map Text (V -> AOI V))
aoiGetPowers = AOI $ \ cx -> return (cx, (Right . aoi_power) cx)

aoiGetDict :: AOI DictC
aoiGetDict = AOI $ \ cx -> (cx, (Right . aoi_dict) cx)

aoiGetFrames :: AOI [Text]
aoiGetFrames = AOI $ \ cx -> (cx, (Right . hls_state . aoi_frames) cx)

aoiPutFrames :: [Text] -> AOI ()
aoiPutFrames fs = AOI $ \cx -> 
    let s' = hls_put fs (aoi_frames cx) in
    s' `seq` cx { aoi_frames = s' }

pushFrame :: Text -> AOI ()
pushFrame text = aoiGetFrames >>= aoiPutFrames . (text :)

popFrame :: AOI ()
popFrame =
    aoiGetFrames >>= \ fs ->
    case fs of
        [] -> fail "debug frames underflow"
        (_:fs) -> aoiPutFrames fs
    
randomBytes :: Int -> IO ByteString
randomBytes n = liftM toBytes CR.makeSystem where 
    toBytes = B.pack . L.take n . randoms

-- AOI context secret (in case of open systems)
newSecret :: IO Text
newSecret = liftM toText (randomBytes 18) where
    toText = T.decodeUtf8 . B64.encode

-- AOI powerblock from a secret
pb :: Text -> V
pb secret = B (BT True True) (ABC [Invoke (T.cons '!' secret)])

-- load a specified dictionary    
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

-- default interpreter for code in ABC is just [{i}]
-- this will invoke the AO to ABC compiler using the AO dictionary
-- (Developers cannot define new words; all edits must be performed
-- via the filesystem, using "reload" if needed.)
defaultInterpreter :: V
defaultInterpreter = B bt (ABC [invokeI]) where
    bt = BT { bt_rel = True, bt_aff = False }
    invokeI = Invoke (T.singleton 'i')

-- 
newDefaultContext :: IO AOI_CONTEXT
newDefaultContext =
    newSecret >>= \ secret ->
    aoiSourceFromArgs >>= \ source ->
    aoiLoadDict source >>= \ dictC ->
    let s = U in -- initial stack
    let h = U in -- initial hand
    let p = pb secret in -- initial powerblock
    let rns = (T.empty, U) in -- record of named stacks
    let ex = U in -- extension area
    let eU0 = (s, (h, (p, (rns, ex)))) in
    let cx0 = AOI_CONTEXT 
            { aoi_dict = dictC
            , aoi_secret = secret
            , aoi_power = defaultPower
            , aoi_source = source
            , aoi_frames = hls_init []
            , aoi_steps = hls_init (toABCV eU0) 
            , aoi_ifn = defaultIFN
            }
    in
    return cx0

main :: IO ()
main = 
    newDefaultContext >>= \ cx ->
    putErrLn (T.unpack (aoi_secret cx)) >>
    return ()

-- states with exponential decay of history
data HLS s = HLS 
    { hls_halflife :: Double 
    , hls_rgen     :: R.StdGen
    , hls_state    :: s
    , hls_hist     :: [s]
    , hls_join     :: s {-newer-} -> s {-older-} -> s {-joined-}
    }

-- defaults to a halflife of 100 steps.
hls_init :: s -> HLS s
hls_init s0 = HLS
    { hls_halflife = 100 -- arbitrary default halflife
    , hls_rgen = mkStdGen 8675309
    , hls_state = s
    , hls_hist = []
    , hls_join = const
    }

hls_getHist :: HLS s -> [s]
hls_getHist hls = hls_state hls : hls_hist hls

-- hls_put is strict in state and history
hls_put :: s -> HLS s -> HLS s
hls_put s hls =
    let (r,g') = R.random (hls_rgen hls) in
    let n = negate $ round $ log (1.0 - r) * hls_halflife hls in
    let h' = dropHLS (hls_join hls) n (hls_state hls : hls_hist hls) in
    h' `seq` s `seq`
    hls { hls_rgen = g', hls_state = s, hls_hist = h' }

hls_modify :: (s -> s) -> HLS s -> HLS s
hls_modify f hls = hls_put (f (hls_state hls)) hls 

-- dropHLS will join two items in a list at the given position
-- This operation is strict in the join function because it is
-- important to avoid a memory leak. 
dropHLS :: (s -> s -> s) -> Integer -> [s] -> [s]
dropHLS _ _ [] = []
dropHLS _ _ (x:[]) = (x:[])
dropHLS jf n (sNewer:sOlder:ss) 
    | (n < 1)   = let s' = jf sNewer sOlder in s' `seq` (s':ss)
    | otherwise = let ss' = dropHLS jf (n-1) ss in ss' `seq` (s:ss')
                  

dropHLS jf n (s:l@(s':ss)) | n > 0 = s : dropHLS jf (n-1) l
                           | otherwise = jf s s' : ss



