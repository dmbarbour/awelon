{-# LANGUAGE PatternGuards #-}

-- data structures and context for AOI
module AOICX
    ( AOI_CONTEXT(..), IFN(..), AOI, runAOI
    , Error(..), Frame, FrameHist
    , CompilerConfig(..), compileActions
    , getCX, putCX, modCX
    , pushFrame, popFrame, popFrame', getFrameHist
    , setFrameHist, resetFrameHist, initialFrameHist
    , defaultIFN, aoiGetIfn, aoiPutIfn
    , aoiGetStep, aoiPushStep
    , aoiReload
    ) where

import Control.Applicative
import qualified Control.Monad.Trans.State.Strict as MT
import qualified Control.Monad.Trans.Error as MT 
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.IORef 
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
--import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as S
--import qualified Data.List as L
import qualified Text.Parsec as P
import Text.Parsec.Text()

import AO.AOTypes
import AO.V
import AO.ParseAO
import AO.AO
import AO.ABC
import HLS

newtype Error = E Text
type Frame = Text
type StepState = (Integer, V AOI)
type FrameHist = HLS [Frame]

instance MT.Error Error where strMsg = E . T.pack 

type AOI_STATE = MT.StateT AOI_CONTEXT IO
type AOI = MT.ErrorT Error AOI_STATE

runAOI :: AOI a -> AOI_CONTEXT -> IO (Either Error a , AOI_CONTEXT)
runAOI op cx = MT.runStateT (MT.runErrorT op) cx

liftCX :: AOI_STATE a -> AOI a
liftCX = lift

getCX :: AOI AOI_CONTEXT
getCX = liftCX MT.get

putCX :: AOI_CONTEXT -> AOI ()
putCX = liftCX . MT.put

modCX :: (AOI_CONTEXT -> AOI_CONTEXT) -> AOI ()
modCX = liftCX . MT.modify


-- AOI_CONTEXT holds the state for running AO.
--
-- Frames are kept in an IORef for two reasons. First, so they remain
-- accessible after ctrl+c. Second, because they change rapidly compared
-- to other state and it's more efficient to handle them this way.
--
data AOI_CONTEXT = AOI_CONTEXT 
    { aoi_dict    :: !DictC  -- loaded and compiled dictionary
    , aoi_source  :: !Import -- for reloading
    , aoi_cconf   :: !CompilerConfig
    , aoi_frames  :: !(IORef FrameHist) -- stack frames + history
    , aoi_step    :: !(HLS StepState) -- recovery values 
    , aoi_ifn     :: !IFN 
    }

data CompilerConfig = CompilerConfig
    { cc_frame_enable :: Bool
    }


-- AOI supports a reprogrammable interpreter to support bootstrap.
-- The interpreter is really an incremental compiler of rough type:
--
--    (text * eIC) -> (error + ([eU -> eU'] * eIC'))
-- 
-- This is probably very inefficient, the user isn't writing much
-- or often. 
data IFN = IFN
    { ifn_op  :: !(ABC AOI)
    , ifn_eIC :: !(V AOI)
    }

aoiGetIfn :: AOI IFN
aoiGetIfn = aoi_ifn <$> getCX

aoiPutIfn :: IFN -> AOI ()
aoiPutIfn ifn = modCX put where
    put cx = cx { aoi_ifn = ifn }

defaultIFN :: IFN
defaultIFN = IFN abc U where
    abc = ABC { abc_code = falseCode, abc_comp = defaultI }
    falseCode = (S.singleton . Invoke . T.pack) "defaultInterpreter"

defaultI :: V AOI -> AOI (V AOI)
defaultI v@(P userInput eIC) =
    case valToText userInput of
        Nothing -> typeErrI v
        Just txt -> case P.parse parseAODef "" txt of
            Left pe -> (return . L . textToVal . T.pack . show) pe
            Right actions -> 
                compileActions actions >>= \ result ->
                case result of
                    Left err -> return (L (textToVal err))
                    Right abc -> return (R (P (B kf0 abc) eIC))
-- fail for any other input                    
defaultI v = typeErrI v

typeErrI :: V AOI -> AOI error
typeErrI v = fail $ msg ++ show v where
    msg = "interpreter expects (text * U); got: "

compileActions :: AODef -> AOI (Either Text (ABC AOI))
compileActions actions = 
    getCX >>= \ cx -> 
    let dc = aoi_dict cx in
    let cc = aoi_cconf cx in
    let wNeed = aoWordsRequired actions in
    let wMissed = Set.filter (`M.notMember` dc) wNeed in
    if Set.null wMissed
        then return $ Right $ compileABC cc $ aoToABC dc actions
        else return $ Left $ 
            T.pack "undefined words: " `T.append` 
            T.unwords (Set.toList wMissed)

compileABC :: CompilerConfig -> S.Seq Op -> ABC AOI
compileABC _ ops = 
    ABC { abc_code = ops
        , abc_comp = runABC invokeAOI ops } 

-- invokeAOI is called only for annotations. The powerblock and
-- default interpreter are provided as ABC precompiled blocks,
-- so are not invoked this way.
invokeAOI :: Text -> V AOI -> AOI (V AOI)
invokeAOI tok =
    case T.uncons tok of
        Just ('&',anno) -> aoiAnno anno 
        _ -> const $ illegalToken tok

illegalToken :: Text -> AOI e
illegalToken tok = fail $ "illegal token: {" ++ T.unpack tok ++ "}"

aoiAnno :: Text -> a -> AOI a
aoiAnno tok =
    case T.uncons tok of
        Just ('@', frame) -> aoiFrame frame
        _ -> return -- no other annotations are supported yet

aoiFrame :: Text -> a -> AOI a
aoiFrame frame =
    case T.uncons frame of
        Just ('-', ftxt) ->
            if T.null ftxt then \ v -> popFrame >> return v
                           else \ v -> popFrame' ftxt >> return v
        _ -> \ v -> pushFrame frame >> return v

popFrame :: AOI ()
popFrame = 
    (aoi_frames <$> getCX) >>= \ fr ->
    liftIO (readIORef fr) >>= \ hls ->
    case hls_get hls of
        (_:fs) -> liftIO (writeIORef fr (hls_put fs hls))
        _ -> fail "popped empty stack!"

popFrame' :: Text -> AOI ()
popFrame' target =
    (aoi_frames <$> getCX) >>= \ fr ->
    liftIO (readIORef fr) >>= \ hls ->
    case hls_get hls of
        (f:fs) | (f == target) ->
            liftIO (writeIORef fr (hls_put fs hls))
        _ -> fail ("failed to pop " ++ T.unpack target ++ " from stack!")

pushFrame :: Text -> AOI ()
pushFrame frame =
    (aoi_frames <$> getCX) >>= \ fr ->
    liftIO (readIORef fr) >>= \ hls ->
    let fs = hls_get hls in
    let hls' = hls_put (frame : fs) hls in
    hls' `seq` liftIO (writeIORef fr hls')

-- obtain the current frame history (for debugging)
getFrameHist :: AOI (HLS [Frame])
getFrameHist = (aoi_frames <$> getCX) >>= liftIO . readIORef

-- clear the current frame history (e.g. between steps)
setFrameHist :: HLS [Frame] -> AOI ()
setFrameHist hist = 
    (aoi_frames <$> getCX) >>= \ fr ->
    liftIO (writeIORef fr hist)

resetFrameHist :: AOI () 
resetFrameHist = setFrameHist initialFrameHist

initialFrameHist :: FrameHist
initialFrameHist = hls_init []

-- manipulate step values
aoiGetStep :: AOI StepState
aoiGetStep = (hls_get . aoi_step <$> getCX) 

aoiPushStep :: V AOI -> AOI ()
aoiPushStep v = modCX put where
    put cx = 
        let hls = aoi_step cx in
        let stepCt = fst (hls_get hls) in
        let stepCt' = 1 + stepCt in
        stepCt' `seq` v `seq` 
        let hls' = hls_put (stepCt', v) hls in
        cx { aoi_step = hls' }

aoiReload :: AOI ()
aoiReload = 
    getCX >>= \ cx ->
    let cc = aoi_cconf cx in
    liftIO (loadDictionary (aoi_source cx)) >>= \ dictAO_pre ->
    let dictAO = preCompile cc dictAO_pre in
    let dc0 = compileDictionary dictAO in
    let dcf = postCompile cc dc0 in
    let cx' = cx { aoi_dict = dcf } in
    putCX cx'

-- OPPORTUNITY FOR OPTIMIZATIONS
preCompile :: CompilerConfig -> Dictionary -> Dictionary
preCompile = const id

-- go ahead and simplify code (it's nicer to see it's
-- simplified form in the REPL).
postCompile :: CompilerConfig -> DictC -> DictC
postCompile = const $ M.map simplifyABC


{-
-- preCompile operates on the AO code.
preCompile :: CompilerConfig -> Dictionary -> Dictionary
preCompile cc dict0 = dictf where
    framedDict = autoFrame dict0
    autoFrame | not (cc_frame_enable cc) = id
              | otherwise = M.mapWithKey (frameWrap cc)  
    dictf = framedDict 


-- for now, wrap a word unless it consists of pure data shuffling
-- around other words. 
frameWrap :: CompilerConfig -> W -> (Locator, AODef) -> (Locator, AODef)
frameWrap _ w (loc,def) = (loc,framedDef) where
    locTxt = wordLocatorText w loc
    annoLoc = Prim . S.singleton . Invoke . T.cons '&' . T.cons '@'
    enterFrame = annoLoc locTxt
    exitFrame = annoLoc (T.singleton '-')
    framedDef = 
        if shuffleDef def then def else 
        enterFrame S.<| (def S.|> exitFrame)

-- pure data shuffling is not framed because it costs too much
-- by 'pure data shuffling' I'm basically going to include everything
-- that is part of the 'fixpoint' function.
-- (and hinders too many downstream optimizations!)
shuffleDef :: S.Seq Action -> Bool
shuffleDef = S.null . S.dropWhileL shuffleAction

shuffleAction :: Action -> Bool
shuffleAction (Word _) = True
shuffleAction (Num _) = True
shuffleAction (Lit _) = True
shuffleAction (BAO _) = True
shuffleAction (Prim ops) = shuffleABC ops
shuffleAction (Amb options) = all shuffleDef options

shuffleABC :: S.Seq Op -> Bool
shuffleABC = S.null . S.dropWhileL shuffleOp


shuffleOp :: Op -> Bool
shuffleOp (Op c) = L.elem c " \nlrwzvc^%'$o"
shuffleOp (TL _) = True
shuffleOp (BL _) = True
shuffleOp (Invoke text) =
    case T.uncons text of 
        Just ('&', _) -> True
        _ -> False
shuffleOp (AMBC options) = all shuffleABC options
-}

