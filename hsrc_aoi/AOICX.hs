{-# LANGUAGE PatternGuards #-}

-- data structures and context for AOI
module AOICX
    ( AOI_CONTEXT(..), IFN(..), AOI, runAOI
    , Error(..), Power
    , defaultContext, defaultEnv, defaultPB
    , getCX, putCX, modCX
    , defaultIFN, aoiGetIfn, aoiPutIfn
    , aoiGetStep, aoiPushStep
    , aoiReload
    ) where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Trans.State.Strict as MT
import qualified Control.Monad.Trans.Error as MT 
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Sequence as S
import qualified Text.Parsec as P
import Text.Parsec.Text()

import AO.AOTypes
import AO.V
import AO.ParseAO
import AO.AO
import AO.ABC
import HLS

newtype Error = E Text
type StepState = (Integer, V AOI)
type Power = V AOI -> AOI (V AOI)

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
    , aoi_powers  :: !(M.Map Text Power) -- provide powers to powerblock
    , aoi_step    :: !(HLS StepState) -- recovery values 
    , aoi_ifn     :: !IFN 
    }

defaultContext :: AOI_CONTEXT
defaultContext = AOI_CONTEXT
    { aoi_dict = M.empty
    , aoi_powers = M.empty
    , aoi_source = T.pack "aoi"
    , aoi_step = hls_init (0, defaultEnv)
    , aoi_ifn = defaultIFN
    }

-- standard environment
defaultEnv :: V AOI
defaultEnv = env where
    s = U
    h = U
    pb = defaultPB
    sn = textToVal T.empty -- initial stack name
    ns = U -- no named stacks
    ex = U
    env = (P s (P h (P pb (P (P sn ns) ex))))

defaultPB :: V AOI
defaultPB = B (KF False False) abc where
    abc = ABC 
        { abc_code = (S.singleton . Invoke . T.pack) "~power~"
        , abc_comp = invokePower 
        }

invokePower, powerFail :: V AOI -> AOI (V AOI)
invokePower v@(P vLbl vMsg) =
    (aoi_powers <$> getCX) >>= \ mPowers ->
    case valToText vLbl of
        Nothing -> powerFail v
        Just label -> case M.lookup label mPowers of
            Nothing -> powerFail v 
            Just power -> liftM (P defaultPB) $ power vMsg
invokePower v = powerFail v
powerFail v = fail $ "message not understood: {~power~} @ " ++ show v

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
    let wNeed = aoWordsRequired actions in
    let wMissed = Set.filter (`M.notMember` dc) wNeed in
    if Set.null wMissed
        then return $ Right $ compileABC cx $ aoToABC dc actions
        else return $ Left $ 
            T.pack "undefined words: " `T.append` 
            T.unwords (Set.toList wMissed)

compileABC :: AOI_CONTEXT -> S.Seq Op -> ABC AOI
compileABC _ ops = 
    ABC { abc_code = ops
        , abc_comp = runABC invNull ops } 

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
    liftIO (loadDictionary (aoi_source cx)) >>= \ dictAO_pre ->
    let dictAO = preCompile cx dictAO_pre in
    let dc0 = compileDictionary dictAO in
    let dcf = postCompile cx dc0 in
    let cx' = cx { aoi_dict = dcf } in
    putCX cx'

-- OPPORTUNITY FOR OPTIMIZATIONS
preCompile :: AOI_CONTEXT -> Dictionary -> Dictionary
preCompile = flip const

-- view simplified code
postCompile :: AOI_CONTEXT -> DictC -> DictC
postCompile = const $ M.map simplifyABC


