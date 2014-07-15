{-# LANGUAGE PatternGuards #-}

-- | AO's 'inline everything' semantics are simple for reasoning, but
-- not efficient. It makes poor reuse of memory, CPU cache, separate
-- compilation, and bandwidth. To mitigate this, AO systems leverage
-- ABC's separate compilation and linking model. Any sequence of ABC
-- can be given a cryptographically unique resource token, which may
-- then be invoked to logically inline the associated ABC.
--
-- See module ABC.Resource for more about the resource model. 
--
-- Long term, the idea is that we should use machine learning to chop
-- large ABC programs into highly reusable, near-optimal components. 
--
-- But this module is concerned with a short term solution. A subset
-- of words in the AO dictionary will be treated as ABC resources by
-- replacing their definitions with the appropriate invocation. The
-- ABC resources may truly be compiled separately for performance.
-- 
-- The selection of words for compilation is based on convention. At
-- this time, we simply compile every word `foo` for which there is
-- a word `compile!foo` defined in the dictionary. This is rather
-- ad-hoc, but it will do the job well enough for now.
--
-- TODO: eventually, I need to support sensitivity concerns, i.e. to
-- compile sensitive ABC modules using an unguessable secret to guard
-- against confirmation attacks.
--
module AO.Precompile 
    ( preCompileDict
    , PreCompD, SecStoreD
    ) where

import Control.Applicative
import Control.Monad.Trans.State
import Control.Arrow (first)

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

import AO.Code
import AO.Compile
import AO.InnerDict

import ABC.Operators
import ABC.Resource
import ABC.Quote
import ABC.Simplify

type PreCompD  = M.Map ResourceToken [Op] -- access precompiled words
type SecStoreD = M.Map HashCT [CipherText]  -- secure store for resources

data PCX md = PCX
    { pcx_dict :: M.Map Word (AO_Code, md)
    , pcx_pcw  :: M.Map Word ResourceToken
    , pcx_prcd :: PreCompD
    , pcx_secd :: SecStoreD
    }

-- | precompile all words for which `compile!word` is defined
-- 
-- Precompiled code is emitted as a map of abcResourceToken values 
-- to Awelon bytecode, pre-simplified but otherwise unmodified.
preCompileDict :: AODict md -> (AODict md, (PreCompD, SecStoreD))
preCompileDict (AODict d) = toResult $ execState runPreCompile s0 where
    toResult s = (AODict (pcx_dict s), (pcx_prcd s, pcx_secd s))
    s0 = PCX d M.empty M.empty M.empty  



runPreCompile :: State (PCX md) ()
runPreCompile = gets (M.keys . pcx_dict) >>= mapM_ preComp

needPreComp :: Word -> State (PCX md) Bool
needPreComp w =
    let cw = T.pack "compile!" `T.append` w in
    get >>= \ s ->
    let bWantPreComp = M.member cw (pcx_dict s) in
    let bAlreadyDone = M.member w (pcx_pcw s) in 
    return (bWantPreComp && not bAlreadyDone)

-- precompile words if doing so is relevant.
preComp :: Word -> State (PCX md) ()
preComp w =
    needPreComp w >>= \ b ->
    if not b then return () else
    gets pcx_dict >>= \ d ->
    let code = fst (d M.! w) in
    simplify <$> (aoCodeToABC code) >>= \ ops ->
    makeResource storeSecD ops >>= \ tok ->
    updateDict w tok ops

storeSecD :: HashCT -> CipherText -> State (PCX md) ()
storeSecD k ct = modify fnAdd where
    fnAdd pcx = pcx { pcx_secd = M.alter fnAddL k (pcx_secd pcx) }
    fnAddL Nothing = Just [ct]
    fnAddL (Just cts) | L.elem ct cts = Just cts -- duplicate
                      | otherwise     = Just (ct:cts) -- 192 bit hash collision

updateDict :: Word -> ResourceToken -> [Op] -> State (PCX md) ()
updateDict w tok ops = modify $ \ s ->
    let dict' = M.adjust (first $ const [AO_Tok tok]) w (pcx_dict s) in
    let pcw'  = M.insert w tok (pcx_pcw s) in
    let prcd' = M.insert tok ops (pcx_prcd s) in
    let secd' = pcx_secd s in
    PCX dict' pcw' prcd' secd'

-- obtain partially precompiled bytecode for an ABC program
aoCodeToABC :: [AO_Action] -> State (PCX md) [Op]
aoCodeToABC (op:ops) = (++) <$> aoActionToABC op <*> aoCodeToABC ops
aoCodeToABC [] = return []

-- obtain bytecode, translating precompiled words to tokens
aoActionToABC :: AO_Action -> State (PCX md) [Op]
aoActionToABC (AO_Word w) =
    preComp w >> -- precompile word (if necessary)
    gets pcx_dict >>= \ d ->
    aoCodeToABC (fst (d M.! w))
aoActionToABC (AO_Block aoOps) = aoCodeToABC aoOps >>= \ ops -> return [BL ops]
aoActionToABC (AO_Num r) = return $ quotes r [Op_l]
aoActionToABC (AO_Text txt) = return $ quotes (TL txt) [Op_l]
aoActionToABC (AO_ABC aOp) = return [aopToABC aOp]
aoActionToABC (AO_Tok tok) = return [Tok tok]
