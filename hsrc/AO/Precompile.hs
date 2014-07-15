
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
    , PreCompD 
    ) where

import Control.Applicative
import Control.Monad.Trans.State

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

type HashString = String
type PreCompD = M.Map HashString [Op] -- hash string to operators
type InnerD md = M.Map Word (AO_Code, md) -- original or final code
type PCX = (M.Map Word HashString, M.Map HashString [Op])

-- | precompile all words for which `compile!word` is defined
-- 
-- Precompiled code is emitted as a map of abcResourceToken values 
-- to Awelon bytecode, pre-simplified but otherwise unmodified.
preCompileDict :: AODict md -> (AODict md, PreCompD)
preCompileDict = flip evalState (M.empty,M.empty) . runPreCompile

isPCW :: M.Map Word a -> Word -> Bool
isPCW d w = M.member cw d where
    cw = T.pack "compile!" `T.append` w

runPreCompile :: AODict md -> State PCX (AODict md, PreCompD)
runPreCompile (AODict d0) =
    let lTargets = L.filter (isPCW d0) $ M.keys d0 in
    mapM_ (preComp d0) lTargets >> -- accumulates in state
    get >>= \ (hsWords,preCompD) ->
    let df = foldr updateWord d0 (M.toList hsWords) in
    return (AODict df, preCompD)

updateWord :: (Word, HashString) -> InnerD md -> InnerD md
updateWord (w,hs) = M.update fn w where
    fn (_,meta) = pure (code',meta)
    code' = [AO_Tok hs] -- single token

-- compile a word to a hash string
preComp :: InnerD md -> Word -> State PCX HashString
preComp d w =
    gets fst >>= \ pcWords ->
    case M.lookup w pcWords of
        Just hs -> return hs
        Nothing ->
            let code = fst (d M.! w) in
            simplify <$> (aoCodeToABC d code) >>= \ ops ->
            -- todo: modify this to more directly construct
            --  both encrypted and decrypted storage for tokens.
            let hs = abcResourceToken ops in
            get >>= \ (mW,mHS) ->
            put (M.insert w hs mW, M.insert hs ops mHS) >>
            return hs

-- obtain partially precompiled bytecode for an ABC program
aoCodeToABC :: InnerD md -> [AO_Action] -> State PCX [Op]
aoCodeToABC d (op:ops) = (++) <$> aoActionToABC d op <*> aoCodeToABC d ops
aoCodeToABC _ [] = return []

-- obtain bytecode, translating precompiled words to tokens
aoActionToABC :: InnerD md -> AO_Action -> State PCX [Op]
aoActionToABC d (AO_Word w) | isPCW d w = preComp d w >>= \ hs -> return [Tok ('#':hs)] 
                            | otherwise = aoCodeToABC d (fst (d M.! w))
aoActionToABC d (AO_Block aoOps) = aoCodeToABC d aoOps >>= \ ops -> return [BL ops]
aoActionToABC _ (AO_Num r) = return $ quotes r [Op_l]
aoActionToABC _ (AO_Text txt) = return $ quotes (TL txt) [Op_l]
aoActionToABC _ (AO_ABC aOp) = return [aopToABC aOp]
aoActionToABC _ (AO_Tok tok) = return [Tok tok]
