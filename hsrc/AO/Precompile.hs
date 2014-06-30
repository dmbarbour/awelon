
-- | This is an idea for partially pre-compiling the AO dictionary
-- by recognizing words starting with `#` for separate compilation
-- and linking. This enables a very flexible mix of compiled code
-- together with interpreted code.
--
-- Words such as #foo have their definitions modified to invocation:
--
--    {#secureHashOfBytecode}
-- 
-- However, this occurs in a recursive manner, such that if #foo uses
-- #bar, #foo will at some point inline the secure hash used in #bar.
--
-- This module only compiles these words down to the Awelon bytecode,
-- and minimally modifies the dictionary. Further compilation of the
-- target words is necessary to achieve any real benefit from it.
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
import ABC.Hash
import ABC.Quote
import ABC.Simplify

type HashString = String
type PreCompD = M.Map HashString [Op] -- hash string to operators
type InnerD md = M.Map Word (AO_Code, md) -- original or final code
type PCX = (M.Map Word HashString, M.Map HashString [Op])

-- | precompile all words whose names start with `#`.
-- 
-- Precompiled code is emitted as a map of abcHash values to
-- Awelon bytecode, pre-simplified but otherwise unmodified.
preCompileDict :: AODict md -> (AODict md, PreCompD)
preCompileDict = flip evalState (M.empty,M.empty) . runPreCompile

-- find words starting with '#'
isPCW :: Word -> Bool
isPCW = maybe False ((== '#') . fst) . T.uncons

runPreCompile :: AODict md -> State PCX (AODict md, PreCompD)
runPreCompile (AODict d0) =
    let lTargets = L.filter isPCW $ M.keys d0 in
    mapM_ (preComp d0) lTargets >> -- accumulates in state
    get >>= \ (hsWords,preCompD) ->
    let df = foldr updateWord d0 (M.toList hsWords) in
    return (AODict df, preCompD)

updateWord :: (Word, HashString) -> InnerD md -> InnerD md
updateWord (w,hs) = M.update fn w where
    fn (_,meta) = pure (code',meta)
    code' = [AO_Tok ('#':hs)] -- single token

-- compile a word to a hash string
preComp :: InnerD md -> Word -> State PCX HashString
preComp d w =
    gets fst >>= \ pcWords ->
    case M.lookup w pcWords of
        Just hs -> return hs
        Nothing ->
            let code = fst (d M.! w) in
            simplify <$> (aoCodeToABC d code) >>= \ ops ->
            let hs = abcHash ops in
            get >>= \ (mW,mHS) ->
            put (M.insert w hs mW, M.insert hs ops mHS) >>
            return hs

-- obtain partially precompiled bytecode for an ABC program
aoCodeToABC :: InnerD md -> [AO_Action] -> State PCX [Op]
aoCodeToABC d (op:ops) = (++) <$> aoActionToABC d op <*> aoCodeToABC d ops
aoCodeToABC _ [] = return []

-- obtain bytecode, translating precompiled words to tokens
aoActionToABC :: InnerD md -> AO_Action -> State PCX [Op]
aoActionToABC d (AO_Word w) | isPCW w   = preComp d w >>= \ hs -> return [Tok ('#':hs)] 
                            | otherwise = aoCodeToABC d (fst (d M.! w))
aoActionToABC d (AO_Block aoOps) = aoCodeToABC d aoOps >>= \ ops -> return [BL ops]
aoActionToABC _ (AO_Num r) = return $ quotes r [Op_l]
aoActionToABC _ (AO_Text txt) = return $ quotes (TL txt) [Op_l]
aoActionToABC _ (AO_ABC aOp) = return [aopToABC aOp]
aoActionToABC _ (AO_Tok tok) = return [Tok tok]
