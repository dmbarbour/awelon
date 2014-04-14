
-- | Efficiently obtain ABC code for a given AO command.
-- This is a 'pure' compilation, and ignores metadata.
module AO.Compile 
    ( compileAOtoABC
    , aopToABC
    ) where

import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Map as M

import ABC.Operators
import ABC.Quote
import AO.Code
import AO.Dict

-- | Compile AO to ABC... or fail with a list of missing words.
--
-- Due to AO's full-inlining semantics, ABC code tends to grow
-- exponentially with depth of abstraction. So it is favorable
-- to garbage collect the ABC code and rebuild as needed. 
compileAOtoABC :: AODict md -> [AO_Action] -> Either [Word] [Op]
compileAOtoABC clnD code =
    let d = readAODict clnD in
    let missingWords = L.filter (`M.notMember` d) (aoWords code) in
    if (null missingWords) then Right $ codeToABC d code [] 
                           else Left  $ L.nub missingWords

-- convert AO code to ABC assuming all words are in dictionary
codeToABC :: (M.Map Word (AO_Code, meta)) -> AO_Code -> QuoteS
codeToABC d (op:ops) = actionToABC d op . codeToABC d ops
codeToABC _ [] = id

-- convert AO action to ABC assuming all words are in dictionary
actionToABC :: (M.Map Word (AO_Code, meta)) -> AO_Action -> QuoteS
actionToABC d (AO_Word w) = codeToABC d $ fst $ fromJust $ M.lookup w d 
actionToABC d (AO_Block ops) = quotes (BL (codeToABC d ops [])) . quotes Op_l
actionToABC _ (AO_Num r) = quotes r . quotes Op_l
actionToABC _ (AO_Text txt) = quotes (TL txt) . quotes Op_l
actionToABC _ (AO_ABC aop) = quotes (aopToABC aop)
actionToABC _ (AO_Tok tok) = quotes (Tok tok)

-- | aopToABC will translate AO's inline ABC 
-- into the corresponding raw ABC operations
aopToABC :: AOp -> Op
aopToABC AOp_l = Op_l
aopToABC AOp_r = Op_r
aopToABC AOp_w = Op_w
aopToABC AOp_z = Op_z
aopToABC AOp_v = Op_v
aopToABC AOp_c = Op_c
aopToABC AOp_L = Op_L
aopToABC AOp_R = Op_R
aopToABC AOp_W = Op_W
aopToABC AOp_Z = Op_Z
aopToABC AOp_V = Op_V
aopToABC AOp_C = Op_C
aopToABC AOp_copy = Op_copy
aopToABC AOp_drop = Op_drop
aopToABC AOp_add = Op_add
aopToABC AOp_neg = Op_neg
aopToABC AOp_mul = Op_mul
aopToABC AOp_inv = Op_inv
aopToABC AOp_divMod = Op_divMod
aopToABC AOp_ap = Op_ap
aopToABC AOp_cond = Op_cond
aopToABC AOp_quote = Op_quote
aopToABC AOp_comp = Op_comp
aopToABC AOp_rel = Op_rel
aopToABC AOp_aff = Op_aff
aopToABC AOp_distrib = Op_distrib
aopToABC AOp_factor = Op_factor
aopToABC AOp_merge = Op_merge
aopToABC AOp_assert = Op_assert
aopToABC AOp_gt = Op_gt


{-

-- compile a clean (acyclic, fully defined) dictionary to ABC.
-- This is achieved by progressive inlining. 
compileDictionary :: Dictionary -> DictC
compileDictionary aoDict = abcDict where
    abcDict = L.foldl cw M.empty (M.keys aoDict)
    cw dc w =
        if M.member w dc then dc else
        maybe dc (cwd dc w) (M.lookup w aoDict)
    cwd dc w (_,def) =
        let deps = aoWordsRequired def in
        let dc' = L.foldl cw dc (Set.toList deps) in
        let abc = aoToABC dc' def in
        M.insert w abc dc'

-- | compile a definition to ABC, given a dictionary that
-- already contains the necessary words. Any missing words will
-- result in an annotation `{&~word}`, but it's probably better
-- to avoid the situation and test for missing words in advance.
aoToABC :: DictC -> S.Seq Action -> S.Seq Op
aoToABC dc = S.foldr (S.><) S.empty . fmap (actionToABC dc)

actionToABC :: DictC -> Action -> S.Seq Op
actionToABC dc (Word w) = maybe (annoMW w) id (M.lookup w dc)
actionToABC _ (Num r) = quoteNum r S.|> Op 'l'
actionToABC _ (Lit txt) = S.empty S.|> TL txt S.|> Op 'l'
actionToABC dc (BAO aoDef) = S.empty S.|> BL (aoToABC dc aoDef) S.|> Op 'l'
actionToABC _ (Prim ops) = ops
actionToABC dc (Amb [onlyOption]) = aoToABC dc onlyOption
actionToABC dc (Amb options) = S.singleton $ AMBC $ map (aoToABC dc) options

annoMW :: W -> S.Seq Op
annoMW = S.singleton . Invoke . T.cons '&' . T.cons '~'

-}
