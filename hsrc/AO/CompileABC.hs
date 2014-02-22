{-# LANGUAGE PatternGuards, EmptyDataDecls, MultiParamTypeClasses #-}
-- | CompileABC is an alternative compiler for ABC code.
--
-- The AO.ABC module includes a relatively direct bytecode
-- compiler and simplifier. However, it does not support
-- rich optimizations, such as partial evaluation and dead
-- code elimination and parallelization. In addition, it
-- performs a great deal of manual dataflow, with unnecessary
-- levels of allocation and deallocations.
--
-- This module provides an independent compiler for ABC, which
-- also performs a small degree of typechecking. Hopefully, this
-- will improve performance a great deal, and provide a useful
-- prototype for a direct implementation in AO. 
--
-- The compiler is implemented in multiple passes, information 
-- becoming more precise in each pass. I might be able to 
-- abstract common structure after doing this, but not before,
-- so there is a lot of duplication for now.
--
module AO.CompileABC
    ( V0(..), V0Inv, op_v0, ops_v0, nullInvV0
     
    ) where

import AO.V
import Data.Ratio
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Error
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.Text as T
import Data.Text (Text)

newtype E = E Text
instance Error E where strMsg = T.pack

-- pass 0 data:
--   some decidedly ad-hoc type information 
--     (could probably refine this a lot)
--   with a few static values available
--   detect simple type errors & partially evaluate some
--
-- pass 0 avoids the 'infinite loop' issue due to merging
-- of conditions during a valid loop. I should probably 
-- develop a more precise model, but this will do for now.
data V0
    = V0Dyn -- runtime value (any type except Void)
    | V0Block KF (S.Seq Op) -- static block (KF permissive)
    | V0BKF KF -- expected to be a block (for typechecking)
    | V0NumC Rational -- static number
    | V0Num -- expected to be a number (for typechecking)
    | V0P V0 V0 -- product of values
    | V0S V0 V0 -- choice of values
    | V0Unit -- unit value (introduced by Op 'v')
    | V0Void V0 -- void (introduced by Op 'V') + static info
    | V0Seal Text V0 -- sealed value

voidV0 :: V0
voidV0 = V0Void V0Dyn

type V0Inv m = Text -> V0 -> m V0
instance Show V0 where show = v0Summary

invFail :: (Monad m, Show v) => Text -> v -> m v
infFail tok v = fail $ "{" ++ T.unpack tok ++ "} @ " ++ show v

opFail :: (Monad m, Show v) => Char -> v -> m v
opFail op v = fail $ op : (" @ " ++ show v)


-- a quick visual summary of static context, to help debugging
v0Summary :: V0 -> String
v0Summary V0Dyn = "?"
v0Summary (V0Block kf _) = addkf kf "B."
v0Summary (V0BKF kf) = addkf kf "B"
v0Summary (V0NumC _) = "N."
v0Summary (V0Num) = "N"
v0Summary (V0P a b) = "(" ++ v0Summary a ++ "*" v0Summary b ++ ")"
v0Summary (V0S a b) = "(" ++ v0Summary a ++ "+" v0Summary b ++ ")"
v0Summary V0Unit = "1"
v0Summary (V0Void V0Dyn) = "0"
v0Summary (V0Void v) = "0(~" ++ v0Summary v ++ ")"
v0Summary (V0Seal txt v) = (T.unpack txt) ++ ":" ++ v0Summary v

addkf :: KF -> String -> String
addkf kf = addf . addk where
    addf = if may_copy kf then id else (++ "f") -- affine, no copy
    addk = if may_drop kf then id else (++ "k") -- relevant, no drop



nullInvV0 :: (Monad m) => V0Inv m
nullInvV0 = invFail

-- monad in this case is mostly used for errors (return vs. fail)
-- annotations are ignored, sealers are tracked, and other effects
-- are passed to the provided invoker. 
op_v0 :: (Monad m) => V0Inv m -> Op -> V0 -> m V0
op_v0 ef (Op '$') = op_v0_ap ef
op_v0 ef (Op '?') = op_v0_condap ef
op_v0 _ (Op c) = op_v0c c 
op_v0 _ (TL text) = return . V0P (textToV0 text)
op_v0 _ (BL ops) = return . V0P (V0Block kf0 ops)
op_v0 ef (Invoke tok) = case T.uncons tok of
    Just ('&', _) -> return 
    Just (':', sealer) -> return . (V0Seal sealer)
    Just ('.', sealer) -> \ v0 -> case v0 of
        V0Seal s v | (s == sealer) -> return v
        V0Dyn -> return V0Dyn
        _ -> invFail tok v0
    _ -> eff tok
op_v0 ef (AMBC [ops]) = ops_v0 ef ops
op_v0 _ (AMBC _) = const $ fail $ "ambiguity not supported"

ops_v0 :: (Monad m) => V0Inv m -> S.Seq Op -> V0 -> m V0
ops_v0 ef = flip $ S.foldlM (flip $ op_v0 ef)

op_v0c :: (Monad m) => Char -> V0 -> m V0
op_v0c ' ' = return
op_v0c '\n' = return
op_v0c 'l' = op_v0_l
op_v0c 'r' = op_v0_r
op_v0c 'w' = op_v0_w
op_v0c 'z' = op_v0_z
op_v0c 'v' = op_v0_v
op_v0c 'c' = op_v0_c
op_v0c '^' = op_v0_dup
op_v0c '%' = op_v0_drop
op_v0c 'o' = op_v0_comp
op_v0c '\'' = op_v0_quote
op_v0c 'k' = op_v0_k
op_v0c 'f' = op_v0_f
op_v0c '#' = op_v0_introNum
op_v0c c | (('0' <= c) && (c <= '9')) = 
    op_v0_digit (fromEnum c - 48)
op_v0c '+' = op_v0_add
op_v0c '*' = op_v0_mul
op_v0c '-' = op_v0_negate
op_v0c '/' = op_v0_recip
op_v0c 'Q' = op_v0_Q
op_v0c 'L' = op_v0_L
op_v0c 'R' = op_v0_R
op_v0c 'W' = op_v0_W
op_v0c 'Z' = op_v0_Z
op_v0c 'V' = op_v0_V
op_v0c 'C' = op_v0_C
op_v0c 'D' = op_v0_D
op_v0c 'F' = op_v0_F
op_v0c 'M' = op_v0_M
op_v0c 'K' = op_v0_K
op_v0c 'P' = op_v0_P
op_v0c 'S' = op_v0_S
op_v0c 'B' = op_v0_B
op_v0c 'N' = op_v0_N
op_v0c '>' = op_v0_gt
op_v0c c v = opFail c v -- unknown operation

op_v0_l, op_v0_r, op_v0_w, op_v0_z, op_v0_v, op_v0_c
    , op_v0_dup, op_v0_drop
    , op_v0_ap, op_v0_comp, op_v0_quote, op_v0_k, op_v0_f
    , op_v0_introNum, op_v0_digit
    , op_v0_add, op_v0_mul, op_v0_recip, op_v0_negate, op_v0_Q
    , op_v0_L, op_v0_R, op_v0_W, op_v0_Z, op_v0_V, op_v0_C
    , op_v0_condap, op_v0_D, op_v0_F, op_v0_M, op_v0_K
    , op_v0_P, op_v0_S, op_V0_B, op_v0_N, op_v0_gt 
    :: Monad m => V0 -> m V0

op_v0_l v =
    asPairV0 v >>= \ (va, vbc) ->
    asPairV0 vbc >>= \ (vb, vc) ->
    return $ V0P (V0P va vb) vc

op_v0_r v =
    asPairV0 v >>= \ (vab, vc) ->
    asPairV0 vab >>= \ (va, vb) ->
    return $ V0P va (V0P vb vc)

op_v0_w v = 
    asPairV0 v >>= \ (va, vbc) ->
    asPairV0 vbc >>= \ (vb, vc) ->
    return $ V0P vb (V0P va vc)

op_v0_z v =
    asPairV0 v >>= \ (va, vbcd) ->
    asPairV0 vbcd >>= \ (vb, vcd) ->
    asPairV0 vcd >>= \ (vc, vd) ->
    return $ V0P va (V0P vc (V0P vb vd))

op_v0_v v = return $ V0P v V0Unit

op_v0_c v = 
    asPairV0 v >>= \ (va, vUnit) ->
    if isUnit vUnit 
        then return va 
        else opFail 'c' v

op_v0_dup v =
    asPairV0 v >>= \ (va,ve) ->
    if isCopyableV0 va
        then return $ V0P va (V0P va ve)
        else opFail '^' v

op_v0_drop v =
    asPairV0 v >>= \ (va,ve) ->
    if isDroppableV0 va 
        then return ve
        else opFail '%' v

op_v0_ap ef v = 
    asPairV0 v >>= \ (b, vxe) ->
    asPairV0 vxe >>= \ (vx, ve) ->
    asBlockV0 b >>= \ (_,mbops) ->
    case mbops of
        Nothing -> return $ V0P V0Dyn ve
        Just ops -> 
            ops_v0 ef ops vx >>= \ vx' ->
            return $ V0P vx' ve

op_v0_comp v = 
    asPairV0 v >>= \ (byz, vbxy_e) ->
    asPairV0 vbxy_e >>= \ (bxy, ve) ->
    asBlockV0 byz >>= \ (kf1, opsyz) ->
    asBlockV0 bxy >>= \ (kf2, opsxy) ->
    let kf' = KF { may_copy = (may_copy kf1 && may_copy kf2)
                 , may_drop = (may_drop kf1 && may_drop kf2) }
    in
    let opsxz = (S.><) <$> opsxy <*> opsyz in
    return $ V0P (mkBlockV0 kf opsxz) ve

op_v0_quote v = 
    asPairV0 v >>= \ (vq, ve) ->
    return $ V0P (quoteV0 vq) ve

quoteV0 :: V0 -> V0
quoteV0 (V0NumC r) = V0Block kf0 (abcQuote (N r))
quoteV0 (V0Block kf ops) = (V0Block kf ops') where
    ops' = addf $ addk $ S.singleton (BL ops) 
    addk = if (may_drop kf) then id else (S.|> Op 'k')
    addf = if (may_copy kf) then id else (S.|> Op 'f')
quoteV0 v = V0BKF kf where
    kf = KF { may_copy = isCopyableV0 v
            , may_drop = isDroppableV0 v }

op_v0_k v = 
    asPairV0 v >>= \ (vb, ve) ->
    asBlockV0 vb >>= \ (kf, mbops) ->
    let kf' = kf { may_drop = False } in
    return $ V0P (mkBlockV0 kf' mbops) ve

op_v0_f v = 
    asPairV0 v >>= \ (vb, ve) ->
    asBlockV0 vb >>= \ (kf, mbops) ->
    let kf' = kf { may_copy = False } in
    return $ V0P (mkBlockV0 kf' mbops) ve

op_v0_introNum = return . V0P (V0NumC 0)

op_v0_digit d v = 
    let f r = 10 * r + fromIntegral d in
    asPairV0 v >>= \ (vn, ve) ->
    asNumV0 vn >>= \ mbn ->
    return $ V0P (mkNumV0 (f <$> mbn)) ve

op_v0_add v =  
    asPairV0 v >>= \ (va, vbe) ->
    asPairV0 vbe >>= \ (vb, ve) ->
    asNumV0 va >>= \ mba ->
    asNumV0 vb >>= \ mbb ->
    let mbr = (+) <$> mba <*> mbb in
    return $ VOP (mkNumV0 mbr) ve

op_v0_mul v = 
    asPairV0 v >>= \ (va, vbe) ->
    asPairV0 vbe >>= \ (vb, ve) ->
    asNumV0 va >>= \ mba ->
    asNumV0 vb >>= \ mbb ->
    let mbr = (*) <$> mba <*> mbb in
    return $ V0P (mkNumV0 mbr) ve

op_v0_recip v = 
    asPairV0 v >>= \ (vn, ve) ->
    asNumV0 vn >>= \ (mbn) ->
    if (Just 0 == mbn)
        then fail $ "recip of zero @ " ++ show v
        else return $ V0P (mkNumV0 (recip <$> mbn)) ve

op_v0_negate v = 
    asPairV0 v >>= \ (vn, ve) ->
    asNumV0 vn >>= \ mbn ->
    return $ V0P (mkNumV0 (negate <$> mbn)) ve

op_v0_Q v = 
    asPairV0 v >>= \ (vb, vae) ->
    asPairV0 vae >>= \ (va, ve) ->
    asNumV0 vb >>= \ mbb ->
    asNumV0 va >>= \ mba ->
    let mbr = divModQ <$> mbb <*> mba in
    if (Just 0 == mbb)
        then fail $ "divmod by zero @ " ++ show v
        else return $ V0P (mkNumV0 mbr) ve

op_v0_L v = 
    asPairV0 v >>= \ (vabc, ve) ->
    asSumV0 vabc >>= \ (va, vbc) ->
    asSumV0 vbc >>= \ (vb, vc) ->
    let sum' = V0S (V0S va vb) vc in
    return $ V0P sum' ve

op_v0_R v = 
    asPairV0 v >>= \ (vabc, ve) ->
    asSumV0 vabc >>= \ (vab, vc) ->
    asSumV0 vab >>= \ (va, vb) ->
    let sum' = V0S va (V0S vb vc) in
    return $ V0P sum' ve

op_v0_W v = 
    asPairV0 v >>= \ (vabc, ve) ->
    asSumV0 vabc >>= \ (va, vbc) ->
    asSumV0 vbc >>= \ (vb, vc) ->
    let sum' = V0S vb (V0S va vc) in
    return $ V0P sum' ve

op_v0_Z v = 
    asPairV0 v >>= \ (vabcd, ve) ->
    asSumV0 vabcd >>= \ (va, vbcd) ->
    asSumV0 vbcd >>= \ (vb, vcd) ->
    asSumV0 vcd >>= \ (vc, vd) ->
    let sum' = V0S va (V0S vc (V0S vb vd)) in
    return $ V0P sum' ve

op_v0_V v = 
    asPairV0 v >>= \ (va, ve) ->
    let sum' = V0S va (V0Void V0Dyn) in
    return $ V0P sum' ve

op_v0_C v = 
    asPairV0 v >>= \ (va0, ve) ->
    asSumV0 va0 >>= \ (va, v0) ->
    if isVoidV0 v0 
        then return $ V0P va ve 
        else opFail 'C' v

op_v0_condap ef v = 
    asPairV0 v >>= \ (vf, vse) ->
    asPairV0 vse >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vl, vr) ->
    asBlockV0 vf >>= \ (kf, mbops) ->
    let bDroppable = may_drop kf in
    let eUndroppable = "? (w/undroppable) @ " ++ show v in
    if not bDroppable then fail eUndroppable else
    case (vl, mbops) of
        (_, Nothing) -> return $ V0P (V0S V0Dyn vr) ve
        (V0Void vInVoid, Just ops) ->
            ops_v0 ef ops vInVoid >>= \ vInVoid' ->
            let sum' = V0S (V0Void vInVoid') vr in
            return $ V0P sum' ve
        (_, Just ops) ->
            ops_v0 ef ops vl >>= \ vl' ->
            return $ V0P (V0S vl' vr) ve

op_v0_D v =
    asPairV0 v >>= \ (va, vse) ->
    asPairV0 vse >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vb, vc) ->
    let ab = V0P va vb in
    let ac = V0P va vc in
    let sum' = V0S ab ac in
    return $ V0P sum' ve

op_v0_F v = 
    asPairV0 v >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vab, vcd) ->
    asPairV0 vab >>= \ (va, vb) ->
    asPairV0 vcd >>= \ (vc, vd) ->
    let ac = V0S va vc in
    let bd = V0S vb vd in
    return $ V0P ac (V0P bd ve)

op_v0_M v = 
    asPairV0 v >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vl, vr) ->
    mergeV0 vl vr >>= \ vm ->
    return $ V0P vm ve

mergeV0 :: (Monad m) => V0 -> V0 -> m V0
mergeV0 (V0Void x) y = mergeV0 x y >> return y -- static merge right
mergeV0 x (V0Void y) = mergeV0 x y >> return x -- static merge left
mergeV0 V0Dyn _ = return V0Dyn -- permissively merge Dyn with anything
mergeV0 _ V0Dyn = return V0Dyn
mergeV0 V0Unit V0Unit = return V0Unit
mergeV0 l@(V0NumC x) (V0NumC y) | (x == y) = return l
mergeV0 x y | isNumV0 x && isNumV0 y = return V0Num
mergeV0 l@(V0Block kfx opsx) (V0Block kfy opsy) | (opsx == opsy) =
    return $ V0Block (mergeKF kfx kfy) opsx
mergeV0 l
mergeV0 (V0B
 y | isBlockV0 x  
mergeV0 x y | isObservableV0 x && isObservableV0 y = return 


mergeV0 V0Unit v = fail $ "cannot merge unit (inL) with " ++ show v
mergeV0 v V0Unit = fail $ "cannot merge unit (inR) with " ++ show v
mergeV0 (V0Seal s1 x1) (V0Seal s2 x2) | (s1 == s2) =
    mergeV0 x1 x2 >>= \ xm ->
    return (V0Seal s1 xm)
mergeV0 
then fail ("merge failure for seals: " ++ T.unpack s1 " & " T.un

op_v0_K v = 
    asPairV0 v >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vl, vr) ->
    let staticAssertionFailure = 
            isVoidV0 vr &&    -- right must be active
            not (isVoidV0 vl) -- unless left is also inactive
    in
    if staticAssertionFailure then opFail 'K' v else
    return $ V0P vr ve

op_v0_P v =  
    asObservableV0 v >>= \ (vx, ve) ->
    let dynP = V0P V0Dyn V0Dyn in
    let vo = if isDynV0 vx then V0S vx dynP else
             if isProdV0 vx then V0S voidV0 vx else
             V0S vx (V0Void dynP)
    in
    return $ V0P vo ve

op_v0_S v = 
    asObservableV0 v >>= \ (vx, ve) ->
    let dynS = V0S V0Dyn V0Dyn in
    let vo = if isDynV0 vx then V0S vx dynS else
             if isSumV0 vx then V0S voidV0 vx else
             V0S vx (V0Void dynS)
    in
    return $ V0P vo ve

op_v0_B v = 
    asObservableV0 v >>= \ (vx, ve) ->
    let dynB = V0BKF kf0 in
    let vo = if isDynV0 vx then V0S vx dynB else
             if isBlockV0 vx then V0S voidV0 vx else
             V0S vx (V0Void dynB)
    in
    return $ V0P vo ve

op_v0_N v = 
    asObservableV0 v >>= \ (vx, ve) ->
    let dynN = V0Num in
    let vo = if isDynV0 vx then V0S vx dynN else
             if isNumV0 vx then V0S voidV0 vx else
             V0S vx (V0Void dynN)
    in
    return $ V0P vo ve

op_v0_gt v = 
    asPairV0 v >>= \ (vx, vye) ->
    asPairV0 vye >>= \ (vy, ve) ->
    gtV0 vy vx >>= \ bGT ->
    let vR = V0P vx vy in
    let vL = V0P vy vx in
    let vgt = case bGT of
            Nothing -> V0S vL vR
            Just True -> V0S (V0Void vL) vR
            Just False -> V0S vL (V0Void vR)
    in
    return $ V0P vgt ve






mkBlockV0 :: KF -> Maybe (S.Seq Op) -> V0
mkBlockV0 kf = maybe (V0BKF kf) (V0Block kf)

mkNumV0 :: Maybe Rational -> V0
mkNumV0 = maybe V0Num V0NumC

asPairV0, asSumV0 :: (Monad m) => V0 -> m (V0, V0)

asPairV0 (V0P a b) = return (a,b)
asPairV0 (V0Dyn) = return (V0Dyn, V0Dyn)
asPairV0 v = fail $ "pair expected @ " ++ show v

asSumV0 (V0S a b) = return (a,b)
asSumV0 (V0Dyn) = return (V0Dyn, V0Dyn)
asSumV0 (V0Void v) = -- split a void
    asSumV0 v >>= \ (a,b) ->
    return (V0Void a, V0Void b)
asSumV0 v = fail $ "sum expected @ " ++ show v

asBlockV0 :: (Monad m) => V0 -> m (KF, Maybe (S.Seq Op))
asBlockV0 (V0Dyn) = return (kf0,Nothing)
asBlockV0 (V0BKF kf) = return (kf, Nothing)
asBlockV0 (V0Block kf ops) = return (kf, Just ops)
asBlockV0 v = fail $ "block expected @ " ++ show v

asObservableV0 :: (Monad m) => V0 -> m (V0, V0)
asObservableV0 v =
    asPairV0 v >>= \ (vo, ve) ->
    if isObservableV0 vo then return (vo, ve) else
    fail $ "observable reqired @ " ++ show v

asNumV0 :: (Monad m) => V0 -> m (Maybe Rational)
asNumV0 (V0Dyn) = return Nothing
asNumV0 (V0Num) = return Nothing
asNumV0 (V0NumC r) = return (Just r)
asNumV0 v = fail $ "number expected @ " ++ show v
    
textToV0 :: Text -> V0
textToV0 t = case T.uncons t of
    Nothing -> V0NumC 3
    Just (c, t') -> 
        let nc = fromIntegral $ fromEnum c in
        V0P (V0NumC nc) (textToV0 t')

-- test whether values are at least potentially 
-- droppable vs. copyable
isCopyableV0, isDroppableV0 :: V0 -> Bool
isCopyableV0 V0Dyn = True -- potentially copyable
isCopyableV0 (V0Block kf _) = may_copy kf
isCopyableV0 (V0BKF kf) = may_copy kf
isCopyableV0 (V0NumC _) = True
isCopyableV0 (V0Num) = True
isCopyableV0 (V0P a b) = isCopyableV0 a && isCopyableV0 b
isCopyableV0 (V0S a b) = isCopyableV0 a && isCopyableV0 b
isCopyableV0 V0Unit = True
isCopyableV0 (V0Void v) = isCopyableV0 v
isCopyableV0 (V0Seal _ v) = isCopyableV0 v

isDroppableV0 V0Dyn = True -- potentially droppable
isDroppableV0 (V0Block kf _) = may_drop kf
isDroppableV0 (V0BKF kf) = may_drop kf
isDroppableV0 (V0NumC _) = True
isDroppableV0 (V0Num) = True
isDroppableV0 (V0P a b) = isDroppableV0 a && isDroppableV0 b
isDroppableV0 (V0S a b) = isDroppableV0 a && isDroppableV0 b
isDroppableV0 V0Unit = True
isDroppableV0 (V0Void v) = isDroppableV0 v
isDroppableV0 (V0Seal _ v) = isDroppableV0 v

isUnitV0, isVoidV0, isObservableV0 :: V0 -> Bool

isUnitV0 (V0Unit) = True
isUnitV0 (V0Dyn) = True
isUnitV0 _ = False

isVoidV0 (V0Void _) = True
isVoidV0 (V0S a b) = isVoidV0 a && isVoidV0 b
isVoidV0 _ = False

isObservableV0 (V0Unit) = False
isObservableV0 (V0Seal _ _) = False
isObservableV0 _ = True

isDynV0, isNumV0, isBlockV0, isProdV0, isSumV0 :: V0 -> Bool

isDynV0 (V0Dyn) = True
isDynV0 _ = False

isNumV0 (V0Num) = True
isNumV0 (V0NumC _) = True
isNumV0 (V0Dyn) = True -- permissively
isNumV0 _ = False

isBlockV0 (V0BKF _) = True
isBlockV0 (V0Block _ _) = True
isBlockV0 (V0Dyn) = True -- permissively
isBlockV0 _ = False

isSumV0 (V0S _ _) = True
isSumV0 (V0Dyn) = True -- permissively
isSumV0 _ = False

isProdV0 (V0P _ _) = True
isProdV0 (V0Dyn) = True -- permissively
isProdV0 _ = False

