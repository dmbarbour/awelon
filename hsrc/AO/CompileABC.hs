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
--    Pass 0: forward pass
--       partial evaluation
--       permissive substructure
--       available to next pass
--    Pass 1: reverse pass
--       dead code elim
--       more info about expected types
--         (untyped data is 'pass-through')
--       track assertions backwards to observation (early fail)
--       more precise substructural requirements tracking
--    Pass 2: forward pass
--       futures, promised values
--       conversion to or from dynamic values model
--       build an 'eval for effects' monoid on the side
--       requires monad with reference-based state?
--    

module AO.CompileABC
    ( V0(..), op_v0, ops_v0, nullInvV0
    , pass0_run, pass0_check, pass0_anno
     
    ) where

import AO.V
import AO.ABC (simplifyABC)
import Data.Monoid (mappend)
import Control.Arrow (first, left, right)
import Control.Applicative
import Data.Functor.Identity
import Control.Monad.Trans.Error
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.Text as T
import qualified Data.List as L
import Data.Text (Text)

newtype E = E { inE :: Text }
instance Error E where strMsg = E . T.pack

-- pass0_check will trivially test a seq 
-- returns Left Error | Right Summary
pass0_check :: S.Seq Op -> Either Text Text
pass0_check = right (T.pack . show) . pass0_run nullInvV0 V0Dyn

pass0_run :: V0Inv -> V0 -> S.Seq Op -> Either Text V0
pass0_run ef v0 ops = runV0 $ ops_v0 ef ops v0 where

runV0 :: V0Pass a -> Either Text a
runV0 = left inE . runIdentity . runErrorT

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
    | V0Block KF (Maybe (S.Seq Op)) -- static block (KF permissive)
    | V0Num (Maybe Rational) -- static number
    | V0P V0 V0 -- product of values
    | V0S LV0 LV0 -- choice of values
    | V0Unit -- unit value (introduced by Op 'v')
    | V0Seal Text V0 -- sealed value

type LV0 = (Bool,V0)
type V0Pass = ErrorT E Identity
type V0Inv = Text -> V0 -> V0Pass V0
instance Show V0 where show = v0Summary 10

invFail :: (Monad m, Show v) => Text -> v -> m v
invFail tok v = fail $ "{" ++ T.unpack tok ++ "} @ " ++ show v

opFail :: (Monad m, Show v) => Char -> v -> m v
opFail op v = fail $ op : (" @ " ++ show v)

indent :: String -> String -> String
indent ws = L.unlines . map (ws ++) . L.lines 


-- a quick visual summary of static context, to help debugging
v0Summary :: Int -> V0 -> String
v0Summary n _ | (n < 0) = "..."
v0Summary _ V0Dyn = "?"
v0Summary _ (V0Block kf (Just ops)) = 
    let nSummary = 14 in
    let opsTxt0 = showOps (simplifyABC ops) in
    let bTooBig = GT == T.compareLength opsTxt0 nSummary in
    let opsTxt = if bTooBig 
            then let txtCut = T.take (nSummary - 3) opsTxt0 in
                 txtCut `T.append` T.pack "..."
            else opsTxt0
    in
    addkf kf $ "[" ++ T.unpack opsTxt ++ "]"
v0Summary _ (V0Block kf Nothing) = addkf kf "B"
v0Summary _ (V0Num (Just r)) = show (N r)
v0Summary _ (V0Num Nothing) = "N"
v0Summary n (V0P a b) = "(" ++ v0Summary (n-1) a ++ "*" ++ v0Summary (n-1) b ++ ")"
v0Summary n (V0S l r) = "(" ++ desc l ++ "+" ++ desc r ++ ")" where
    desc (True,v) = v0Summary (n-1) v
    desc (False,V0Dyn) = "void"
    desc (False,v) = "void`" ++ v0Summary (n-1) v
v0Summary _ V0Unit = "unit"
v0Summary n (V0Seal txt v) = (T.unpack txt) ++ ":" ++ v0Summary n v

addkf :: KF -> String -> String
addkf kf = addf . addk where
    addf = if may_copy kf then id else (++ "f") -- affine, no copy
    addk = if may_drop kf then id else (++ "k") -- relevant, no drop

nullInvV0 :: V0Inv
nullInvV0 = invFail

ops_v0 :: V0Inv -> S.Seq Op -> V0 -> V0Pass V0
ops_v0 ef = flip $ S.foldlM (flip $ op_v0 ef)

-- monad in this case is mostly used for errors (return vs. fail)
-- annotations are ignored, sealers are tracked, and other effects
-- are passed to the provided invoker. 
op_v0 :: V0Inv -> Op -> V0 -> V0Pass V0
op_v0 ef (Op '$') = op_v0_ap ef
op_v0 ef (Op '?') = op_v0_condap ef
op_v0 _ (Op c) = op_v0c c 
op_v0 _ (TL text) = return . V0P (textToV0 text)
op_v0 _ (BL ops) = return . V0P (V0Block kf0 (Just ops))
op_v0 ef (Invoke tok) = case T.uncons tok of
    Just ('&', _) -> return 
    Just (':', sealer) -> return . (V0Seal sealer)
    Just ('.', sealer) -> \ v0 -> case v0 of
        V0Seal s v | (s == sealer) -> return v
        V0Dyn -> return V0Dyn
        _ -> invFail tok v0
    _ -> ef tok
op_v0 ef (AMBC [ops]) = ops_v0 ef ops
op_v0 _ (AMBC _) = const $ fail $ "ambiguity not supported"

op_v0c :: Char -> V0 -> V0Pass V0
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
op_v0c  c  = opFail c -- unknown operation

op_v0_l, op_v0_r, op_v0_w, op_v0_z, op_v0_v, op_v0_c
    , op_v0_dup, op_v0_drop
    , op_v0_comp, op_v0_quote, op_v0_k, op_v0_f
    , op_v0_introNum
    , op_v0_add, op_v0_mul, op_v0_recip, op_v0_negate, op_v0_Q
    , op_v0_L, op_v0_R, op_v0_W, op_v0_Z, op_v0_V, op_v0_C
    , op_v0_D, op_v0_F, op_v0_M, op_v0_K
    , op_v0_P, op_v0_S, op_v0_B, op_v0_N, op_v0_gt 
    :: V0 -> V0Pass V0

op_v0_digit :: Int -> V0 -> V0Pass V0
op_v0_ap, op_v0_condap :: V0Inv -> V0 -> V0Pass V0

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
    if isUnitV0 vUnit  
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
        Just ops -> case pass0_run ef vx ops of
            Left etxt -> -- error within block call
                let emsg1 = "failure in '$' call @ " ++ show v in
                let emsg2 = indent " " (T.unpack etxt) in
                fail (emsg1 ++ "\n" ++ emsg2)
            Right vx' -> return $ V0P vx' ve

op_v0_comp v = 
    asPairV0 v >>= \ (byz, vbxy_e) ->
    asPairV0 vbxy_e >>= \ (bxy, ve) ->
    asBlockV0 byz >>= \ (kf1, yz) ->
    asBlockV0 bxy >>= \ (kf2, xy) ->
    let kf' = kf1 `mappend` kf2 in
    let xz = (S.><) <$> xy <*> yz in
    return $ V0P (mkBlockV0 kf' xz) ve

op_v0_quote v = 
    asPairV0 v >>= \ (vq, ve) ->
    return $ V0P (quoteV0 vq) ve

quoteV0 :: V0 -> V0
quoteV0 (V0Num (Just r)) = V0Block kf0 (Just $ abcQuote (N r))
quoteV0 (V0Block kf (Just ops)) = (V0Block kf (Just ops')) where
    ops' = (addf . addk) $ S.singleton (BL ops) 
    addk = if (may_drop kf) then id else (S.|> Op 'k')
    addf = if (may_copy kf) then id else (S.|> Op 'f')
quoteV0 v = V0Block kf Nothing where
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

op_v0_introNum = return . V0P num0 where
    num0 = V0Num (Just 0)

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
    return $ V0P (mkNumV0 mbr) ve

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
    let divbz = (Just 0) == mbb in
    let eMsg = "divmod by zero @ " ++ show v in
    if divbz then fail eMsg else
    case divModQ <$> mbb <*> mba of
        Nothing -> 
            let num = V0Num Nothing in
            return $ V0P num (V0P num ve)
        Just (r, q) ->
            let rn = V0Num (Just r) in
            let qn = V0Num (Just (fromIntegral q)) in
            return $ V0P rn (V0P qn ve)

op_v0_L v = 
    asPairV0 v >>= \ (vabc, ve) ->
    asSumV0 vabc >>= \ (va, vbc) ->
    asSumLV0 vbc >>= \ (vb, vc) ->
    let sum' = V0S (lV0S va vb) vc in
    return $ V0P sum' ve

lV0S :: LV0 -> LV0 -> LV0
lV0S a b = (fst a || fst b, V0S a b) 

op_v0_R v = 
    asPairV0 v >>= \ (vabc, ve) ->
    asSumV0 vabc >>= \ (vab, vc) ->
    asSumLV0 vab >>= \ (va, vb) ->
    let sum' = V0S va (lV0S vb vc) in
    return $ V0P sum' ve

op_v0_W v = 
    asPairV0 v >>= \ (vabc, ve) ->
    asSumV0 vabc >>= \ (va, vbc) ->
    asSumLV0 vbc >>= \ (vb, vc) ->
    let sum' = V0S vb (lV0S va vc) in
    return $ V0P sum' ve

op_v0_Z v = 
    asPairV0 v >>= \ (vabcd, ve) ->
    asSumV0 vabcd >>= \ (va, vbcd) ->
    asSumLV0 vbcd >>= \ (vb, vcd) ->
    asSumLV0 vcd >>= \ (vc, vd) ->
    let sum' = V0S va (lV0S vc (lV0S vb vd)) in
    return $ V0P sum' ve

op_v0_V v = 
    asPairV0 v >>= \ (va, ve) ->
    let sum' = V0S (True,va) voidLV0 in
    return $ V0P sum' ve

op_v0_C v = 
    asPairV0 v >>= \ (va0, ve) ->
    if isDynV0 va0 then return (V0P V0Dyn ve) else
    asSumV0 va0 >>= \ (va, v0) ->
    if isVoidLV0 v0 
        then return $ V0P (snd va) ve 
        else opFail 'C' v

op_v0_condap ef v = 
    asPairV0 v >>= \ (vf, vse) ->
    asPairV0 vse >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (lvl, lvr) ->
    asBlockV0 vf >>= \ (kf, mbops) ->
    let bDroppable = may_drop kf in
    let eUndroppable = "? (w/undroppable) @ " ++ show v in
    if not bDroppable then fail eUndroppable else
    case mbops of
        Nothing -> -- cannot test statically
            let lvl' = (fst lvl, V0Dyn) in
            return $ V0P (V0S lvl' lvr) ve
        Just ops -> case pass0_run ef (snd lvl) ops of
            Left etxt -> -- failed within call
                let emsg1 = "failure in '?' call @ " ++ show v in
                let emsg2 = indent " " (T.unpack etxt) in
                fail (emsg1 ++ "\n" ++ emsg2)
            Right vl' -> -- success; static block tested
                let lvl' = (fst lvl, vl') in
                return $ V0P (V0S lvl' lvr) ve

op_v0_D v =
    asPairV0 v >>= \ (va, vse) ->
    asPairV0 vse >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vb, vc) ->
    let ab = V0P va (snd vb) in
    let ac = V0P va (snd vc) in
    let sum' = V0S (fst vb,ab) (fst vc,ac) in
    return $ V0P sum' ve

op_v0_F v = 
    asPairV0 v >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vab, vcd) ->
    asPairV0 (snd vab) >>= \ (va, vb) ->
    asPairV0 (snd vcd) >>= \ (vc, vd) ->
    let ac = V0S (fst vab, va) (fst vcd,vc) in
    let bd = V0S (fst vab, vb) (fst vcd,vd) in
    return $ V0P ac (V0P bd ve)

op_v0_M v = 
    asPairV0 v >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vl, vr) ->
    return $ V0P (mergeLV0 vl vr) ve

-- this merge is permissive; it never rejects a merge because it
-- lacks enough context (such as blocks in the environment) to 
-- decide whether two values are *future* compatible.
mergeLV0 :: LV0 -> LV0 -> V0
mergeLV0 (False,_) (True,y) = y
mergeLV0 (True,x) (False,_) = x
mergeLV0 (_,x) (_,y) = mergeV0 x y

mergeV0 :: V0 -> V0 -> V0
mergeV0 V0Dyn _ = V0Dyn
mergeV0 _ V0Dyn = V0Dyn
mergeV0 V0Unit V0Unit = V0Unit
mergeV0 (V0P x1 x2) (V0P y1 y2) = V0P m1 m2 where
    m1 = mergeV0 x1 y1
    m2 = mergeV0 x2 y2
mergeV0 (V0S x1 x2) (V0S y1 y2) = V0S m1 m2 where
    m1 = (fst x1 || fst y1, mergeLV0 x1 y1)
    m2 = (fst x2 || fst y2, mergeLV0 x2 y2)
mergeV0 (V0Num r1) (V0Num r2) = 
    if (r1 == r2) then V0Num r1 
        else V0Num Nothing
mergeV0 (V0Block kfx x) (V0Block kfy y) =
    let kf' = kfx `mappend` kfy in
    if (x == y) then V0Block kf' x 
        else V0Block kf' Nothing
mergeV0 (V0Seal s1 x1) (V0Seal s2 x2)
    | (s1 == s2) = V0Seal s1 (mergeV0 x1 x2)
mergeV0 _ _ = V0Dyn

op_v0_K v = 
    asPairV0 v >>= \ (vs, ve) ->
    asSumV0 vs >>= \ (vl, vr) ->
    let inL = isVoidLV0 vr && not (isVoidLV0 vl) in
    if inL then fail $ "static assertion failure 'K' @ " ++ show v else
    return $ V0P (snd vr) ve

op_v0_P v =  
    asObservableV0 v >>= \ (vx, ve) ->
    let dynP = V0P V0Dyn V0Dyn in
    let vo = if isDynV0 vx then V0S (True,vx) (True,dynP) else
             if isProdV0 vx then V0S voidLV0 (True,vx) else
             V0S (True,vx) (False,dynP)
    in
    return $ V0P vo ve

op_v0_S v = 
    asObservableV0 v >>= \ (vx, ve) ->
    let dynS = V0S (True,V0Dyn) (True,V0Dyn) in
    let vo = if isDynV0 vx then V0S (True,vx) (True,dynS) else
             if isSumV0 vx then V0S voidLV0 (True,vx) else
             V0S (True,vx) (False,dynS)
    in
    return $ V0P vo ve

op_v0_B v = 
    asObservableV0 v >>= \ (vx, ve) ->
    let dynB = V0Block kf0 Nothing in
    let vo = if isDynV0 vx then V0S (True,vx) (True,dynB) else
             if isBlockV0 vx then V0S voidLV0 (True,vx) else
             V0S (True,vx) (False,dynB)
    in
    return $ V0P vo ve

op_v0_N v = 
    asObservableV0 v >>= \ (vx, ve) ->
    let dynN = V0Num Nothing in
    let vo = if isDynV0 vx then V0S (True,vx) (True,dynN) else
             if isNumV0 vx then V0S voidLV0 (True,vx) else
             V0S (True,vx) (False,dynN)
    in
    return $ V0P vo ve

voidLV0 :: LV0
voidLV0 = (False,V0Dyn)

op_v0_gt v = 
    asPairV0 v >>= \ (vx, vye) ->
    asPairV0 vye >>= \ (vy, ve) ->
    case runV0 $ gtV0 vy vx of
        Left etxt -> 
            let emsg1 = "comparison failure '>' @ " ++ show v in
            let emsg2 = indent " " $ T.unpack etxt in
            fail (emsg1 ++ "\n" ++ emsg2)
        Right bGT ->
            let vR = V0P vx vy in
            let vL = V0P vy vx in
            let vgt = case bGT of
                 Nothing    -> V0S (True,vL) (True,vR)  -- dynamic compare
                 Just True  -> V0S (False,vL) (True,vR) -- static true
                 Just False -> V0S (True,vL) (False,vR) -- static false
            in 
            return $ V0P vgt ve

-- returns Nothing if no comparison could be made
-- returns Just True if left is greater than right
-- returns Just False if left is less than right
--   Products are greater than numbers
--   Numbers are greater thans sums
--   Rights are greater than Lefts
-- fails when:
--   comparing unit with non-unit (and non-dyn)
--   comparing with sealed values or blocks
gtV0 :: V0 -> V0 -> V0Pass (Maybe Bool)
gtV0 (V0Dyn) _ = return Nothing
gtV0 _ (V0Dyn) = return Nothing
gtV0 V0Unit V0Unit = return (Just False)
gtV0 V0Unit y = fail $ "compare unit (inL) with " ++ show y
gtV0 x V0Unit = fail $ "compare unit (inR) with " ++ show x
gtV0 (V0Num x) (V0Num y) = return $ (>) <$> x <*> y
gtV0 (V0P x1 x2) (V0P y1 y2) =
    gtV0 x1 y1 >>= \ mbGT ->
    case mbGT of
        Nothing -> return Nothing
        Just True -> return (Just True)
        Just False ->
            gtV0 y1 x1 >>= \ mbLT ->
            case mbLT of
                Nothing -> return Nothing
                Just True -> return (Just False)
                Just False -> gtV0 x2 y2
gtV0 (V0S x1 x2) (V0S y1 y2) =
    let xInR = isVoidLV0 x1 && not (isVoidLV0 x2) in
    let xInL = not (isVoidLV0 x1) && isVoidLV0 x2 in
    let yInR = isVoidLV0 y1 && not (isVoidLV0 y2) in
    let yInL = not (isVoidLV0 y1) && isVoidLV0 y2 in
    if (xInL && yInL) then gtV0 (snd x1) (snd y1) else
    if (xInR && yInR) then gtV0 (snd x2) (snd y2) else
    if (xInR && yInL) then return (Just True) else
    if (xInL && yInR) then return (Just False) else
    return Nothing -- might be right vs. left at runtime
gtV0 bx by | (isBlockV0 bx || isBlockV0 by) = 
    -- no comparing blocks!
    fail $ "cannot compare " ++ show bx ++ " with " ++ show by
gtV0 tx ty | (isSealedV0 tx || isSealedV0 ty) =
    -- no comparing sealed values! unseal them first.
    fail $ "cannot compare " ++ show tx ++ " with " ++ show ty
gtV0 p n | isProdV0 p && (isNumV0 n || isSumV0 n) = return (Just True)
gtV0 n s | isNumV0 n && isSumV0 s = return (Just True)
gtV0 n p | (isNumV0 n || isSumV0 n) && isProdV0 p = return (Just False)
gtV0 s n | isSumV0 s && isNumV0 n = return (Just False)
gtV0 _ _ = return Nothing -- delay comparison to runtime

isVoidLV0 :: LV0 -> Bool
isVoidLV0 = not . fst

mkBlockV0 :: KF -> Maybe (S.Seq Op) -> V0
mkBlockV0 = V0Block

mkNumV0 :: Maybe Rational -> V0
mkNumV0 = V0Num

asPairV0 :: (Monad m) => V0 -> m (V0, V0)

asPairV0 (V0P a b) = return (a,b)
asPairV0 (V0Dyn) = return (V0Dyn, V0Dyn)
asPairV0 v = fail $ "pair expected @ " ++ show v

asSumV0 :: Monad m => V0 -> m (LV0, LV0)
asSumV0 (V0S a b) = return (a,b)
asSumV0 (V0Dyn) = return ((True,V0Dyn), (True,V0Dyn))
asSumV0 v = fail $ "sum expected @ " ++ show v

asSumLV0 :: Monad m => LV0 -> m (LV0, LV0)
asSumLV0 (vAlive,v) =
    asSumV0 v >>= \ (la, lb) ->
    let la' = first (vAlive &&) la in
    let lb' = first (vAlive &&) lb in
    return (la', lb')

asBlockV0 :: (Monad m) => V0 -> m (KF, Maybe (S.Seq Op))
asBlockV0 (V0Dyn) = return (kf0,Nothing)
asBlockV0 (V0Block kf mbops) = return (kf, mbops)
asBlockV0 v = fail $ "block expected @ " ++ show v

asObservableV0 :: (Monad m) => V0 -> m (V0, V0)
asObservableV0 v =
    asPairV0 v >>= \ (vo, ve) ->
    if isObservableV0 vo then return (vo, ve) else
    fail $ "observable reqired @ " ++ show v

asNumV0 :: (Monad m) => V0 -> m (Maybe Rational)
asNumV0 (V0Dyn) = return Nothing
asNumV0 (V0Num mbr) = return mbr
asNumV0 v = fail $ "number expected @ " ++ show v
    
textToV0 :: Text -> V0
textToV0 t = case T.uncons t of
    Nothing -> V0Num (Just 3)
    Just (c, t') -> 
        let nc = fromIntegral $ fromEnum c in
        V0P (V0Num (Just nc)) (textToV0 t')

-- test whether values are at least potentially 
-- droppable vs. copyable
isCopyableV0, isDroppableV0 :: V0 -> Bool
isCopyableV0 V0Dyn = True -- potentially copyable
isCopyableV0 (V0Block kf _) = may_copy kf
isCopyableV0 (V0Num _) = True
isCopyableV0 (V0P a b) = isCopyableV0 a && isCopyableV0 b
isCopyableV0 (V0S a b) = isCopyableV0 (snd a) && isCopyableV0 (snd b)
isCopyableV0 V0Unit = True
isCopyableV0 (V0Seal _ v) = isCopyableV0 v

isDroppableV0 V0Dyn = True -- potentially droppable
isDroppableV0 (V0Block kf _) = may_drop kf
isDroppableV0 (V0Num _) = True
isDroppableV0 (V0P a b) = isDroppableV0 a && isDroppableV0 b
isDroppableV0 (V0S a b) = isDroppableV0 (snd a) && isDroppableV0 (snd b)
isDroppableV0 V0Unit = True
isDroppableV0 (V0Seal _ v) = isDroppableV0 v

isUnitV0, isObservableV0 :: V0 -> Bool

isUnitV0 (V0Unit) = True
isUnitV0 (V0Dyn) = True
isUnitV0 _ = False

isObservableV0 (V0Unit) = False
isObservableV0 (V0Seal _ _) = False
isObservableV0 _ = True

-- simple tests
isDynV0, isNumV0, isBlockV0, isProdV0, isSumV0, isSealedV0 :: V0 -> Bool
isNumV0 (V0Num _) = True
isNumV0 _ = False
isBlockV0 (V0Block _ _) = True
isBlockV0 _ = False
isSumV0 (V0S _ _) = True
isSumV0 _ = False
isProdV0 (V0P _ _) = True
isProdV0 _ = False
isSealedV0 (V0Seal _ _) = True
isSealedV0 _ = False
isDynV0 V0Dyn = True
isDynV0 _ = False

------------------------------------------
-- Initial Pass Annotation!
------------------------------------------

pass0_anno :: V0Inv -> V0 -> S.Seq Op -> Either Text (S.Seq (V0,Op), V0)
pass0_anno ef v ops = runV0 $ pass0_annoM ef v ops

pass0_annoM :: V0Inv -> V0 -> S.Seq Op -> V0Pass (S.Seq (V0,Op), V0)
pass0_annoM ef = step S.empty where
    step sR v ops = case S.viewl ops of
        S.EmptyL -> return (sR, v)
        (op S.:< ops') ->
            let sR' = sR S.|> (v,op) in
            op_v0 ef op v >>= \ v' ->
            step sR' v' ops'

------------------------------------------
-- Okay, time for the second pass.
-- 
-- The first compilation pass handles partial evaluation,
-- and provides just a little structure to the output type.
-- It can also catch a few errors.
--
-- The second pass is all about dead-code elimination, and
-- propagating assertion requirements backwards (so we can
-- fail early). 

------------------------------------------
-- Okay... time for the second pass
--   in this case, the data flows right to left
--   but 



