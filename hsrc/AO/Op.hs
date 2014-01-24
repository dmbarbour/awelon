{-# LANGUAGE PatternGuards #-}
-- ABC primitive operations (dynamic)
module AO.Op 
    ( op_l, op_r, op_w, op_z, op_v, op_c
    , op_L, op_R, op_W, op_Z, op_V, op_C
    , op_sp, op_lf
    , op_drop, op_copy, copyable, droppable

    , op_num
    , op_0, op_1, op_2, op_3, op_4
    , op_5, op_6, op_7, op_8, op_9
    , op_add, op_mul, op_negate, op_recip, op_Q

    , op_apply, op_cond, op_quote
    , op_compose, op_rel, op_aff

    , op_D, op_F, op_M, op_K
    , op_P, op_S, op_B, op_N, op_GT
    
    , op_invoke_seal, op_invoke_unseal
    ) where

import Control.Monad ((>=>))
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Sequence as S
import AO.V

-- using 'fail' to report errors, rather than in value V
opFail :: (Monad c) => Char -> V c -> c error
opFail op v = fail $ op : (" @ " ++ show v)

p :: V c -> V c -> V c
p = prod

op_l, op_r, op_w, op_z, op_v, op_c :: (Monad c) => V c -> c (V c)

op_l (P kf a (P _ b c)) = return (P kf (p a b) c)
op_l v = opFail 'l' v

op_r (P kf (P _ a b) c) = return (P kf a (p b c))
op_r v = opFail 'r' v

op_w (P kf a (P _ b c)) = return (P kf b (p a c))
op_w v = opFail 'w' v

op_z (P kf a (P kf' b (P _ c d))) = return (P kf a (P kf' c (p b d)))
op_z v = opFail 'z' v

op_v a = return (p a U)

op_c (P _ a U) = return a
op_c v = opFail 'c' v

op_L, op_R, op_W, op_Z, op_V, op_C :: (Monad c) => V c -> c (V c)

op_L (P kf (L a) e) = return (P kf (L (L a)) e)
op_L (P kf (R (L b)) e) = return (P kf (L (R b)) e)
op_L (P kf (R (R c)) e) = return (P kf (R c) e)
op_L v = opFail 'L' v

op_R (P kf (L (L a)) e) = return (P kf (L a) e)
op_R (P kf (L (R b)) e) = return (P kf (R (L b)) e)
op_R (P kf (R c) e) = return (P kf (R (R c)) e)
op_R v = opFail 'R' v

op_W (P kf (L a) e) = return (P kf (R (L a)) e)
op_W (P kf (R (L b)) e) = return (P kf (L b) e)
op_W v@(P _ (R (R _)) _) = return v
op_W v = opFail 'W' v

op_Z v@(P _ (L _) _) = return v
op_Z (P kf (R (L b)) e) = return (P kf (R (R (L b))) e)
op_Z (P kf (R (R (L c))) e) = return (P kf (R (L c)) e)
op_Z v@(P _ (R (R (R _))) _) = return v
op_Z v = opFail 'Z' v

op_V (P kf a e) = return (P kf (L a) e)
op_V v = opFail 'V' v

op_C (P kf (L a) e) = return (P kf a e)
op_C v = opFail 'C' v

op_sp, op_lf :: (Monad c) => V c -> c (V c)
op_sp = return
op_lf = return

-- note: affine and relevant properties are enforced
--  but I might later remove this for performance
op_drop, op_copy :: (Monad c) => V c -> c (V c)
op_copy v@(P kf a _) | copyable a = return (P kf a v)
op_copy v = opFail '^' v

op_drop (P _ a e) | droppable a = return e
op_drop v = opFail '%' v


op_num, op_0, op_1, op_2, op_3, op_4
      , op_5, op_6, op_7, op_8, op_9 :: (Monad c) => V c -> c (V c)

op_num e = return (p (N 0) e)

op_0 = opDigit 0
op_1 = opDigit 1
op_2 = opDigit 2
op_3 = opDigit 3
op_4 = opDigit 4
op_5 = opDigit 5
op_6 = opDigit 6
op_7 = opDigit 7
op_8 = opDigit 8
op_9 = opDigit 9

opDigit :: (Monad c) => Int -> V c -> c (V c)
opDigit d (P kf (N r) e) = return (P kf (N (10*r + (fromIntegral d))) e)
opDigit d v = opFail (toEnum (48 + d)) v

op_add, op_mul, op_negate, op_recip, op_Q :: (Monad c) => V c -> c (V c)

op_add (P _ (N a) (P kf (N b) e)) = return (P kf (N (a+b)) e)
op_add v = opFail '+' v

op_mul (P _ (N a) (P kf (N b) e)) = return (P kf (N (a*b)) e)
op_mul v = opFail '*' v

op_negate (P kf (N a) e) = return (P kf (N (negate a)) e)
op_negate v = opFail '-' v

op_recip (P kf (N a) e) | (0 /= a) = return (P kf (N (recip a)) e)
op_recip v = opFail '/' v

op_Q (P _ (N b) (P kf (N a) e)) | (0 /= b) = 
    let (r,q) = divModQ b a in
    return (P kf (N r) (P kf (N (fromIntegral q)) e))
op_Q v = opFail 'Q' v

-- divModQ b a = (r,q)
--   such that qb + r = a
--             q is integral
--             r is in (b,0] or [0,b)
-- i.e. this is a divMod for rationals
divModQ :: Rational -> Rational -> (Rational, Integer)
divModQ b a = 
    let num = numerator a * denominator b in
    let den = numerator b * denominator a in
    let (qN,rN) = num `divMod` den in
    let denR = denominator a * denominator b in
    (rN % denR, qN)

op_apply, op_cond :: (Monad c) => (V c) -> c (V c)
op_apply (P _ (B _ abc) (P _ x e)) = 
    abc_comp abc x >>= \ x' -> return (p x' e)
op_apply v = opFail '$' v

op_cond (P _ (B kf abc) (P _ (L x) e)) | may_drop kf =
    abc_comp abc x >>= \ x' -> return (p (L x') e)
op_cond (P _ (B kf _) v@(P _ (R _) _)) | may_drop kf = 
    return v
op_cond v = opFail '$' v

op_compose :: (Monad c) => V c -> c (V c)
op_compose (P _ (B kyz cyz) (P _ (B kxy cxy) e)) = return (p bxz e) 
  where bxz = B kxz cxz
        kxz = KF { may_copy = (may_copy kyz && may_copy kxy)
                 , may_drop = (may_drop kyz && may_drop kxy) }
        cxz = ABC { abc_code = (abc_code cxy S.>< abc_code cyz)
                  , abc_comp = (abc_comp cxy >=> abc_comp cyz) }
op_compose v = opFail 'o' v

op_quote :: (Monad c) => V c -> c (V c)
op_quote (P kf a e) = return (P kf bb e) where
    bb = B bkf abc
    bkf = KF { may_copy = copyable a, may_drop = droppable a }
    abc = ABC { abc_code = abcQuote a, abc_comp = return . p a }
op_quote v = opFail '\'' v



op_rel, op_aff :: (Monad c) => V c -> c (V c)

op_rel (P _ (B kf abc) e) = return (p (B kf' abc) e) where
    kf' = kf { may_drop = False }
op_rel v = opFail 'k' v

op_aff (P _ (B kf abc) e) = return (p (B kf' abc) e) where
    kf' = kf { may_copy = False }
op_aff v = opFail 'f' v

op_D, op_F, op_M, op_K :: (Monad c) => V c -> c (V c)

op_D (P _ a (P _ (L b) e)) = return (p (L (p a b)) e)
op_D (P _ a (P _ (R c) e)) = return (p (R (p a c)) e)
op_D v = opFail 'D' v

op_F (P _ (L (P _ a b)) e) = return (p (L a) (p (L b) e))
op_F (P _ (R (P _ c d)) e) = return (p (R c) (p (R d) e))
op_F v = opFail 'F' v

op_M (P kf (L a) e) = return (P kf a e)
op_M (P kf (R a') e) = return (P kf a' e)
op_M v = opFail 'M' v

op_K (P kf (R b) e) = return (P kf b e)
op_K v = opFail 'K' v

op_P, op_S, op_B, op_N :: (Monad c) => V c -> c (V c)

op_P (P kf o@(P _ _ _) e) = return (P kf (R o) e)
op_P (P kf o e) | observable o = return (P kf (L o) e)
op_P v = opFail 'P' v

op_S (P kf o@(L _) e) = return (P kf (R o) e)
op_S (P kf o@(R _) e) = return (P kf (R o) e)
op_S (P kf o e) | observable o = return (P kf (L o) e)
op_S v = opFail 'S' v

op_B (P kf o@(B _ _) e) = return (P kf (R o) e)
op_B (P kf o e) | observable o = return (P kf (L o) e)
op_B v = opFail 'B' v

op_N (P kf o@(N _) e) = return (P kf (R o) e)
op_N (P kf o e) | observable o = return (P kf (L o) e)
op_N v = opFail 'N' v

op_GT :: (Monad c) => V c -> c (V c)
op_GT v@(P kf x (P _ y e)) =
    case tryGT y x of
        Just True -> return (P kf (R (p x y)) e)
        Just False -> return (P kf (L (p y x)) e)
        Nothing -> opFail '>' v
op_GT v = opFail '>' v

tryGT :: V c -> V c -> Maybe Bool
tryGT (N y) (N x) = Just (y > x)
tryGT (P _ y1 y2) (P _ x1 x2) =
    case tryGT y1 x1 of
        Nothing -> Nothing
        Just True -> Just True
        Just False -> case tryGT x1 y1 of
            Nothing -> Nothing
            Just True -> Just False
            Just False -> tryGT y2 x2
tryGT (R y) (R x) = tryGT y x
tryGT (L y) (L x) = tryGT y x
tryGT U U = Just False
tryGT y x = 
    if (structGT y x) then Just True else
    if (structGT x y) then Just False else
    Nothing

-- greater-than due to structure
structGT :: V c -> V c -> Bool
structGT (P _ _ _) x = is_num x || is_sum x
structGT (N _) x = is_sum x
structGT (R _) (L _) = True
structGT _ _ = False

is_num, is_sum :: V c -> Bool

is_num (N _) = True
is_num _ = False

is_sum (L _) = True
is_sum (R _) = True
is_sum _ = False

op_invoke_seal, op_invoke_unseal :: (Monad c) => Text -> V c -> c (V c)

op_invoke_seal tok = return . S tok
op_invoke_unseal t (S t' v) | (t == t') = return v
op_invoke_unseal t v = fail $
    "{/" ++ T.unpack t ++ "} @ " ++ show v

