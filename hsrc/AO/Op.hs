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

    , op_apply, tcLoop, op_cond, op_quote
    , op_compose, abcCompose, op_rel, op_aff

    , op_D, op_F, op_M, op_K
    , op_P, op_S, op_B, op_N, op_GT
    
    , op_invoke_seal, op_invoke_unseal
    ) where

import Control.Monad ((>=>))
import Data.Ratio
import Data.Text (Text)
import qualified Data.Sequence as S
import qualified Data.Text as T
import AO.V

-- using 'fail' to report errors, rather than in value V
opFail :: (Monad c) => Char -> V c -> c error
opFail op v = fail $ op : (" @ " ++ show v)

op_l, op_r, op_w, op_z, op_v, op_c :: (Monad c) => V c -> c (V c)

op_l (P a (P b c)) = return (P (P a b) c)
op_l v = opFail 'l' v

op_r (P (P a b) c) = return (P a (P b c))
op_r v = opFail 'r' v

op_w (P a (P b c)) = return (P b (P a c))
op_w v = opFail 'w' v

op_z (P a (P b (P c d))) = return (P a (P c (P b d)))
op_z v = opFail 'z' v

op_v a = return (P a U)

op_c (P a U) = return a
op_c v = opFail 'c' v

op_L, op_R, op_W, op_Z, op_V, op_C :: (Monad c) => V c -> c (V c)

op_L (P (L a) e) = return (P (L (L a)) e)
op_L (P (R (L b)) e) = return (P (L (R b)) e)
op_L (P (R (R c)) e) = return (P (R c) e)
op_L v = opFail 'L' v

op_R (P (L (L a)) e) = return (P (L a) e)
op_R (P (L (R b)) e) = return (P (R (L b)) e)
op_R (P (R c) e) = return (P (R (R c)) e)
op_R v = opFail 'R' v

op_W (P (L a) e) = return (P (R (L a)) e)
op_W (P (R (L b)) e) = return (P (L b) e)
op_W v@(P (R (R _)) _) = return v
op_W v = opFail 'W' v

op_Z v@(P (L _) _) = return v
op_Z (P (R (L b)) e) = return (P (R (R (L b))) e)
op_Z (P (R (R (L c))) e) = return (P (R (L c)) e)
op_Z v@(P (R (R (R _))) _) = return v
op_Z v = opFail 'Z' v

op_V (P a e) = return (P (L a) e)
op_V v = opFail 'V' v

op_C (P (L a) e) = return (P a e)
op_C v = opFail 'C' v

op_sp, op_lf :: (Monad c) => V c -> c (V c)
op_sp = return
op_lf = return

-- note: affine and relevant properties are enforced
--  but I might later remove this for performance
op_drop, op_copy :: (Monad c) => V c -> c (V c)
op_copy (P a e) | copyable a = return (P a (P a e))
op_copy v = opFail '^' v

op_drop (P a e) | droppable a = return e
op_drop v = opFail '%' v


op_num, op_0, op_1, op_2, op_3, op_4
      , op_5, op_6, op_7, op_8, op_9 :: (Monad c) => V c -> c (V c)

op_num = return . P (N 0)

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
opDigit d (P (N r) e) = return (P (N (10*r + (fromIntegral d))) e)
opDigit d v = opFail (toEnum (48 + d)) v

op_add, op_mul, op_negate, op_recip, op_Q :: (Monad c) => V c -> c (V c)

op_add (P (N a) (P (N b) e)) = return (P (N (a+b)) e)
op_add v = opFail '+' v

op_mul (P (N a) (P (N b) e)) = return (P (N (a*b)) e)
op_mul v = opFail '*' v

op_negate (P (N a) e) = return (P (N (negate a)) e)
op_negate v = opFail '-' v

op_recip (P (N a) e) | (0 /= a) = return (P (N (recip a)) e)
op_recip v = opFail '/' v

op_Q (P (N b) (P (N a) e)) | (0 /= b) = 
    let (r,q) = divModQ b a in
    return (P (N r) (P (N (fromIntegral q)) e))
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
op_apply (P (B _ abc) (P x e)) = 
    abc_comp abc x >>= tcLoop >>= \ x' -> return (P x' e)
op_apply v = opFail '$' v

op_cond (P (B kf abc) (P (L x) e)) | may_drop kf =
    abc_comp abc x >>= tcLoop >>= \ x' -> return (P (L x') e)
op_cond (P (B kf _) v@(P (R _) _)) | may_drop kf = 
    return v
op_cond v = opFail '$' v

tcLoop :: (Monad c) => (V c) -> c (V c)
tcLoop (TC op) = op >>= tcLoop
tcLoop op = return op

op_compose :: (Monad c) => V c -> c (V c)
op_compose (P (B kyz cyz) (P (B kxy cxy) e)) = return (P bxz e) 
  where bxz = B kxz cxz
        kxz = KF { may_copy = (may_copy kyz && may_copy kxy)
                 , may_drop = (may_drop kyz && may_drop kxy) }
        cxz = abcCompose cxy cyz
op_compose v = opFail 'o' v

abcCompose :: (Monad c) => ABC c -> ABC c -> ABC c
abcCompose xy yz = xz where
    xz = ABC { abc_code = (abc_code xy S.>< abc_code yz) 
             , abc_comp = (abc_comp xy >=> (tcLoop >=> abc_comp yz))  }


op_quote :: (Monad c) => V c -> c (V c)
op_quote (P a e) = return (P bb e) where
    bb = B kf abc
    kf = KF { may_copy = copyable a, may_drop = droppable a }
    abc = ABC { abc_code = abcQuote a, abc_comp = return . P a }
op_quote v = opFail '\'' v



op_rel, op_aff :: (Monad c) => V c -> c (V c)

op_rel (P (B kf abc) e) = return (P (B kf' abc) e) where
    kf' = kf { may_drop = False }
op_rel v = opFail 'k' v

op_aff (P (B kf abc) e) = return (P (B kf' abc) e) where
    kf' = kf { may_copy = False }
op_aff v = opFail 'f' v

op_D, op_F, op_M, op_K :: (Monad c) => V c -> c (V c)

op_D (P a (P (L b) e)) = return (P (L (P a b)) e)
op_D (P a (P (R c) e)) = return (P (R (P a c)) e)
op_D v = opFail 'D' v

op_F (P (L (P a b)) e) = return (P (L a) (P (L b) e))
op_F (P (R (P c d)) e) = return (P (R c) (P (R d) e))
op_F v = opFail 'F' v

op_M (P (L a) e) = return (P a e)
op_M (P (R a') e) = return (P a' e)
op_M v = opFail 'M' v

op_K (P (R b) e) = return (P b e)
op_K v = opFail 'K' v

op_P, op_S, op_B, op_N :: (Monad c) => V c -> c (V c)

op_P (P o@(P _ _) e) = return (P (R o) e)
op_P (P o e) | observable o = return (P (L o) e)
op_P v = opFail 'P' v

op_S (P o@(L _) e) = return (P (R o) e)
op_S (P o@(R _) e) = return (P (R o) e)
op_S (P o e) | observable o = return (P (L o) e)
op_S v = opFail 'S' v

op_B (P o@(B _ _) e) = return (P (R o) e)
op_B (P o e) | observable o = return (P (L o) e)
op_B v = opFail 'B' v

op_N (P o@(N _) e) = return (P (R o) e)
op_N (P o e) | observable o = return (P (L o) e)
op_N v = opFail 'N' v

op_GT :: (Monad c) => V c -> c (V c)
op_GT v@(P x (P y e)) =
    case tryGT y x of
        Just True -> return (P (R (P x y)) e)
        Just False -> return (P (L (P y x)) e)
        Nothing -> opFail '>' v
op_GT v = opFail '>' v

tryGT :: V c -> V c -> Maybe Bool
tryGT (N y) (N x) = Just (y > x)
tryGT (P y1 y2) (P x1 x2) =
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

-- greater-than due to structure (partial ordering)
structGT :: V c -> V c -> Bool
structGT (P _ _) x = is_num x || is_sum x
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

