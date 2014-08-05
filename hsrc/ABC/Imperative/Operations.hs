{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
-- | Operations for the Imperative implementation of ABC.
--
-- These use very short names, so they shouldn't be exposed in
-- arbitrary contexts (or they'll conflict).
-- 
-- All operators except for SP and LF are defined. In addition,
-- an operator 'apc' corresponds to the ABC sequence '$c' and
-- is essential for leveraging Haskell's tail-call optimization.
module ABC.Imperative.Operations
    ( l,r,w,z,v,c       -- l r w z v c
    , sL,sR,sW,sZ,sV,sC -- L R W Z V C
    , cp,rm             -- ^ %
    , add,neg,mul,inv,divQ -- + - * / Q
    , ap,co,qu,o        -- $ ? ' o
    , k,f               -- k f
    , sD,sF,sM,sK       -- D F M K
    , gt                -- >
    , n0                -- #
    , d0,d1,d2,d3,d4    -- 0 1 2 3 4
    , d5,d6,d7,d8,d9    -- 5 6 7 8 9

    , bl, tl, tok       -- BL, TL, Tok

    , apc               -- $c
    , sL',sR',sW',sZ',sV',sC' -- (sum ops apart from pairs)
    , onFst -- apply to first element
    
    ) where

import Control.Monad ((>=>))
import Data.Monoid
import qualified Data.Sequence as S
import ABC.Imperative.Value
import ABC.Imperative.Runtime
import ABC.Quote
import ABC.Operators (abcDivMod)

opFail :: (Monad m) => String -> V m -> m (V m)
opFail s v = fail $ s ++ " @ " ++ show v

l,r,w,z,v,c :: (Monad m) => Prog m
sL,sR,sW,sZ,sV,sC :: (Monad m) => Prog m
cp,rm :: (Monad m) => Prog m
add,neg,mul,inv,divQ,gt :: (Monad m) => Prog m
qu,o,k,f :: (Monad m) => Prog m
sD,sF,sM :: (Monad m) => Prog m
n0 :: (Monad m) => Prog m
d0,d1,d2,d3,d4,d5,d6,d7,d8,d9 :: (Monad m) => Prog m
ap,apc,co,sK :: (Monad m) => Prog m

-- | block literals require TWO encodings of the same program. 
--
--     The first is the raw ABC code for the program (as a string)
--     The second is the Haskell encoding
--
-- The two encodings must be equivalent up to performance. That is,
-- it must always be possible to interpret the code instead of run
-- the block, and the only difference should be performance. The ABC
-- code can be used to recover performance when composing lots of
-- small blocks.
--
-- The ABC encoding becomes useful for metaprogramming, where
-- a developer composes lots of blocks at runtime. The result
-- can be optimized and JIT'd.
-- 
--
-- To keep the Haskell encoding tight, the program is 
-- provided as a string rather than a bulky list of [Op].
bl :: (Monad m) => String -> Prog m -> Prog m
bl s p = return . (P (B b)) where
    b = Block { b_aff = False, b_rel = False, b_prog = p 
              , b_code = S.fromList (read s) }

-- | text literals are relatively trivial. They quote a text value,
-- which has type `µT.(1+(Chr*T))` where Chr is a small integer in
-- range 0..0x10ffff. We translate the given string into the target
-- text value.
tl :: (Monad m) => String -> Prog m
tl s = return . P (textToVal s)

-- | tokens go straight to the Runtime
tok :: (Runtime m) => String -> Prog m
tok = invoke

n0 = (n_ 0)
d0 = (d_ 0)
d1 = (d_ 1)
d2 = (d_ 2)
d3 = (d_ 3)
d4 = (d_ 4)
d5 = (d_ 5)
d6 = (d_ 6)
d7 = (d_ 7)
d8 = (d_ 8)
d9 = (d_ 9)

n_ :: (Monad m) => Rational -> Prog m
n_ r = return . P (N r) 

d_ :: (Monad m) => Int -> Prog m
d_ d (P (N n) e) = return (P (N (nd_ n d)) e)
d_ _ v = opFail "digit" v

nd_ :: Rational -> Int -> Rational
nd_ n d = 10*n + fromIntegral d
{-# INLINE nd_ #-}


l (P a (P b c)) = return (P (P a b) c)
l v = opFail "l" v

r (P (P a b) c) = return (P a (P b c))
r v = opFail "r" v

w (P a (P b c)) = return (P b (P a c))
w v = opFail "w" v

z (P a (P b (P c d))) = return (P a (P c (P b d)))
z v = opFail "z" v

v a = return (P a U)

c (P a U) = return a
c v = opFail "c" v

sL = onFst "L" sL'
sR = onFst "R" sR'
sW = onFst "W" sW'
sZ = onFst "Z" sZ'
sV = onFst "V" sV'
sC = onFst "C" sC'

onFst :: (Monad m) => String -> Prog m -> Prog m
onFst _s f (P a e) = f a >>= \ a' -> return (P a' e)
onFst s _ v = opFail s v
{-# RULES
"onFst>=>onFst" forall s1 s2 f1 f2 . onFst s1 f1 >=> onFst s2 f2 = onFst (s1++s2) (f1 >=> f2)
 #-}

sL', sR', sW', sZ', sV', sC' :: (Monad m) => Prog m

sL' v@(L _a) = return (L v)
sL' (R (L b)) = return (L (R b))
sL' (R v@(R _c)) = return v
sL' v = opFail "L." v

sR' (L v@(L _a)) = return v
sR' (L (R b)) = return (R (L b))
sR' v@(R _c) = return (R v)
sR' v = opFail "R." v

sW' v@(L _a) = return (R v)
sW' (R v@(L _b)) = return v
sW' v@(R (R _c)) = return v
sW' v = opFail "W." v

sZ' v@(L _a) = return v
sZ' v@(R (L _b)) = return (R v)
sZ' (R v@(R (L _c))) = return v
sZ' v@(R (R (R _d))) = return v
sZ' v = opFail "Z." v

sV' = return . L
sC' (L a) = return a
sC' v = opFail "C." v

cp v@(P a _e) | copyable a = return (P a v)
cp v = opFail "^" v

rm (P a e) | droppable a = return e
rm v = opFail "%" v

add (P (N a) (P (N b) e)) = return (P (N (a+b)) e)
add v = opFail "+" v

neg (P (N a) e) = return (P (N (negate a)) e)
neg v = opFail "-" v

mul (P (N a) (P (N b) e)) = return (P (N (a*b)) e)
mul v = opFail "*" v

inv (P (N a) e) | (0 /= a) = return (P (N (recip a)) e)
inv v = opFail "/" v

divQ (P (N divisor) (P (N dividend) e)) = return result where
    result = (P (N remainder) (P (N quotient) e))
    (quotient,remainder) = dividend `abcDivMod` divisor
divQ v = opFail "Q" v

gt (P x@(N nx) (P y@(N ny) e)) =
    if (ny > nx) then return (P (R (P x y)) e)
                 else return (P (L (P y x)) e)
gt v = opFail ">" v


qu (P a e) = return (P (B block) e) where
    block = Block { b_aff = aff, b_rel = rel, b_code = code, b_prog = prog }
    aff = not (copyable a)
    rel = not (droppable a)
    code = (S.fromList . quote) a
    prog = return . (P a)
qu v = opFail "'" v

o (P (B bxy) (P (B byz) e)) = return (P (B bxz) e) where
    bxz = bxy `mappend` byz
o v = opFail "o" v

k (P (B b) e) = return (P (B b') e) where
    b' = b { b_rel = True }
k v = opFail "k" v

f (P (B b) e) = return (P (B b') e) where
    b' = b { b_aff = True }
f v = opFail "f" v

sD (P a (P (L b) e)) = return (P (L (P a b)) e)
sD (P a (P (R c) e)) = return (P (R (P a c)) e)
sD v = opFail "D" v

sF (P (L (P a b)) e) = return (P (L a) (P (L b) e))
sF (P (R (P c d)) e) = return (P (R c) (P (R d) e))
sF v = opFail "F" v

sM (P (L a ) e) = return (P a  e)
sM (P (R a') e) = return (P a' e)
sM v = opFail "M" v

ap (P (B b) (P a e)) = b_prog b a >>= \ a' -> return (P a' e)
ap v = opFail "$" v

co (P (B b) (P (L a) e)) | (not (b_rel b)) =
    b_prog b a >>= \ a' -> return (P (L a') e)
co (P (B b) v@(P (R _) _)) | (not (b_rel b)) = return v
co v = opFail "?" v

sK (P (R b) e) = return (P b e)
sK v = opFail "K" v

apc (P (B b) (P a U)) = b_prog b a -- can tail-call optimize
apc v = opFail "$c" v
