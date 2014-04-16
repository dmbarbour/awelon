{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
    ) where

import Data.Monoid
import qualified Data.Text as T
import Data.Text(Text)
import qualified Data.Sequence as S
import ABC.Imperative.Value
import ABC.Imperative.Runtime
import ABC.Quote

type PureProg m = V m -> V m

opFail :: (Monad m) => String -> V m -> m (V m)
opFail s v = fail $ s ++ " @ " ++ show v

opError :: String -> V m -> V m
opError s v = error $ s ++ " @ " ++ show v

onFst :: String -> (V m -> V m) -> V m -> V m
onFst _ f (P a e) = (P (f a) e)
onFst s _ v = opError s v
{-# INLINE onFst #-}

l,r,w,z,v,c :: (Functor m) => Prog m
sL,sR,sW,sZ,sV,sC :: (Functor m) => Prog m
cp,rm :: (Functor m) => Prog m
add,neg,mul,inv,divQ,gt :: (Functor m) => Prog m
qu,o,k,f :: (Functor m) => Prog m
sD,sF,sM :: (Functor m) => Prog m
n0 :: (Functor m) => Prog m
d0,d1,d2,d3,d4,d5,d6,d7,d8,d9 :: (Functor m) => Prog m
ap,apc,co,sK :: (Monad m) => Prog m
m_ap, m_apc, m_co, m_K :: (Monad m) => V m -> m (V m)

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
bl :: (Functor m) => (String,Prog m) -> Prog m
bl = fmap . P . B . mkB where
    mkB (s,p) = Block { b_aff = False, b_rel = False
                      , b_code = code s, b_prog = p }
    code = S.fromList . read

-- | text literals are relatively trivial. They quote a text value,
-- which has type `µT.(1+(Chr*T))` where Chr is a small integer in
-- range 0..0x10ffff. We translate the given string into the target
-- text type.
tl :: (Functor m) => String -> Prog m
tl = fmap . P . textToVal

-- | tokens will handle sealers before passing to `invoke`
-- (note: I might shift this responsibility into the runtime)
tok :: (Runtime m) => String -> Prog m
tok = invoke

l = fmap fl
r = fmap fr
w = fmap fw
z = fmap fz
v = fmap fv
c = fmap fc
sL = fmap fL
sR = fmap fR
sW = fmap fW
sZ = fmap fZ
sV = fmap fV
sC = fmap fC
cp = fmap fcp
rm = fmap frm
add = fmap fadd
neg = fmap fneg
mul = fmap fmul
inv = fmap finv
divQ = fmap fdivQ
gt = fmap fgt
qu = fmap fqu
o = fmap fo
k = fmap fk
f = fmap ff
sD = fmap fD
sF = fmap fF
sM = fmap fM
n0 = fmap (n_ 0)
d0 = fmap (d_ 0)
d1 = fmap (d_ 1)
d2 = fmap (d_ 2)
d3 = fmap (d_ 3)
d4 = fmap (d_ 4)
d5 = fmap (d_ 5)
d6 = fmap (d_ 6)
d7 = fmap (d_ 7)
d8 = fmap (d_ 8)
d9 = fmap (d_ 9)

ap  = (=<<) m_ap
co  = (=<<) m_co
sK  = (=<<) m_K
apc = (=<<) m_apc

n_ :: Rational -> PureProg m
n_ = P . N 
{-# NOINLINE n_ #-}

d_ :: Int -> PureProg m
d_ d (P (N n) e) = (P (N (nd_ n d)) e)
d_ _ v = opError "digit" v
{-# NOINLINE d_ #-}

nd_ :: Rational -> Int -> Rational
nd_ n d = 10*n + fromIntegral d
{-# INLINE nd_ #-}

fl,fr,fw,fz,fv,fc :: PureProg m

fl (P a (P b c)) = (P (P a b) c)
fl v = opError "l" v

fr (P (P a b) c) = (P a (P b c))
fr v = opError "r" v

fw (P a (P b c)) = (P b (P a c))
fw v = opError "w" v

fz (P a (P b (P c d))) = (P a (P c (P b d)))
fz v = opError "z" v

fv a = (P a U)

fc (P a U) = a
fc v = opError "c" v

fL,fR,fW,fZ,fV,fC :: PureProg m
fL',fR',fW',fZ',fV',fC' :: PureProg m

fL = onFst "L" fL'
fR = onFst "R" fR'
fW = onFst "W" fW'
fZ = onFst "Z" fZ'
fV = onFst "V" fV'
fC = onFst "C" fC'

fL' v@(L _a) = (L v)
fL' (R (L b)) = (L (R b))
fL' (R v@(R _c)) = v
fL' v = opError "L." v

fR' (L v@(L _a)) = v
fR' (L (R b)) = (R (L b))
fR' v@(R _c) = (R v)
fR' v = opError "R." v

fW' v@(L _a) = (R v)
fW' (R v@(L _b)) = v
fW' v@(R (R _c)) = v
fW' v = opError "W." v

fZ' v@(L _a) = v
fZ' v@(R (L _b)) = (R v)
fZ' (R v@(R (L _c))) = v
fZ' v@(R (R (R _d))) = v
fZ' v = opError "Z." v

fV' = L
fC' (L a) = a
fC' v = opError "C." v

fcp,frm :: PureProg m

fcp v@(P a _e) | copyable a = (P a v)
fcp v = opError "^" v

frm (P a e) | droppable a = e
frm v = opError "%" v

fadd,fneg,fmul,finv,fdivQ,fgt :: PureProg m

fadd (P (N a) (P (N b) e)) = (P (N (a+b)) e)
fadd v = opError "+" v

fneg (P (N a) e) = (P (N (negate a)) e)
fneg v = opError "-" v

fmul (P (N a) (P (N b) e)) = (P (N (a*b)) e)
fmul v = opError "*" v

finv (P (N a) e) | (0 /= a) = (P (N (recip a)) e)
finv v = opError "/" v

fdivQ (P (N divisor) (P (N dividend) e)) = result where
    result = (P (N remainder) (P (N quotient) e))
    (quotientI,remainder) = dividend `divModQ` divisor
    quotient  = fromIntegral quotientI
fdivQ v = opError "Q" v

fgt (P x@(N nx) (P y@(N ny) e)) =
    if (ny > nx) then (P (R (P x y)) e)
                 else (P (L (P y x)) e)
fgt v = opError ">" v


fqu :: (Functor m) => PureProg m
fqu (P a e) = (P (B block) e) where
    block = Block { b_aff = aff, b_rel = rel, b_code = code, b_prog = prog }
    aff = not (copyable a)
    rel = not (droppable a)
    code = (S.fromList . quote) a
    prog = fmap (P a)
fqu v = opError "'" v

fo,fk,ff :: PureProg m

fo (P (B byz) (P (B bxy) e)) = (P (B bxz) e) where
    bxz = bxy `mappend` byz
fo v = opError "o" v

fk (P (B b) e) = (P (B b') e) where
    b' = b { b_rel = True }
fk v = opError "k" v

ff (P (B b) e) = (P (B b') e) where
    b' = b { b_aff = True }
ff v = opError "f" v

fD,fF,fM :: PureProg m

fD (P a (P (L b) e)) = (P (L (P a b)) e)
fD (P a (P (R c) e)) = (P (R (P a c)) e)
fD v = opError "D" v

fF (P (L (P a b)) e) = (P (L a) (P (L b) e))
fF (P (R (P c d)) e) = (P (R c) (P (R d) e))
fF v = opError "F" v

fM (P (L a ) e) = (P a  e)
fM (P (R a') e) = (P a' e)
fM v = opError "M" v

m_ap (P (B b) (P a e)) = 
    b_prog b (return a) >>= \ a' -> return (P a' e)
m_ap v = opFail "$" v

m_co (P (B b) (P (L a) e)) | (not (b_rel b)) =
    b_prog b (return a) >>= \ a' -> return (P (L a') e)
m_co (P (B b) v@(P (R _) _)) | (not (b_rel b)) = return v
m_co v = opFail "?" v

m_K (P (R b) e) = return (P b e)
m_K v = opFail "K" v

m_apc (P (B b) (P a U)) = b_prog b (return a) -- can tail-call optimize
m_apc v = opFail "$c" v


-- data plumbing rules
{-# RULES
"l.r" fl . fr = id
"r.l" fr . fl = id
"v.c" fv . fc = id
"c.v" fc . fv = id
"w.w" fw . fw = id
"z.z" fz . fz = id

"L.R" fL . fR = id
"R.L" fR . fL = id
"V.C" fV . fC = id
"C.V" fC . fV = id
"W.W" fW . fW = id
"Z.Z" fZ . fZ = id
 #-}

-- static number computations
{-# RULES
"staticNum" forall d n . d_ d . n_ n        = n_ (nd_ n d)
"staticAdd" forall a b . fadd . n_ a . n_ b = n_ (a + b)
"staticNeg" forall a   . fneg . n_ a        = n_ (negate a)
"staticMul" forall a b . fmul . n_ a . n_ b = n_ (a * b)
"staticInv" forall a   . finv . n_ a        = n_ (recip a)
 #-}

