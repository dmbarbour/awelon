
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
    , add,neg,mul,div,dQ -- + - * / Q
    , ap,co,qu,o        -- $ ? ' o
    , k,f               -- k f
    , sD,sF,sM,sK       -- D F M K
    , n0                -- #
    , d0,d1,d2,d3,d4    -- 0 1 2 3 4
    , d5,d6,d7,d8,d9    -- 5 6 7 8 9
    , bl, tl, tok       -- BL, TL, Tok
    , apc               -- $c
    ) where

import qualified Data.Text as T
import qualified Data.Sequence as S
import ABC.Imperative.Value
import ABC.Imperative.Runtime

type PureProg m = V m -> V m

l,r,w,z,v,c :: (Functor m) => Prog m
fl,fr,fw,fz,fv,fc :: PureProg m

sL,sR,sW,sZ,sV,sC :: (Functor m) => Prog m
fL,fR,fW,fZ,fV,fC :: PureProg m

cp,rm :: (Functor m) => Prog m
fcp,frm :: PureProg m

add,neg,mul,div,dQ :: (Functor m) => Prog m
fadd,fneg,fmul,fdiv,fdQ :: PureProg m

qu,o,k,f :: (Functor m) => Prog m
fqu,fo,fk,ff :: PureProg m

sD,sF,sM :: (Functor m) => Prog m
fD,fF,fM :: PureProg m

n0 :: (Functor m) => Prog m
d0,d1,d2,d3,d4,d5,d6,d7,d8,d9 :: (Functor m) => Prog m
n_,d_ :: Integer -> PureProg m

-- monadic operators
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
bl :: (String,Prog m) -> Prog m
bl = fmap . P . B . mkB where
    mkB (s,p) = Block { b_aff = False, b_rel = False
                      , b_code = code s, b_prog = p }
    code = S.fromList . read

-- | text literals are relatively trivial. They quote a text value,
-- which has type `µT.(1+(Chr*T))` where Chr is a small integer in
-- range 0..0x10ffff. We translate the given string into the target
-- text type.
tl :: String -> Prog m
tl = fmap . P . textToVal

-- | tokens will handle sealers before passing to `invoke`
tok :: (Runtime m) => String -> Prog m
tok (':':s) = fmap (S (T.pack s))
tok ('.':s) = (=<<) (m_unseal s)
tok s = invoke s

m_unseal :: (Monad m) => String -> V m -> m (V m)
m_unseal s (S s' v) | (s == s') = return v
m_unseal s v = opFail ("{." ++ s ++ "}") v

opFail :: (Monad m) => String -> V m -> m (V m)
opFail s v = fail $ s ++ " @ " ++ show v

opError :: String -> V m -> V m
opError s v = error $ s ++ " @ " ++ show v

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
div = fmap fdiv
dQ = fmap fdQ
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

n_ = P . N . fromIntegral
{-# NOINLINE n_ #-}

d_ d (P (N n) e) = (P (N (10*n+d)) e)
d_ _ v = opError "digit" v
{-# NOINLINE d_ #-}

-- aim here is fusion of digit operations
nd_ :: Integer -> Integer -> Integer
nd_ n d = 10*n+d
{-# INLINE nd_ #-}

-- number generation rules
{-# RULES
"staticNumber" d_ d . n_ n = n_ (nd_ n d)
 #-}

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


ap = (=<<) m_ap
co = (=<<) m_co
sK = (=<<) m_K
apc = (=<<) m_apc

m_ap (P (B b) (P a e)) = 
    b_prog b (return a) >>= \ a' -> return (P a' e)
m_ap v = opFail "$" v

m_co (P (B b) (P (L a) e)) | droppable b =
    b_prog b (return a) >>= \ a' -> return (P (L a') e)
m_co (P (B b) v@(P (R _) _)) | droppable b = return v
m_co v = opFail "?" v

m_K (P (R b) e) = return (P b e)
m_K v = opFail "K" v

m_apc (P (B b) (P a U)) = b_prog b (return a) -- can tail-call optimize
m_apc v = opFail "$c" v

