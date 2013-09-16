{-# LANGUAGE TypeOperators, EmptyDataDecls, FlexibleInstances,
             MultiParamTypeClasses, FunctionalDependencies, 
             DeriveDataTypeable, NoMonomorphismRestriction
 #-}

-- | This module describes a very heterogeneous approach to
-- modeling navigation of and operations on large structures, 
-- including full environments.
--
-- Background: This model is based on Awelon's structure, but is
-- simplified by assuming a single partition.
module HLens
    ( (:*:), (:+:), U, Z, V
    , N, A, R, L
    , (>>>) -- from Control.Category

    -- basics
    , swap, intro1, elim1, assocl, rot3, first
    , assocr, rot2, rot4, rot5, rot6, pzip, second
    , mirror, intro0, elim0, assocls, rot3s, left
    , assocrs, rot2s, rot4s, rot5s, rot6s, pzips, right
    , copy, drop, conjoin, disjoin, merge1, merge
    , arr, fromPair, toPair, fromEither, toEither
    , noCopy, noDrop, makeAffine, makeRelevant, makeLinear
    , capture, compose
    , assertSum, assertProd, assertVal

    -- zippers
    , zwrap, zunwrap, zf, zuf, zs, zus
    , szwrap, szunwrap, szf, szuf, szs, szus

    
    
    ) where

import Prelude (const)
import Data.Typeable (Typeable)
import Data.Either 
import Control.Category

-- | BASIC STRUCTURES
--    These are really just pairs, eithers, and functions.
--
--    But they are wrapped to prevent direct access, to
--    protect linearity properties for example. 
--
data (:*:) a b = P a b  deriving (Typeable)
data (:+:) a b = L a | R b deriving (Typeable)
data (:~>) a b = Pure (a -> b) deriving (Typeable)
data U = U deriving (Typeable) -- unit; identity for (:*:)
data Z     deriving (Typeable) -- zero; identity for (:+:)

instance Show U where show = "U"
instance Show Z where show = "ERROR!Z"

instance (Show a, Show b) => Show (a :*: b) where
    show (P a b) = "(" ++ show a ++ " :*: " ++ show b ++ ")"
instance (Show a, Show b) => Show (a :+: b) where
    show (L a) = "(" ++ show a ++ " :+: {inactive})"
    show (R b) = "({inactive} :+: " ++ show b ++ ")"


infixr 3 :*:
infixr 2 :+:
infixr 1 :~>


-- | Haskell values in the model
--    Type 'x' is the Haskell value type.
--    Type 'l' describes substructural properties.
--          N is normal: allows copy or drop
--          A is affine: no copy
--          R is relevant: no drop
--          L is linear: no copy, no drop 
data V l x = V x

data N -- normal
data A -- affine
data R -- relevant
data L -- linear

instance Category (:~>) where
    id = Pure id
    (.) (Pure f) (Pure g) = Pure (f . g)

-- | PRIMITIVE DATA PLUMBING 
intro1  :: a :~> (U :*: a)
elim1   :: (U :*: a) :~> a
swap    :: (a :*: b) :~> (b :*: a)
assocl  :: (a :*: (b :*: c)) :~> ((a :*: b) :*: c)
rot3    :: (a :*: (b :*: (c :*: d))) :~> (c :*: (a :*: (b :*: d)))

intro0  :: a :~> (Z :+: a)
elim0   :: (Z :+: a) :~> a
mirror  :: (a :+: b) :~> (b :+: a)
assocls :: (a :+: (b :+: c)) :~> ((a :+: b) :+: c)
rot3s   :: (a :+: (b :+: (c :+: d))) :~> (c :+: (a :+: (b :+: d)))

-- BASIC DATA PLUMBING 
assocr  :: ((a :*: b) :*: c) :~> (a :*: (b :*: c))
rot2    :: (a :*: (b :*: c)) :~> (b :*: (a :*: c))
rot4    :: (a :*: (b :*: (c :*: (d :*: e)))) :~> (d :*: (a :*: (b :*: (c :*: e))))
pzip    :: ((a :*: b) :*: (c :*: d)) :~> ((a :*: c) :*: (b :*: d))

assocrs :: ((a :+: b) :+: c) :~> (a :+: (b :+: c))
rot2s   :: (a :+: (b :+: c)) :~> (b :+: (a :+: c))
rot4s   :: (a :+: (b :+: (c :+: (d :+: e)))) :~> (d :+: (a :+: (b :+: (c :+: e))))
pzips   :: ((a :+: b) :+: (c :+: d)) :~> ((a :+: c) :+: (b :+: d))

assocr = swap . assocl . swap . assocl . swap
rot2 = intro1 >>> rot3 >>> intro1 >>> rot3 >>> elim1 >>> elim1
rot4 = assocl >>> rot3 >>> rot2 >>> assocr >>> rot3
rot5 = assocl >>> rot4 >>> rot2 >>> assocr >>> rot3
rot6 = assocl >>> rot5 >>> rot2 >>> assocr >>> rot3
pzip = assocr >>> rot3 >>> rot2 >>> assocl

assocrs = mirror . assocls . mirror . assocls . mirror
rot2s = intro0 >>> rot3s >>> intro0 >>> rot3s >>> elim0 >>> elim0
rot4s = assocls >>> rot3s >>> rot2s >>> assocrs >>> rot3s
rot5s = assocls >>> rot4s >>> rot2s >>> assocrs >>> rot3s
rot6s = assocls >>> rot5s >>> rot2s >>> assocrs >>> rot3s
pzips = assocrs >>> rot3s >>> rot2s >>> assocls


-- | SUM-PRODUCT INTERACTIONS
--
-- In a distributed environment, 'disjoin' would be constrained to
-- address the issue of moving the choice to the data.
--
-- Note that 'distrib' loses information. Usually, it is used only
-- as part of copying a sum type, in which case there is no total loss.
-- A more naive distrib, like (x * (y + z)) ~> ((x * y) + (x * z)) is
-- too powerful because it implicitly copies x).  
-- 
--
conjoin :: ((x :*: y) :+: (x :*: z)) :~> (x :*: (y :+: z))
disjoin :: (x :*: (y :+: z)) :~> ((x :*: y) :+: (x :*: z))
distrib :: ((a :*: b) :+: (c :*: d)) :~> ((a :+: c) :*: (b :+: d))
merge1  :: (U :+: U) :~> U


-- (x + (y * y)) -> ((x + y) * (x + y))? no, this drops y
-- ((x * x) + (y * y))


merge :: (x :+: x) :~> x
merge = q (intro1 >>> swap) >>> left >>> mirror >>>
        q (intro1 >>> swap) >>> left >>> conjoin >>>
        swap >>> q merge1 >>> first >>> elim1

-- | PRIMITIVE QUOTATION
q :: v -> (e :~> ((V N v) :*: e))

-- | OPERATING ON ATOMIC HASKELL VALUES
arr :: V l (a -> b) :~> V l (V N a :~> V N b)
fromPair :: V l (a,b) :~> (V l a :*: V l b)
toPair :: (V l a :*: V l b) :~> V l (a,b)
fromEither :: V l (Either a b) :~> (V l a :+: V l b)
toEither :: (V l a :+: V l b) :~> V l (Either a b)

-- | HIGHER ORDER PROGRAMMING AND STAGING
capture :: V l a :~> V l (U :~> V l a)
compose :: (V l (x :~> y) :*: V l (y :~> z)) :~> V l (x :~> z)

-- | SUBSTRUCTURE - control copy and drop operations
-- (simple, and surprisingly expressive for safety)
--
-- Substructure is intended for blocks (values of type (x :~> y)).
-- Control drop with `noDrop` and copy with `noCopy`.
--
-- Ugh, Haskell is so inexpressive for this stuff.

class Copy x where copy :: x :~> (x :*: x)
class Drop x where drop :: x :~> U

instance Copy (V N x) where copy = copyN
instance Copy (V R x) where copy = copyR
instance (Copy x, Copy y) => Copy (x :*: y) where 
    copy = q copy >>> first >>> swap >>> 
           q copy >>> first >>> swap >>> pzip
instance (Copy x, Copy y) => Copy (x :+: y) where
    copy = q copy >>> left >>> mirror >>>
           q copy >>> left >>> mirror >>> distrib

instance Drop (V N x) where drop = dropN
instance Drop (V A x) where drop = dropA
instance (Drop x, Drop y) => Drop (x :*: y) where
    drop = q drop >>> first >>> elim1 >>> drop
instance (Drop x, Drop y) => Drop (x :+: y) where
    drop = q drop >>> left >>> mirror >>> 
           q drop >>> left >>> merge1

class NoDrop x x' | x -> x' where noDrop :: x :~> x'
class NoCopy x x' | x -> x' where noCopy :: x :~> x'

instance NoDrop (V N x) (V R x) where noDrop = noDropN
instance NoDrop (V A x) (V L x) where noDrop = noDropA
instance NoDrop (V R x) (V R x) where noDrop = id
instance NoDrop (V L x) (V L x) where noDrop = id

--instance (NoDrop x x', NoDrop y y') => NoDrop (x :*: y) (x' :*: y') where
--    noDrop = q noDrop >>> first >>> swap >>> q noDrop >>> first >>> swap
--instance (NoDrop x x', NoDrop y y') => NoDrop (x :+: y) (x' :+: y') where
--    noDrop = q noDrop >>> left >>> mirror >>> q noDrop >>> left >>> mirror

instance NoCopy (V N x) (V A x) where noCopy = noCopyN
instance NoCopy (V R x) (V L x) where noCopy = noCopyR
instance NoCopy (V A x) (V A x) where noCopy = id
instance NoCopy (V L x) (V L x) where noCopy = id

--instance (NoCopy x x', NoCopy y y') => NoCopy (x :*: y) (x' :*: y') where
--    noCopy = q noCopy >>> first >>> swap >>> q noCopy >>> first >>> swap
--instance (NoCopy x x', NoCopy y y') => NoCopy (x :+: y) (x' :+: y') where
--    noCopy = q noCopy >>> left >>> mirror >>> q noCopy >>> left >>> mirror

makeAffine = noCopy
makeRelevant = noDrop
makeLinear = noCopy >>> noDrop

-- | PRIMITIVE COMBINATORS
--
-- The combinators in this case must be held in the environment
-- prior to execution. This supports a more compositional model
-- than Arrows, albeit a less restrained environment.
first   :: (V l (a :~> a') :*: (a :*: b)) :~> (a' :*: b)
left    :: (V l (a :~> a') :*: (a :+: b)) :~> (a' :+: b)

-- DERIVED COMBINATORS
second  :: (V l (b :~> b') :*: (a :*: b)) :~> (a :*: b')
second = assocl >>> swap >>> rot2 >>> first >>> swap

right :: (V l (b :~> b') :*: (a :+: b)) :~> (a :+: b')
right = q mirror >>> second >>> left >>> mirror

-- | SIMPLE TYPE ASSERTIONS
assertProd :: (x :*: y) :~> (x :*: y)
assertProd = id

assertSum :: (x :+: y) :~> (x :+: y)
assertSum = id

assertVal :: (V l x) :~> (V l x)
assertVal = id

-- | ZIPPER OPERATIONS
--
-- Here we have a heterogeneous, type-level zipper. Haskell's type
-- system isn't really well designed to handle types that change on
-- every operation. 
--
-- Here I model zippers as:
--
--    target :*: (a :*: b)  (or for sums)
--
-- The target is the current object of attention. Initially, it is
-- the whole object. But developers are free to step into structured
-- objects to manipulate just part of them. 
--    
--    zwrap :: x ~> x*(1*1) (rv. zunwrap)
--    zf    :: (x*y)*(l*r) ~> x*(1*(y*(l*r))) (rv. zuf)
--    zs    :: (x*y)*(l*r) ~> y*((x*l)*r)     (rv. zus)
--    zu is one step back (selects zf or zs)
--    zuN is all steps back
zwrap :: x :~> (x :*: (U :*: U))
zwrap = intro1 >>> intro1 >>> assocl >>> swap
zunwrap = swap >>> assocr >>> elim1 >>> elim1

zf :: (x :*: y) :*: (l :*: r) :~> x :*: (U :*: (y :*: (l :*: r)))
zf =  assocr >>> intro1 >>> rot2
zuf = rot2 >>> elim1 >>> assocl

zs :: (x :*: y) :*: (l :*: r) :~> y :*: ((x :*: l) :*: r)
zs = pzip >>> rot2
zus = rot2 >>> pzip

szwrap :: x :~> (x :+: (Z :+: Z))
szwrap = intro0 >>> intro0 >>> assocls >>> mirror
szunwrap = mirror >>> assocrs >>> elim0 >>> elim0

szf :: (x :+: y) :+: (l :+: r) :~> x :+: (Z :+: (y :+: (l :+: r)))
szf = assocrs >>> intro0 >>> rot2s
szuf = rot2s >>> elim0 >>> assocls

szs :: (x :+: y) :+: (l :+: r) :~> y :+: ((x :+: l) :+: r)
szs = pzips >>> rot2s
szus = rot2s >>> pzips

class ZU x x' | x -> x' where zu :: x :~> x'
instance ZU (x :*: (U :*: (y :*: (l :*: r)))) ((x :*: y) :*: (l :*: r)) where zu = zuf
instance ZU (y :*: ((x :*: l) :*: r)) ((x :*: y) :*: (l :*: r)) where zu = zus

-- really, type-level programming in Haskell seems too painful
--  I would need UndecidableInstances for this to work.
--class ZUN x x' | x -> x' where zuN :: x ~> (x' :*: (U :*: U))
--instance ZUN (x :*: (U :*: U)) (x :*: (U :*: U)) where zuN = id
--instance (ZU x x', ZUN x' y) => ZUN x y where zuN = zu >>> zuN


-- PRIMITIVE DEFINITIONS
intro1f = P U 
elim1f (P U a) = a
swapf (P a b) = (P b a)
assoclf (P a (P b c)) = (P (P a b) c)
rot3f (P a (P b (P c d))) = (P c (P a (P b d)))

intro1 = Pure intro1f
elim1 = Pure elim1f
swap = Pure swapf
assocl = Pure assoclf
rot3 = Pure rot3f

intro0f = R
elim0f (R a) = a
mirrorf (R a) = (L a)
mirrorf (L a) = (R a)
assoclsf (L a) = (L (L a))
assoclsf (R (L b)) = (L (R b))
assoclsf (R (R c)) = R c
rot3sf (L a) = R (L a)
rot3sf (R (L b)) = R (R (L b))
rot3sf (R (R (L c))) = L c
rot3sf (R (R (R d))) = (R (R (R d)))

intro0 = Pure intro0f
elim0 = Pure elim0f
mirror = Pure mirrorf
assocls = Pure assoclsf
rot3s = Pure rot3sf

-- Operating on Haskell values.
qf v e = P (V v) e
arr' (V f) = (V (Pure f')) 
    where f' (V a) = V (f a)
openpf (V (a,b)) = P (V a) (V b)
shutpf (P (V a) (V b)) = V (a,b)
opensf (V (Left a)) = L (V a)
opensf (V (Right b)) = R (V b)
shutsf (L (V a)) = V (Left a)
shutsf (R (V b)) = V (Right b)
capturef = V . Pure . const
composef (P (V (Pure fxy)) (V (Pure fyz))) = V (Pure (fyz . fxy))

q = Pure . qf
arr = Pure arr'
fromPair = Pure openpf
toPair = Pure shutpf
fromEither = Pure opensf
toEither = Pure shutsf
capture = Pure capturef
compose = Pure composef

-- these affect a type tag, not the value
copyR' v = P v v
copyN' v = P v v
dropA' = const U
dropN' = const U
noDropN' (V x) = V x
noDropA' (V x) = V x
noCopyN' (V x) = V x
noCopyR' (V x) = V x
copyN = Pure copyN'
copyR = Pure copyR'
dropN = Pure dropN'
dropA = Pure dropA'
noDropN = Pure noDropN'
noDropA = Pure noDropA'
noCopyN = Pure noCopyN'
noCopyR = Pure noCopyR'

-- conjoin :: ((x :*: y) :+: (x :*: z)) :~> (x :*: (y :+: z))
conjoinf (L (P x y)) = P x (L y)
conjoinf (R (P x y)) = P x (R y)
conjoin = Pure conjoinf

-- disjoin :: (x :*: (y :+: z)) :~> ((x :*: y) :+: (x :*: z))
disjoinf (P x (L y)) = L (P x y)
disjoinf (P x (R z)) = R (P x z)
disjoin = Pure disjoinf

-- distrib :: ((a :*: b) :+: (c :*: d)) :~> ((a :+: c) :*: (b :+: d))
-- loses information in general case, i.e. cannot rebuild (a :*: b)
distribf (L (P a b)) = P (L a) (L b)
distribf (R (P c d)) = P (R c) (R d)
distrib = Pure distribf

merge1f (L U) = U
merge1f (R U) = U
merge1 = Pure merge1f

-- COMBINATORS!
firstf (P (V (Pure fn)) (P a b)) = P (fn a) b
leftf (P (V (Pure fn)) (L a)) = L (fn a)
leftf (P _ (R b)) = R b
first = Pure firstf
left = Pure leftf

{-# RULES
"swap.swap"  swap . swap = id
"intro1.elim1" intro1 . elim1 = id
"elim1.intro1" elim1 . intro1 = id
"assocl.assocr" assocl . assocr = id
"assocr.assocl" assocr . assocl = id
"rot3.rot3.rot3" rot3 . rot3 . rot3 = id
"pzip.pzip" pzip . pzip = id
"rot2.rot2" rot2 . rot2 = id
"mirror.mirror" mirror . mirror = id
"intro0.elim0" intro0 . elim0 = id
"elim0.intro0" elim0 . intro0 = id
"assocls.assocrs" assocls . assocrs = id
"assocrs.assocls" assocrs . assocls = id
"rot3s.rot3s.rot3s" rot3s . rot3s . rot3s = id
"rot2s.rot2s" rot2s . rot2s = id
"pzips.pzips" pzips . pzips = id
"conjoin.disjoin" conjoin . disjoin = id
"disjoin.conjoin" disjoin . conjoin = id
 #-}




