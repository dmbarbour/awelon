{-# LANGUAGE TypeOperators, EmptyDataDecls, DeriveDataTypeable, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
    PatternGuards #-}

-- | AO Runtime 
-- 
-- So... it comes to this.
--
-- The ABC interpreter is, in a word, slow. A simple test of the
-- version 0.5.2 code - `[] 10000 repeat` (repeating an empty 
-- action 10k times) took about 6 seconds on my (decent) machine, 
-- or just over 0.6 ms per loop (using the fixpoint implementation
-- of repeat). That's fast enough for small, interactive programs. 
-- But I need 100x that performance for a reasonable bootstrap.
--
-- My goal, at the moment, is to implement a trivial compiler to
-- Haskell, and leverage GHC's powerful code and rewrite features
-- ... and type-checking (where possible). The whole AO dictionary
-- is compiled to a single file, which imports AORT for the ABC
-- definitions.
--
module AO.AORT
    ( (:*), (:+), Void, Unit(..), Sealed(..)
    , abc_l, abc_r, abc_w, abc_z, abc_v, abc_c
    , abc_L, abc_R, abc_W, abc_Z, abc_V, abc_C
    , abc_drop, abc_copy
    , abc_quote, quote, abc_o, abc_apply, abc_k, abc_f
    , abc_add, abc_mul, abc_neg, abc_inv, abc_Q
    , abc_cond
    , abc_D, abc_F, abc_M, abc_K
    , abc_P, abc_S, abc_B, abc_N
    , abc_gt
    , abc_seal, abc_unseal
    ) where

import Data.Typeable
import Data.Text (Text)
import Data.Ratio 
import qualified Data.Text as T

data (:*) a b = !a :* !b deriving (Typeable, Eq)
data (:+) a b = InL !a | InR !b deriving (Typeable, Eq)
data Void deriving (Typeable)
data Unit = Unit deriving (Typeable, Eq)
data Sealed v = Sealed !Text !v deriving (Typeable)

abc_l :: (a :* (b :* c)) -> ((a :* b) :* c)
abc_l    (a :* (b :* c)) =  ((a :* b) :* c)

abc_r :: ((a :* b) :* c) -> (a :* (b :* c)) 
abc_r    ((a :* b) :* c) =  (a :* (b :* c))

abc_w :: (a :* (b :* c)) -> (b :* (a :* c))
abc_w    (a :* (b :* c)) = (b :* (a :* c))

abc_z :: (a :* (b :* (c :* d))) -> (a :* (c :* (b :* d)))
abc_z    (a :* bcd) = (a :* abc_w bcd)

abc_v :: a -> (a :* Unit)
abc_v    a =  (a :* Unit)

abc_c :: (a :* Unit) -> a
abc_c    (a :* Unit) =  a

abc_L :: (a :+ (b :+ c)) -> ((a :+ b) :+ c)
abc_L (InL a) = InL (InL a)
abc_L (InR (InL b)) = InL (InR b)
abc_L (InR (InR c)) = InR c

abc_R :: ((a :+ b) :+ c) -> (a :+ (b :+ c))
abc_R (InL (InL a)) = InL a
abc_R (InL (InR b)) = InR (InL b)
abc_R (InR c) = InR (InR c)

abc_W :: (a :+ (b :+ c)) -> (b :+ (a :+ c))
abc_W (InL a) = InR (InL a)
abc_W (InR (InL b)) = InL b
abc_W (InR (InR c)) = InR (InR c)

abc_Z :: (a :+ (b :+ (c :+ d))) -> (a :+ (c :+ (b :+ d)))
abc_Z (InL a) = InL a
abc_Z (InR bcd) = InR (abc_W bcd)

abc_V :: a -> (a :+ b)
abc_V a = InL a

abc_C :: (a :+ Void) -> a
abc_C (InL a) = a
abc_C _ = error "in Void at operator C"

-- % - assumes dropable
abc_drop :: (a :* b) -> b
abc_drop (_ :* b) = b

-- ^ - assumes copyable
abc_copy :: (a :* b) -> (a :* (a :* b))
abc_copy (a :* b) = (a :* (a :* b))

-- $ - assumes pure, for now
--  (or using unsafePerformIO if needed)
abc_apply :: ((a->b) :* (a :* e)) -> (b :* e) 
abc_apply (f :* (a :* e)) = (f a :* e)

-- '
abc_quote :: (a :* e) -> ((s -> (a :* s)) :* e)
abc_quote (a :* e) = (quote a :* e) 

quote :: a -> s -> (a :* s)
quote = (:*)

-- o
abc_o :: ((b->c) :* ((a->b) :* e)) -> ((a->c) :* e)
abc_o (f :* (g :* e)) = ((f . g) :* e)

-- AORT does not enforce affine (f) and relevant (k) properties
-- (beyond ensuring they apply to a block)
abc_k, abc_f :: ((a->b) :* e) -> ((a->b) :* e)
abc_k = id
abc_f = id

-- + *
abc_add, abc_mul :: (Rational :* (Rational :* e)) -> (Rational :* e)
abc_add (x :* (y :* e)) = ((x + y) :* e)
abc_mul (x :* (y :* e)) = ((x * y) :* e)

-- - /
abc_neg, abc_inv :: (Rational :* e) -> (Rational :* e)
abc_neg (x :* e) = ((negate x) :* e)
abc_inv (x :* e) = ((recip x) :* e)

-- ABC's flexible divMod operation
abc_Q :: (Rational :* (Rational :* e)) -> (Rational :* (Rational :* e))
abc_Q (b :* (a :* e)) =
    let (r,q) = divModQ b a in
    (r :* ((fromIntegral q) :* e))

divModQ :: Rational -> Rational -> (Rational, Integer)
divModQ b a = 
    let num = numerator a * denominator b in
    let den = numerator b * denominator a in
    let (qN,rN) = num `divMod` den in
    let denR = denominator a * denominator b in
    (rN % denR, qN)

-- ?
abc_cond :: ((a->a') :* ((a :+ b) :* e)) -> ((a' :+ b) :* e)
abc_cond (f :* (InL a :* e)) = InL (f a) :* e
abc_cond (_ :* (InR b :* e)) = InR b :* e

-- merge for AORT is very conservative. This isn't necessarily a bad
-- thing, but it will take some extra work.
abc_D :: (a :* ((b :+ c) :* e)) -> ((a :* b) :+ (a :* c)) :* e
abc_F :: ((a :* b) :+ (c :* d)) :* e -> (a :+ c) :* ((b :+ d) :* e)
abc_M :: (a :+ a) :* e -> (a :* e)
abc_K :: (a :+ b) :* e -> (b :* e)

abc_D (a :* (InL b :* e)) = InL (a :* b) :* e
abc_D (a :* (InR c :* e)) = InR (a :* c) :* e
abc_F (InL (a :* b) :* e) = InL a :* (InL b :* e)
abc_F (InR (c :* d) :* e) = InR c :* (InR d :* e)
abc_M (InL a :* e) = a :* e
abc_M (InR a :* e) = a :* e
abc_K (InL _ :* _) = error "assertion failure!"
abc_K (InR b :* e) = b :* e

class Observable o where
    isN, isP, isS, isB :: o -> (o :+ o)
    isN = InL
    isP = InL
    isS = InL
    isB = InL

instance Observable (a :* b) where isP = InR
instance Observable (a :+ b) where isS = InR
instance Observable (a -> b) where isB = InR
instance Observable Rational where isN = InR

abc_P, abc_S, abc_B, abc_N :: (Observable o) => (o :* e) -> ((o :+ o) :* e)
abc_P (o :* e) = isP o :* e
abc_S (o :* e) = isS o :* e
abc_B (o :* e) = isB o :* e
abc_N (o :* e) = isN o :* e

-- AORT supports only a conservative set of comparisons
instance Ord Unit where compare _ _ = EQ
instance (Ord a, Ord b) => Ord (a :* b) where
    compare (a1 :* b1) (a2 :* b2) =
        case compare a1 a2 of
            EQ -> compare b1 b2
            x -> x
instance (Ord a, Ord b) => Ord (a :+ b) where
    compare (InL _) (InR _) = LT
    compare (InR _) (InL _) = GT
    compare (InL a1) (InL a2) = compare a1 a2
    compare (InR b1) (InR b2) = compare b1 b2

abc_gt :: (Ord a) => (a :* (a :* e)) -> ((a :* a) :+ (a :* a)) :* e
abc_gt (x :* (y :* e)) =
    if y > x then InR (x :* y) :* e
             else InL (y :* x) :* e


abc_seal :: Text -> a -> Sealed a
abc_seal = Sealed

abc_unseal :: Text -> Sealed a -> a
abc_unseal t (Sealed tok a) | (t == tok) = a
abc_unseal t (Sealed tok _) = error $ 
    "wrong unsealer: {$" ++ T.unpack tok ++ 
    "}{/" ++ T.unpack t ++ "}"


 