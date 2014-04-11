
-- | Trivial 'peephole' simplifier for ABC
--
-- This doesn't address partial evaluation, cycle recognition,
-- or other rich optimizations that ABC can support. However, 
-- even trivial simplification can help a lot for performance.
--
module ABC.Simplify 
    ( simplify
    ) where

import ABC.Operators

-- | eliminates all reversible subprograms, single pass!
simplify :: [Op] -> [Op] 
simplify = sz []

-- zipper-based simplifier 'walks' the list 
-- and simplifies between left and right
sz :: [Op] -> [Op] -> [Op]
sz l (OpC Op_SP : r) = sz l r -- elim SP
sz l (OpC Op_LF : r) = sz l r -- elim LF
sz l (BL ops : r) = sz (BL ops' : l) r where
    ops' = simplify ops -- deep simplify
sz (OpC Op_w : l) (OpC Op_w : r) = sz l r
sz (OpC Op_l : l) (OpC Op_r : r) = sz l r
sz (OpC Op_r : l) (OpC Op_l : r) = sz l r
sz (OpC Op_v : l) (OpC Op_c : r) = sz l r
sz (OpC Op_c : l) (OpC Op_v : r) = sz l r
sz (OpC Op_z : l) (OpC Op_z : r) = sz l r
sz (OpC Op_w : OpC Op_z : l) (OpC Op_z : r) = -- zwz = wzw
    sz l (OpC Op_w : OpC Op_z : OpC Op_w : r) 
sz (OpC Op_W : l) (OpC Op_W : r) = sz l r
sz (OpC Op_L : l) (OpC Op_R : r) = sz l r
sz (OpC Op_R : l) (OpC Op_L : r) = sz l r
sz (OpC Op_V : l) (OpC Op_C : r) = sz l r
sz (OpC Op_C : l) (OpC Op_V : r) = sz l r
sz (OpC Op_Z : l) (OpC Op_Z : r) = sz l r
sz (OpC Op_W : OpC Op_Z : l) (OpC Op_Z : r) = -- ZWZ = WZW
    sz l (OpC Op_W : OpC Op_Z : OpC Op_W : r) 
sz l (op:r) = sz (op:l) r -- no simplifications found; move on
sz l [] = reverse l -- all done!
