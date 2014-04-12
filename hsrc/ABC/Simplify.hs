{-# LANGUAGE PatternGuards #-}

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
sz (OpC opl:l) (OpC opr:r) | opsCancel opl opr = sz l r
sz l (BL ops : r) = sz (BL ops' : l) r where
    ops' = simplify ops -- deep simplify
sz (OpC Op_w : OpC Op_z : l) (OpC Op_z : r) = -- zwz = wzw
    sz l (OpC Op_w : OpC Op_z : OpC Op_w : r) 
sz (OpC Op_W : OpC Op_Z : l) (OpC Op_Z : r) = -- ZWZ = WZW
    sz l (OpC Op_W : OpC Op_Z : OpC Op_W : r) 
sz l (op:r) = sz (op:l) r -- no simplifications found; move on
sz l [] = reverse l -- all done!
