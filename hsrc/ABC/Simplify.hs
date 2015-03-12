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
sz l (Op_SP : r) = sz l r -- elim SP
sz l (Op_LF : r) = sz l r -- elim LF
sz (opl:l) (opr:r) | opsCancel opl opr = sz l r
sz l (BL ops : r) = sz (BL ops' : l) r where
    ops' = simplify ops -- deep simplify
sz (Op_w : Op_z : l) (Op_z : r) = -- zwz = wzw
    sz l (Op_w : Op_z : Op_w : r) 
sz (Op_W : Op_Z : l) (Op_Z : r) = -- ZWZ = WZW
    sz l (Op_W : Op_Z : Op_W : r) 

-- inline functions...
sz (BL ops : l) (Op_v : Op_r : Op_ap : Op_c : r) = sz l (ops ++ r)
sz (BL ops : Op_v : l) (Op_ap : Op_c : r) = sz l (ops ++ r)

sz l (op:r) = sz (op:l) r -- no simplifications found; move on
sz l [] = reverse l -- all done!
