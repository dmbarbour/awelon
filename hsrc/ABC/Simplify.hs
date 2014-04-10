
-- | Trivial 'peephole' simplifiers for ABC
--
-- This doesn't address partial evaluation, cycle recognition,
-- or other rich optimizations that ABC can support. However, 
-- even trivial simplification can help a lot for performance.
--
module ABC.Simplify 
    ( simplify
    ) where

import ABC.Operators

-- remove spaces from the entire operations sequence
-- (doesn't touch deep blocks)
removeSpaces :: [Op] -> [Op]
removeSpaces (OpC Op_LF : ops) = removeSpaces ops
removeSpaces (OpC Op_SP : ops) = removeSpaces ops
removeSpaces (op:ops) = op:(removeSpaces ops)
removeSpaces [] = []

-- single pass to remove common data plumbing
simplifyP :: [Op] -> [Op]
simplifyP (OpC Op_w : OpC Op_w : ops) = simplifyP ops
simplifyP (OpC Op_l : OpC Op_r : ops) = simplifyP ops
simplifyP (OpC Op_r : OpC Op_l : ops) = simplifyP ops
simplifyP (OpC Op_v : OpC Op_c : ops) = simplifyP ops
simplifyP (OpC Op_c : OpC Op_v : ops) = simplifyP ops
simplifyP (OpC Op_z : OpC Op_z : ops) = simplifyP ops
simplifyP (OpC Op_z : OpC Op_w : OpC Op_z : ops) =
    (OpC Op_w : simplifyP (OpC Op_z : OpC Op_w : ops))
simplifyP (op : ops) = op : (simplifyP ops)
simplifyP [] = []

-- single pass to remove common data plumbing for sums
simplifyS :: [Op] -> [Op]
simplifyS (OpC Op_W : OpC Op_W : ops) = simplifyS ops
simplifyS (OpC Op_L : OpC Op_R : ops) = simplifyS ops
simplifyS (OpC Op_R : OpC Op_L : ops) = simplifyS ops
simplifyS (OpC Op_V : OpC Op_C : ops) = simplifyS ops
simplifyS (OpC Op_C : OpC Op_V : ops) = simplifyS ops
simplifyS (OpC Op_Z : OpC Op_Z : ops) = simplifyS ops
simplifyS (OpC Op_Z : OpC Op_W : OpC Op_Z : ops) =
    (OpC Op_W : simplifyS (OpC Op_Z : OpC Op_W : ops))
simplifyS (op : ops) = op : (simplifyS ops)
simplifyS [] = []

onBlocks :: ([Op] -> [Op]) -> Op -> Op
onBlocks f (BL ops) = BL (f ops)
onBlocks _ op = op

-- | 'simplify' is a fixed pipeline peephole optimizer
--
-- It won't simplify completely, but it can simplify a
-- reasonable depth with predictable linear performance.
simplify :: [Op] -> [Op]
simplify = p.p.s.p.p.s.d.c where
    c = removeSpaces
    d = fmap (onBlocks simplify)
    s = simplifyS
    p = simplifyP
