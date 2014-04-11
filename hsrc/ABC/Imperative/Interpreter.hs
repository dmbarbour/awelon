
-- | Simple imperative interpreter for ABC code
module ABC.Imperative.Interpreter 
    ( interpret
    ) where

import qualified Data.Sequence as S 
import ABC.Imperative.Runtime
import ABC.Imperative.Operations
import ABC.Imperative.Value
import ABC.Operators

-- | build a Haskell process from a sequence of ABC operations. 
interpret :: (Runtime cx) => [Op] -> Prog cx
interpret (OpC Op_ap : OpC Op_c : []) = apc -- for tail-call optimization
interpret (op : ops) = interpret ops . iop op
interpret [] = id

-- interpret one operation
iop :: (Runtime cx) => Op -> Prog cx 
iop (OpC op) = iopc op
iop (BL ops) = fmap (P (B block)) where
    block = Block { b_aff = False, b_rel = False, b_code = code, b_prog = prog }
    code  = S.fromList ops
    prog  = interpret ops
iop (TL s) = tl s
iop (Tok s) = tok s

iopc :: (Runtime cx) => OpC -> Prog cx
iopc Op_l = l
iopc Op_r = r
iopc Op_w = w
iopc Op_z = z
iopc Op_v = v
iopc Op_c = c
iopc Op_L = sL
iopc Op_R = sR
iopc Op_W = sW
iopc Op_Z = sZ
iopc Op_V = sV
iopc Op_C = sC
iopc Op_copy = cp
iopc Op_drop = rm
iopc Op_add = add
iopc Op_neg = neg
iopc Op_mul = mul
iopc Op_inv = inv
iopc Op_divMod = divQ
iopc Op_ap = ap
iopc Op_cond = co
iopc Op_quote = qu
iopc Op_comp = o
iopc Op_rel = k
iopc Op_aff = f
iopc Op_distrib = sD
iopc Op_factor = sF
iopc Op_merge = sM
iopc Op_assert = sK
iopc Op_gt = gt
iopc Op_introNum = n0
iopc Op_0 = d0
iopc Op_1 = d1
iopc Op_2 = d2
iopc Op_3 = d3
iopc Op_4 = d4
iopc Op_5 = d5
iopc Op_6 = d6
iopc Op_7 = d7
iopc Op_8 = d8
iopc Op_9 = d9
iopc Op_SP = id
iopc Op_LF = id

