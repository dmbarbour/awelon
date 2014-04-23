
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
interpret (Op_ap : Op_c : []) = apc -- for tail-call optimization
interpret (op : ops) =  iop op >=> interpret ops
interpret [] = return

-- interpret one operation
iop :: (Runtime cx) => Op -> Prog cx 
iop Op_l = l
iop Op_r = r
iop Op_w = w
iop Op_z = z
iop Op_v = v
iop Op_c = c
iop Op_L = sL
iop Op_R = sR
iop Op_W = sW
iop Op_Z = sZ
iop Op_V = sV
iop Op_C = sC
iop (BL ops) = return . (P (B block)) where
    block = Block { b_aff = False, b_rel = False, b_code = code, b_prog = prog }
    code  = S.fromList ops
    prog  = interpret ops
iop (TL s) = tl s
iop (Tok s) = tok s
iop Op_copy = cp
iop Op_drop = rm
iop Op_add = add
iop Op_neg = neg
iop Op_mul = mul
iop Op_inv = inv
iop Op_divMod = divQ
iop Op_ap = ap
iop Op_cond = co
iop Op_quote = qu
iop Op_comp = o
iop Op_rel = k
iop Op_aff = f
iop Op_distrib = sD
iop Op_factor = sF
iop Op_merge = sM
iop Op_assert = sK
iop Op_gt = gt
iop Op_introNum = n0
iop Op_0 = d0
iop Op_1 = d1
iop Op_2 = d2
iop Op_3 = d3
iop Op_4 = d4
iop Op_5 = d5
iop Op_6 = d6
iop Op_7 = d7
iop Op_8 = d8
iop Op_9 = d9
iop Op_SP = return
iop Op_LF = return

