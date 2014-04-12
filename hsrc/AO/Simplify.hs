{-# LANGUAGE PatternGuards #-}

-- | Simplify the ABC code within an AO definition. This is
-- somewhat redundant with ABC's simplifier, but is useful if
-- attempting a balance between inlining vs. space savings 
-- when simplifying the larger dictionary.
--
module AO.Simplify (simplifyCode) where

import ABC.Operators
import AO.Code

simplifyCode :: AO_Code -> AO_Code
simplifyCode = sz []

-- zipper-based simplification
sz :: [AO_Action] -> [AO_Action] -> [AO_Action]
sz l (AO_ABC Op_SP : r) = sz l r
sz l (AO_ABC Op_LF : r) = sz l r
sz (AO_ABC opl : l) (AO_ABC opr : r) | opsCancel opl opr = sz l r
sz l (AO_Block ops : r) = sz (AO_Block ops' : l) r where
    ops' = simplifyCode ops
sz (AO_ABC Op_w : AO_ABC Op_z : l) (AO_ABC Op_z : r) =
    sz l (AO_ABC Op_w : AO_ABC Op_z : AO_ABC Op_w : r)
sz (AO_ABC Op_W : AO_ABC Op_Z : l) (AO_ABC Op_Z : r) =
    sz l (AO_ABC Op_W : AO_ABC Op_Z : AO_ABC Op_W : r)
sz l (op:r) = sz (op:l) r -- no simplifications found; move on
sz l [] = reverse l -- all done!
