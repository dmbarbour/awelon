{-# LANGUAGE ViewPatterns #-}
-- convert ABC code to HS
module ABC.Imperative.ABC2HS
    ( abc2hs, abc2hs_imports -- default implementation

    -- possibly multiple implementations
    , abc2hs_naive, abc2hs_imports_naive, opMapNaive
    ) where

import qualified Data.Map as M
import ABC.Operators

type Error = String

abc2hs :: [Op] -> Either Error String
abc2hs = return . abc2hs_naive

abc2hs_imports :: [String]
abc2hs_imports = abc2hs_imports_naive

abc2hs_imports_naive :: [String]
abc2hs_imports_naive = ["ABC.Imperative.Operations"]

abc2hs_naive :: [Op] -> String
abc2hs_naive = flip abc2hs_naive' []

abc2hs_naive' :: [Op] -> ShowS
abc2hs_naive' [] = showString "return"
abc2hs_naive' (Op_ap:Op_c:[]) = showString "apc"
abc2hs_naive' (op:[]) = op2hs_naive' op
abc2hs_naive' (op:ops) = op2hs_naive' op . showString ">=>" . abc2hs_naive' ops

opMapNaive :: M.Map Op String
opMapNaive = M.fromList $
    [(Op_l,"l"),(Op_r,"r"),(Op_w,"w"),(Op_z,"z"),(Op_v,"v"),(Op_c,"c")
    ,(Op_L,"sL"),(Op_R,"sR"),(Op_W,"sW"),(Op_Z,"sZ"),(Op_V,"sV"),(Op_C,"sC")
    ,(Op_copy,"cp"),(Op_drop,"rm")
    ,(Op_add,"add"),(Op_neg,"neg"),(Op_mul,"mul"),(Op_inv,"inv"),(Op_divMod,"divQ")
    ,(Op_ap,"ap"),(Op_cond,"co"),(Op_quote,"qu"),(Op_comp,"o")
    ,(Op_rel,"k"),(Op_aff,"f")
    ,(Op_distrib,"sD"),(Op_factor,"sF"),(Op_merge,"sM"),(Op_assert,"sK")
    ,(Op_gt,"gt")
    ,(Op_introNum,"n0")
    ,(Op_0,"d0"),(Op_1,"d1"),(Op_2,"d2"),(Op_3,"d3"),(Op_4,"d4")
    ,(Op_5,"d5"),(Op_6,"d6"),(Op_7,"d7"),(Op_8,"d8"),(Op_9,"d9")
    ,(Op_SP,"return"),(Op_LF,"return")
    ]
inOpMap :: Op -> Maybe String
inOpMap = flip M.lookup opMapNaive

op2hs_naive' :: Op -> ShowS
op2hs_naive' (inOpMap -> Just s) = showString s
op2hs_naive' (TL s) = showString "tl" . shows s
op2hs_naive' (Tok s) = showString "tok" . shows s
op2hs_naive' (BL ops) = showString "bl" . opsStr . progVal where
    opsStr = shows (show ops) -- show all ops in a string
    progVal = showChar '(' . abc2hs_naive' ops . showChar ')'
op2hs_naive' op = error $ "op2hs_naive missing def for " ++ show op
