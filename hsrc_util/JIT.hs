{-# LANGUAGE ViewPatterns, ImpredicativeTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A JIT for ABC.
--
-- This particular just-in-time compiler leverages Haskell's plugins
-- framework. 
--
-- IMPROVEMENTS TO CONSIDER:
--
--   Eliminate redundancy for parsing and metadata: (mid priority)
--
--     Instead of generating a program directly, generate a function
--     that will accept a few arguments from the caller and return a
--     program in context. 
--
--     This could also be used for partial reuse of compilations.
--     (Could be simple use of Data.Map?)
--
--   Let-based construction: (high priority)
--
--     Translate program into a sequence of 'let' expressions and an
--     occasional imperative operation. This should also be easier to
--     read in the common case.
--
--   Partial Evaluation: (high priority)
--
--     Track constants in the program (numbers, blocks) and use them
--     to specialize code or inline functions.
--
--   Cycle recognition: (high priority)
--
--     Detect when a block needs itself. Translate this into appropriate
--     Haskell code, especially in cases where it can inline itself.
--
--   Incremental: (low priority)
--
--     Create a JIT-based interpreter that JIT's small chunks of a 
--     program and runs those. 
--
module JIT 
    ( abc_jit, abc2hs, abc2hs_imports -- default implementation

    , abc_jit_test -- for testing

    -- possibly multiple implementations
    , abc2hs_naive, abc2hs_imports_naive, opMapNaive
    ) where

import qualified Data.Map as M
import qualified Data.List as L

import qualified System.Eval.Haskell as Eval

import ABC.Operators
import ABC.Simplify
import ABC.Imperative.Value
import ABC.Imperative.Runtime

type Error = String

type JitProg = forall m . Runtime m => Prog m

-- Note: The plugins project seems to have a bug. After the first one, all
-- evaluations seem to return the same value, at least in ghci. 
--
-- Other than that, this seems to work so far.
abc_jit :: [Op] -> IO (Either [Error] JitProg)
abc_jit ops = 
    case abc2hs ops of
        Left err -> return (Left [err])
        Right src -> defaultEval src

abc_jit_test :: [Op] -> IO (Prog IO)
abc_jit_test ops = abc_jit ops >>= either (fail . L.unlines) (return . asIO) where
    asIO :: (forall m . Runtime m => Prog m) -> Prog IO
    asIO p = p

defaultEval :: String -> IO (Either [Error] a)
defaultEval src = Eval.unsafeEval_ src mods args ldflags incs where
    mods = abc2hs_imports
    args = langOpts ++ warnOpts ++ compOpts 
    langOpts = ["-XNoImplicitPrelude"
               ,"-XNoMonomorphismRestriction"
               ,"-XRank2Types"]
    warnOpts = ["-Wall"
               ,"-Werror"
               ,"-fno-warn-missing-signatures"
               ,"-fno-warn-unused-imports"]
    compOpts = ["-O0","-fno-enable-rewrite-rules"]
    ldflags = []
    incs = []

abc2hs :: [Op] -> Either Error String
abc2hs = return . abc2hs_naive . simplify

abc2hs_imports :: [String]
abc2hs_imports = abc2hs_imports_naive

abc2hs_imports_naive :: [String]
abc2hs_imports_naive = 
    ["ABC.Imperative.Operations"
    ,"ABC.Imperative.Runtime"
    ,"ABC.Imperative.Value"
    ,"Control.Monad (return)"
    ]

abc2hs_naive :: [Op] -> String
abc2hs_naive ops = "(" ++ abc2hs_naive' ops ") :: forall m . Runtime m => Prog m"

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


