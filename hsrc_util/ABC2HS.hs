-- a mediocre, but not entirely naive, translation 
-- from Awelon bytecode into Haskell code.
--
-- Goal:
--
--   get fast enough that I'm not too embarrassed to run an AO-based
--   web application server or similar; doesn't need to be fantastic,
--   but should certainly be several times faster than interpretation
--   (on various microbenchmarks)
--
-- Design challenges:
--
--   (a) sum types; optimal factoring of conditional behaviors
--   (b) the divMod operation; two outputs, two inputs
--   (c) cycle detection and prevention
--   (d) effective error reporting
--
-- The sum types are the greatest challenge. If a subprogram makes
-- five choices, this could potentially bloat into thirty-two different
-- paths unless I'm careful with respect to factoring. It also seems
-- important to restrict where a decision is made, e.g. whether it is
-- shallow vs. deep in a pattern. 
--
-- For now, I'll just let this bloat happen, and see if I can solve it
-- later by automated flattening through refactoring and/or smarter
-- choices to divide subprograms prior to construction.
-- 
-- Cycle detection and prevention is easier. I can limit depth for
-- inlining of blocks (even a limited depth would likely work well).
-- And I can attempt to recognize whenever a block is reused.
--
-- Error reporting: I could probably emit the given value and the
-- expected patterns ("value ∉ patterns"). That would likely work
-- for most use-cases, but is possibly a little bulky. 
--
-- For divmod, I'm thinking I'll treat it as a special case invocation
-- for now. It's simply an unusual expression compared to most others.
--
module ABC2HS (abc2hs) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.List as L
import ABC.Operators

type ModuleName   = String 
type ErrorMsg     = String
type Name         = String
type Token        = String
type MkProg       = StateT ProgCX (ErrorT ErrorMsg Identity)
type MkProgC      = StateT ProgC (ListT MkProg)

-- generate the full module string, including all headers
abc2hs :: ModuleName -> [Op] -> Either ErrorMsg String
abc2hs modName ops = hsModuleText modName <$> abc2prog ops

-- generate just the meat of the module (no headers) and the
-- main function name (e.g. 'p0'). 
abc2prog :: [Op] -> Either ErrorMsg (Name,String)
abc2prog = runMkProg pcx0 . mkProg

-- operate in a state monad (for now)
runMkProg :: ProgCX -> MkProg a -> (Either ErrorMsg a)
runMkProg cx op = runIdentity $ runErrorT $ evalStateT op cx

-- compute module text, assuming an initial prog name and an 
-- extended program that may contain toplevel definitions.
hsModuleText :: ModuleName -> (Name,String) -> String
hsModuleText modName (main, context) = prefix context where
    p = showChar '\n'
    prefix  = showMod.p.showImps.p.showRsc.p
    showMod = 
        showString "module " . showString modName .
        showString " ( source, resource ) where "
    showImps = 
        showString "import ABC.Imperative.Prelude" . p
    showRsc =
        showString "resource :: Resource" . p .
        showString "resource = Resource " . showString main . p

-- build a program and toplevel context
mkProg :: [Op] -> MkProg (Name,String)
mkProg ops =
    defProg ops >>= \ p0 -> -- create and name main program
    -- gcContext [p0] >>    -- garbage collect subprograms
    addSource ops >>        -- include ABC source for resource
    get >>= \ cx ->         -- use context for subprograms
    return (p0, prog2hs cx) -- generate haskell code

-- write Haskell code for a set of subprograms and texts
prog2hs :: ProgCX -> String
prog2hs cx = showHs [] where
    p = showChar '\n'
    showHs = showDefs . showTexts
    showDefs = showl showDef (pcx_defs cx)
    showTexts = showl showText (M.toList (pcx_texts cx))
    showl f (x:xs) = f x . showl f xs
    showl _ [] = id
    showDef def = shows def . p
    showText (txt,nm) = 
        showString nm . showString " :: String" . p .
        showString nm . showString " = " . shows txt . p . p

-- let's get some utilities implemented...
--
-- First of all, we need a toplevel that can track a collection of
-- definitions, in addition to track whether we have already named
-- or implemented a subprogram. We can usefully track texts to avoid
-- repetition there, too.
--
data ProgCX = ProgCX
    { pcx_progs :: M.Map [Op] Name    -- ABC source → prog name
    , pcx_texts :: M.Map String Name  -- ABC text → prog name
    , pcx_defs  :: [ProgDef]          -- progdef includes name 
    }

pcx0 :: ProgCX
pcx0 = ProgCX 
    { pcx_progs = M.empty
    , pcx_texts = M.empty
    , pcx_defs  = []
    }

-- add a program to the context. This happens before
-- defining it, in order that cyclic programs (due to
-- fixpoint behavior) are implemented only once.
addProg :: [Op] -> MkProg (Bool,Name) -- (bExists,name)
addProg ops =
    get >>= \ cx ->
    let progs = pcx_progs cx in
    case M.lookup ops progs of
        Just pN -> return (True,pN)
        Nothing ->
            let pN = 'p' : show (M.size progs) in
            let progs' = M.insert ops pN progs in
            let cx' = cx { pcx_progs = progs' } in
            put cx' >> return (False,pN)

-- do we already have a given subprogram? 
hasProg :: [Op] -> MkProg (Maybe Name) -- bExists
hasProg ops = M.lookup ops <$> gets pcx_progs

-- define a new program
addProgDef :: ProgDef -> MkProg ()
addProgDef d = modify $ \ cx ->
    let defs' = d : pcx_defs cx in
    cx { pcx_defs = defs' }

-- add a text (e.g. ABC or text literal) to the context
addText :: String -> MkProg Name
addText txt = 
    get >>= \ cx ->
    let texts = pcx_texts cx in
    case M.lookup txt texts of
        Just tN -> return tN
        Nothing ->
            let tN = 't' : show (M.size texts) in
            let texts' = M.insert txt tN texts in
            let cx' = cx { pcx_texts = texts' } in
            put cx >> return tN

-- go ahead and provide the original ABC source code in each
-- module. This is unlikely to be a significant portion of
-- the code, and it might be convenient.
addSource :: [Op] -> MkProg ()
addSource ops = modify $ \ cx ->
    let texts' = M.insert (show ops) "source" (pcx_texts cx) in
    cx { pcx_texts = texts' }

-- name a subprogram, then implement it...
defProg :: [Op] -> MkProg Name
defProg ops = 
    addProg ops >>= \ (bExists, pN) ->
    if bExists then return pN else
    runProgC pc0 (defProgC ops (Var 0)) >>= \ lCases ->
    if null lCases then fail $ "BADLY TYPED: " ++ show ops else
    addProgDef (mkProgDef pN lCases) >>
    return pN

runProgC :: ProgC -> MkProgC a -> MkProg [(a,ProgC)]
runProgC pc0 op = runListT (runStateT op pc0)

-- a program definition currently consists of:
--  (1) a name for this program
--  (2) a set of one or more cases
-- There may be multiple cases in the end.
data ProgDef = ProgDef !Name !Cases
type Case = (Expr,ProgC)
type Cases = [Case]

instance Show ProgDef where
    showsPrec _ (ProgDef nm cs) =
        showString nm . showString " :: (Runtime m) => V m -> m (V m)\n" .
        showConcat (showCase nm (Var 0)) cs

showConcat :: (a -> ShowS) -> [a] -> ShowS
showConcat f (a:as) = f a . showConcat f as
showConcat _ [] = id

showCase :: Name -> Expr -> Case -> ShowS
showCase nm v0 (result,prog) = 
    showString nm . showChar ' ' .
    showString " (pattern) = error \"todo!\""
    --showsPattern (expandPattern v0 prog) .
    
mkProgDef :: Name -> Cases -> ProgDef
mkProgDef = ProgDef

defProgC :: [Op] -> Expr -> MkProgC Expr
-- defProgC (Op_ap : Op_c : ops) = ops_apc >=> defProgC ops
defProgC (op:ops) = op op >=> defProgC ops
defProgC [] = return

-- potentially optimize common inlining behaviors
--ops_apc :: Expr -> MkProgC Expr
--ops_apc = op Op_ap >=> op Op_c

-- handle individual ops
op :: Op -> Expr -> MkProgC Expr
op Op_l = opl
op Op_r = opr
op Op_w = opw
op Op_z = opz
op Op_v = opv
op Op_c = opc
op Op_L = opL
op Op_R = opR
op Op_W = opW
op Op_Z = opZ
op Op_V = opV
op Op_C = opC
op (BL b) = opBlock b
op (TL s) = opText s
op (Tok s) = opTok s
op Op_copy = opCopy
op Op_drop = opDrop
op Op_add = opAdd
op Op_neg = opNeg
op Op_mul = opMul
op Op_inv = opInv
op Op_divMod = opDivMod
op Op_ap = opApply
op Op_cond = opCond
op Op_quote = opQuote
op Op_comp = opCompose
op Op_rel = opRel
op Op_aff = opAff
op Op_distrib = opDistrib
op Op_factor = opFactor
op Op_merge = opMerge
op Op_assert = opAssert
op Op_gt = opGT
op Op_introNum = opIntroNum
op Op_0 = opDigit 0
op Op_1 = opDigit 1
op Op_2 = opDigit 2
op Op_3 = opDigit 3
op Op_4 = opDigit 4
op Op_5 = opDigit 5
op Op_6 = opDigit 6
op Op_7 = opDigit 7
op Op_8 = opDigit 8
op Op_9 = opDigit 9
op Op_SP = return 
op Op_LF = return 

opl,opr,opw,opz,opv,opc
 ,opL,opR,opW,opZ,opV,opC
 ,opL',opR',opW',opZ',opV',opC'
 ,opCopy,opDrop
 ,opAdd,opNeg,opMul,opInv,opDivMod
 ,opApply,opCond,opQuote,opCompose,opRel,opAff
 ,opDistrib,opFactor,opMerge,opAssert,opGT
 ,opIntroNum :: Expr -> MkProgC Expr

opDigit :: Int -> Expr -> MkProgC Expr
opBlock :: [Op] -> Expr -> MkProgC Expr
opText  :: String -> Expr -> MkProgC Expr
opTok   :: Token -> Expr -> MkProgC Expr

-- product types are relatively trivial.
opl xyz = 
    asProd xyz >>= \ (x,yz) ->
    asProd yz >>= \ (y,z) ->
    return (Prod (Prod x y) z)
opr xyz =
    asProd xyz >>= \ (xy,z) ->
    asProd xy >>= \ (x,y) ->
    return (Prod x (Prod y z))
opw xyz =
    asProd xyz >>= \ (x,yz) ->
    asProd yz >>= \ (y,z) ->
    return (Prod y (Prod x z))
opz wxyz = 
    asProd wxyz >>= \ (w,xyz) ->
    opw xyz >>= \ yxz ->
    return (Prod w yxz)
opv x = return (Prod x Unit)
opc xu =
    asProd xu >>= \ (x,u) ->
    asUnit u >>
    return x

-- sum types are easy enough to process at this point, but I'm
-- creating a lot of factoring work down the line. I wonder if
-- it might be better to track sums more explicitly.
opL = onFst opL'
opR = onFst opR'
opW = onFst opW'
opZ = onFst opZ'
opV = onFst opV'
opC = onFst opC'

onFst :: (Expr -> MkProgC Expr) -> Expr -> MkProgC Expr
onFst f xe =
    asProd xe >>= \ (x,e) ->
    f x >>= \ x' ->
    return (Prod x' e)

opL' = asSum >=> either inLL (asSum >=> either inLR inR) where
    inLL = return . SumL . SumL
    inLR = return . SumL . SumR
    inR  = return . SumR
opR' = asSum >=> either (asSum >=> either inL inRL) inRR where
    inL  = return . SumL
    inRL = return . SumR . SumL
    inRR = return . SumR . SumR
opW' = asSum >=> either inRL (asSum >=> either inL inRR) where
    inRL = return . SumR . SumL
    inL  = return . SumL
    inRR = return . SumR . SumR
opZ' = asSum >=> either inL (opW' >=> inR) where
    inL  = return . SumL
    inR  = return . SumR
opV' = return . SumL
opC' xv =
    asSum xv >>= \ x_v ->
    case x_v of
        Left x -> return x
        Right _v -> fail "type error; `C` requires void in right" 

opCopy xe =
    asProd xe >>= \ (x,e) ->
    requireCopyable x >>
    return (Prod x (Prod x e))
opDrop xe =
    asProd xe >>= \ (x,e) ->
    requireDroppable x >> -- drop only droppable values
    return e

opAdd xye =
    asProd xye >>= \ (x,ye) ->
    asProd ye >>= \ (y,e) ->
    asNumber x >>= \ nx ->
    asNumber y >>= \ ny ->
    return (Prod (addNumbers nx ny) e)

opNeg xe =
    asProd xe >>= \ (x,e) ->
    asNumber x >>= \ nx ->
    return (Prod (negNumber nx) e)

opMul xye =
    asProd xye >>= \ (x,ye) ->
    asProd ye >>= \ (y,e) ->
    asNumber x >>= \ nx ->
    asNumber y >>= \ ny ->
    return (Prod (mulNumbers nx ny) e)

opInv xe =
    asProd xe >>= \ (x,e) ->
    asNumber x >>= \ nx ->
    requireNonZero nx >>
    return (Prod (invNumber nx) e)

opDivMod xye =
    asProd xye >>= \ (x,ye) ->
    asProd ye >>= \ (y,e) ->
    asNumber x >>= \ divisor ->
    requireNonZero divisor >>
    asNumber y >>= \ dividend ->
    runDivModQ dividend divisor >>= \ (quotient,remainder) ->
    return (Prod (VNum remainder) (Prod (VNum quotient) e))

runDivModQ :: NumExpr -> NumExpr -> MkProgC (NumExpr, NumExpr)
runDivModQ (NumConst dividend) (NumConst divisor) =
    let (quotient,remainder) = divModQ dividend divisor in
    return (NumConst (fromInteger quotient), NumConst remainder)
runDivModQ dividend divisor =
    newSymbol >>= \ result ->
    asProd (Var result) >>= \ (vQuot,vRem) ->
    asNumber vQuot >>= \ nQuot ->
    asNumber vRem >>= \ nRem ->
    let param = Prod (VNum dividend) (VNum divisor) in
    addCall (Call DivModQ param result) >>
    return (nQuot, nRem)

opApply bxe =
    asProd bxe >>= \ (b,xe) ->
    asProd xe >>= \ (x,e) ->
    asBlock b >>= \ block ->
    runCode (bx_code block) x >>= \ x' ->
    return (Prod x' e)

opCond bxe =
    asProd bxye >>= \ (b,xye) ->
    asProd xye >>= \ (xy,e) ->
    asBlock b >>= \ block ->
    requireCond (boolNot (bx_rel block)) >>
    asSum xy >>= \ x_y ->
    case x_y of 
        Left x -> 
            runCode (bx_code block) x >>= \ x' ->
            return (Prod x' e)
        Right y -> return (Prod y e)

opQuote xe =
    asProd xe >>= \ (x,e) ->
    testCopyable x >>= \ bCopyable ->
    testDroppable x >>= \ bDroppable ->
    let rel = boolNot bDroppable in
    let aff = boolNot bCopyable in
    let code = CodeQuote x in
    let block = BX { bx_code = code, bx_aff = aff, bx_rel = rel } in
    return (Prod (VBlock block) e)

opCompose yz_xy_e =
    asProd yz_xy_e >>= \ (yz, xy_e) ->
    asProd xy_e >>= \ (xy,e) ->
    asBlock yz >>= \ byz ->
    asBlock xy >>= \ bxy ->
    return (Prod (VBlock (blockCompose bxy bzy)) e)

opRel xe =
    asProd xe >>= \ (x,e) -> 
    asBlock x >>= \ bx ->
    let bx' = bx { bx_rel = BoolConst True } in
    return (Prod (VBlock bx') e)

opAff xe =
    asProd xe >>= \ (x,e) ->
    asBlock x >>= \ bx ->
    let bx' = bx { bx_aff = BoolConst True } in
    return (Prod (VBlock bx') e)

opDistrib xse =
    asProd xse >>= \ (x,se) ->
    asProd se >>= \ (s,e) ->
    asSum s >>= \ b_c ->
    case b_c of
        Left b -> return (Prod (SumL (Prod x b)) e)
        Right c -> return (Prod (SumR (Prod x c)) e)

opFactor se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ ab_cd ->
    case ab_cd of
        Left ab ->
            asProd ab >>= \ (a,b) ->
            return (Prod (SumL a) (Prod (SumL b) e)
        Right cd ->
            asProd cd >>= \ (c,d) ->
            return (Prod (SumR c) (Prod (SumR d) e)

opMerge se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ a_b ->
    case a_b of
        Left a -> return (Prod a e)
        Right a' -> return (Prod a' e)

opAssert se = 
    asProd se >>= \ (s,e) ->
    asSum s >>= \ a_b ->
    case a_b of
        Left _a -> fail "assertion failure"
        Right b -> return (Prod b e)

opGT yxe = 
    asProd xye >>= \ (x,ye) ->
    asProd ye >>= \ (y,e) ->
    asNumber x >>= \ nx ->
    asNumber y >>= \ ny ->
    testGT ny nx >>= \ (nx',ny',bGT) ->
    let x' = VNum nx' in
    let y' = VNum ny' in
    if bGT then return (Prod (SumR (Prod x' y')) e)
           else return (Prod (SumL (Prod y' x')) e)

opIntroNum e = return (Prod (VNum (NumConst 0)) e)

opDigit d re = 
    asProd re >>= \ (r,e) ->
    asNumber r >>= \ nr ->
    let nr' = addNumbers (NumConst (fromIntegral d)) $
              mulNumbers (NumConst 10) $ nr 
    in
    return (Prod (VNum nr) e)

opBlock ops e =
    let bx = BX (CodeConst ops) False False in
    return (Prod (VBlock bx) e)

opText txt e = return (Prod (textToExpr txt) e)

-- conventional sealer/unsealer pairs
opTok s@(':':_) x = return (Sealed s x)
opTok ('.':s) x = fromSealed (':':s) x
opTok tok@"&compile" b =
    -- don't recompile a block if we locally know it's compiled
    asBlock b >>= \ bx ->
    let b' = VBlock bx in
    case bx_code bx of
        CodeConst _ -> return b'
        _ -> runInvoke tok b'
opTok token expr = runInvoke token expr


asBlock :: Expr -> MkProgC BlockExpr
asProd :: Expr -> MkProgC (Expr, Expr)
asSum :: Expr -> MkProgC (Either Expr Expr)
asNumber :: Expr -> MkProgC NumExpr
fromSealed :: Token -> Expr -> MkProgC Expr



-- | a new ProgC generally starts with (Var 0)
-- and adds new variables as necessary. 
--
-- Each variable may be inferred to have some structure.
data ProgC = ProgC
    { pc_vars    :: M.Map Sym Expr
    , pc_conds   :: [Cond]
    , pc_calls   :: [Call]
    , pc_gensym  :: !Sym
    }
data Call = Call Callee Expr Sym
data Callee 
    = SubProg Name 
    | Invoke Token 
    | DivModQ
type Cond = BoolExpr
type Sym = Integer

pc0 :: ProgC
pc0 = ProgC M.empty [] [] 0

newSymbol :: MkProgC Sym
newSymbol =
    get >>= \ pc ->
    let sym = 1 + pc_gensym pc in
    let pc' = pc { pc_gensym = sym } in
    put pc' >> return sym

addCall :: Call -> MkProgC ()
addCall call = modify $ \ pc ->
    let calls' = call : pc_calls pc in
    pc { pc_calls = calls' }

addCond :: Cond -> MkProgC ()
addCond cond = modify $ \ pc ->
    let conds' = cond : pc_conds pc in
    pc { pc_conds = conds' }

runInvoke :: Token -> Expr -> MkProgC Expr
runInvoke token expr =
    newSymbol >>= \ sResult ->
    addCall (Call (Invoke token) expr sResult) >>
    return (Var sResult)

runSubProg :: Name -> Expr -> MkProgC Expr
runSubProg nm expr =
    newSymbol >>= \ sResult ->
    addCall (Call (SubProg nm) expr sResult) >>
    return (Var sResult)

requireCond :: Cond -> MkProgC ()
requireCond (BoolConst False) = mzero -- invalid path!
requireCond c = addCond c

requireNonZero :: NumExpr -> MkProgC ()
requireNonZero (NumConst r) = when (0 == r) mzero
requireNonZero expr = addCond (BoolNZ expr)

data Expr
    = Var !Sym
    | Prod !Expr !Expr | Unit
    | SumL !Expr | SumR !Expr
    | Sealed !Token !Expr
    | VNum !NumExpr
    | VBlock !BlockExpr

textToExpr :: String -> Expr
textToExpr (c:cs) = SumL (Prod (charToExpr c) (textToExpr cs))
textToExpr [] = SumR Unit

charToExpr :: Char -> Expr
charToExpr = VNum . NumConst . fromIntegral . fromEnum

data NumExpr
    = NumConst !Rational
    | NumVar !Sym
    | AddNum !NumExpr !NumExpr
    | MulNum !NumExpr !NumExpr
    | InvNum !NumExpr
    | NegNum !NumExpr

data BlockExpr = BlockExpr 
    { bx_code :: !CodeExpr
    , bx_aff  :: !BoolExpr
    , bx_rel  :: !BoolExpr
    }

data CodeExpr 
    = CodeConst [Op]
    | CodeVar !Sym
    | CodeComp !CodeExpr !CodeExpr
    | CodeQuote !Expr

data BoolExpr 
    = BoolConst !Bool
    | BoolVar !Sym
    | BoolOr  !BoolExpr !BoolExpr
    | BoolNot !BoolExpr 
    | BoolGT  !NumExpr !NumExpr -- greater than
    | BoolNZ  !NumExpr -- non-zero
    | BCopyable  !Expr
    | BDroppable !Expr

