
-- | Translate ABC code into a box-and-wire graph
--
-- An earlier effort at this collapsed to cumulative complexity,
-- presumably because it attempted too many responsibilities (such
-- as partial evaluation and inlining). 
--
-- Any 'wire' in this graph may be active or inactive. Typically, the
-- activity is determined at runtime. Activity corresponds roughly to
-- the 'Maybe' type - i.e. a wire maybe carries a value at runtime.
-- For a product type (a*b), both `a` and `b` are either both active
-- or both inactive. For a sum type (a+b), at most one of `a` and `b`
-- are active. At compile time, a sum type (a+b) might be represented
-- as a pair (Maybe a)*(Maybe b), such that we can operate on branches
-- independently. (This is analogous to the interpretation of signals
-- in Sirea and RDP.)
--
-- At the moment, this activity is not tracked in ABCGraph except at
-- sum types, which allow changes in activity. However, if I later 
-- aim to 'inline' conditional behaviors, tracking activity will be
-- a much more significant concern.
--
-- The design here is aimed to separate the concerns of representation
-- and reduction, albeit at the cost of requiring a lot of extra steps.
--
module ABCGraph
    ( abc2graph
    , Wire(..)
    , Node(..)
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad 
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer.Strict
import Data.Functor.Identity

import Data.Ratio

data Wire
    = Var WireLabel
    | Num NumWire
    | Block CodeBundle
    | Prod Wire Wire
    | Unit
    | Sum BoolWire Wire Wire -- (false+true) order
    | Seal String Wire
    -- note that in sums, the inner boolean is overridden by an outer
    -- boolean; i.e. Sum False (Sum True x1 x2) (Sum True x3 x4) is in x2

tydesc :: Wire -> String
tydesc (Var _) = "var"
tydesc (Num _) = "number"
tydesc (Block _) = "block"
tydesc (Prod a b) = tydesc a ++ "*" ++ tydesc b
tydesc Unit = "unit"
tydesc (Sum _ a b) = tydesc a ++ "+" ++ tydesc b
tydesc (Seal s v) = tydesc v ++ "{:" ++ s ++ "}"

data CodeBundle = CodeBundle 
    { cb_src :: SrcWire
    , cb_aff :: BoolWire
    , cb_rel :: BoolWire
    }

type WireLabel = Label Wire
type NumWire = Label Rational
type BoolWire = Label Bool
type SrcWire = Label [Op]
newtype Label n = Label Integer deriving (Ord,Eq)

-- NOTE: copy, drop, and most data plumbing operations are implicit
-- (nodes do assert elements are copyable or droppable)
data Node
    = Elab WireLabel Wire -- elaborate a wire description
    | Void () WireLabel -- a wire from nowhere (via `V`)...

    -- math
    | NumConst Rational NumWire
    | Add (NumWire,NumWire) NumWire
    | Neg NumWire NumWire
    | Mul (NumWire,NumWire) NumWire
    | Inv NumWire NumWire
    | DivMod (NumWire,NumWire) (NumWire,NumWire) -- (dividend,divisor)→(quotient,remainder)
    | IsNonZero NumWire BoolWire
    | GreaterThan (NumWire,NumWire) BoolWire -- (x,y) (x>y)
    
    -- booleans (for sums, affine, relevant, safety)
    | BoolConst Bool BoolWire
    | BoolOr (BoolWire,BoolWire) BoolWire
    | BoolAnd (BoolWire,BoolWire) BoolWire
    | BoolNot BoolWire BoolWire
    | BoolCopyable WireLabel BoolWire
    | BoolDroppable WireLabel BoolWire

    -- assertions
    | BoolAssert BoolWire ()

    -- higher order programming
    | SrcConst [Op] SrcWire
    | Quote Wire SrcWire
    | Compose (SrcWire,SrcWire) SrcWire -- (x→y,y→z) x→z
    | Apply (SrcWire,Wire) WireLabel
    
    -- conditional behavior
    | CondAp (BoolWire,SrcWire,Wire) WireLabel
    | Mux (BoolWire,Wire,Wire) WireLabel -- (cond,onTrue,onFalse) dest

    -- extended behaviors
    | Invoke String Wire WireLabel
    deriving (Show)

type MkGraph = StateT GCX (ErrorT String Identity)
type GCX = ([Node],Integer)
type Box = Wire -> MkGraph Wire

runMkGraph :: GCX -> MkGraph a -> Either String (a,GCX)
evalMkGraph :: MkGraph a -> Either String a
gcx0 :: GCX

runMkGraph gcx op = runIdentity $ runErrorT $ runStateT op gcx
evalMkGraph = runIdentity . runErrorT . flip evalStateT gcx0
gcx0 = ([],0)

newLabel :: MkGraph (Label t)
newLabel =
    get >>= \ (ns,s) ->
    let s' = s + 1 in
    put (ns,s') >>
    return $! (Label s')

emitNode :: Node -> MkGraph ()
emitNode n = modify $ \ (ns,s) -> (n:ns,s)

-- runABC will actually emit nodes associated with the ABC operators
runABC :: [Op] -> Box
runABC (op:ops) = runOp op >=> runABC ops
runABC [] = return

runOp :: Op -> Box
runOp Op_l = opl
runOp Op_r = opr
runOp Op_w = opw
runOp Op_z = opz
runOp Op_v = opv
runOp Op_c = opc
runOp Op_L = opL
runOp Op_R = opR
runOp Op_W = opW
runOp Op_Z = opZ
runOp Op_V = opV
runOp Op_C = opC
runOp (BL ops) = opBL ops
runOp (TL str) = opTL str
runOp (Tok tok) = opTok tok
runOp Op_copy = opCopy
runOp Op_drop = opDrop
runOp Op_add = opAdd
runOp Op_neg = opNeg
runOp Op_mul = opMul
runOp Op_inv = opInv
runOp Op_divMod = opDivMod
runOp Op_ap = opApply
runOp Op_cond = opCond
runOp Op_quote = opQuote
runOp Op_comp = opCompose
runOp Op_rel = opRel
runOp Op_aff = opAff
runOp Op_distrib = opDistrib
runOp Op_factor = opFactor
runOp Op_merge = opMerge
runOp Op_assert = opAssert
runOp Op_gt = opGT
runOp Op_introNum = opIntroNum
runOp Op_0 = opDigit 0
runOp Op_1 = opDigit 1
runOp Op_2 = opDigit 2
runOp Op_3 = opDigit 3
runOp Op_4 = opDigit 4
runOp Op_5 = opDigit 5
runOp Op_6 = opDigit 6
runOp Op_7 = opDigit 7
runOp Op_8 = opDigit 8
runOp Op_9 = opDigit 9
runOp Op_SP = return
runOp Op_LF = return

opl,opr,opw,opz,opv,opc :: Box
opL,opR,opW,opZ,opV,opC :: Box
opL',opR',opW',opZ',opV',opC' :: Box
opBL :: [Op] -> Box
opTL :: String -> Box
opTok :: String -> Box
opCopy,opDrop :: Box
opAdd,opNeg,opMul,opInv,opDivMod :: Box
opApply,opCond,opQuote,opCompose,opAff,opRel :: Box
opDistrib,opFactor,opMerge,opAssert :: Box
opGT :: Box
opIntroNum :: Box
opDigit :: Int -> Box

opl abc =
    asProd abc >>= \ (a,bc) ->
    asProd bc >>= \ (b,c) ->
    return (Prod (Prod a b) c)
opr abc = 
    asProd abc >>= \ (ab,c) ->
    asProd ab >>= \ (a,b) ->
    return (Prod a (Prod b c))
opw abc =
    asProd abc >>= \ (a,bc) ->
    asProd bc >>= \ (b,c) ->
    return (Prod b (Prod a c))
opz abcd =
    asProd abcd >>= \ (a,bcd) ->
    opw bcd >>= \ cbd ->
    return (Prod a cbd)
opv a = return (Prod a Unit)
opc au = 
    asProd au >>= \ (a,u) ->
    asUnit u >>
    return a

onFst :: Box -> Box
onFst f ae = 
    asProd ae >>= \ (a,e) -> 
    f a >>= \ a' -> 
    return (Prod a' e)

opL = onFst opL'
opR = onFst opR'
opW = onFst opW'
opZ = onFst opZ'
opV = onFst opV'
opC = onFst opC'

opL' abc = 
    asSum abc >>= \ (inBC, a, bc) ->
    asSum bc >>= \ (inC_when_inBC, b, c) ->
    boolAnd inBC inC_when_inBC >>= \ inC ->
    return (Sum inC (Sum inBC a b) c)
opR' abc =
    asSum abc >>= \ (inC, ab, c) ->
    asSum ab >>= \ (inB_unless_inC, a, b) ->
    boolOr inC inB_unless_inC >>= \ inBC ->
    return (Sum inBC a (Sum inC b c))
opW' abc =
    asSum abc >>= \ (inBC, a, bc) ->
    asSum bc >>= \ (inC_when_inBC, b, c) ->
    boolNot inBC >>= \ inA ->
    boolOr inA inC_when_inBC >>= \ inAC ->
    return (Sum inAC b (Sum inBC a c))
opZ' abcd =
    asSum abcd >>= \ (inBCD,a,bcd) ->
    opW' bcd >>= \ cbd ->
    return (Sum inBCD a cbd)
opV' a = 
    newVoid >>= \ v ->
    newBoolConst False >>= \ inV ->
    return (Sum inV a v)
opC' av =
    asSum av >>= \ (inV,a,v) ->
    (boolAssert <*> boolNot inV) >>
    return a

opBL ops v = mkBlock ops >>= \ cb -> return (Prod (Block cb) v)

mkBlock :: [Op] -> MkGraph CodeBlock
mkBlock ops = CodeBlock <$> newSrcConst ops 
                        <*> newBoolConst False
                        <*> newBoolConst False 

opTL txt v = textToWire txt >>= \ txtWire -> return (Prod txtWire v)

opTok s@(':':_) w = return (Seal s w)
opTok ('.':s) w = unseal (':':s) w
opTok tok w = newLabel >>= \ r -> emitNode (Invoke tok w r) >> return (Var r)

unseal :: String -> Box
unseal s (Seal s' w) | (s == s') = return w
unseal s (Var w) = elab w (Seal s <$> newVar) >>= unseal s
unseal s v = fail $ "expecting {:"++s++"} @ " ++ tydesc v

opCopy ae =
    asProd ae >>= \ (a,e) ->
    (boolAssert <*> boolCopyable a) >>
    return (Prod a (Prod a e))
opDrop ae =
    asProd ae >>= \ (a,e) ->
    (boolAssert <*> boolDroppable a) >>
    return e

opAdd abe =
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNumber a >>= \ na ->
    asNumber b >>= \ nb ->
    newNumWire >>= \ nr ->
    emitNode (Add (na,nb) nr) >>
    return (Prod (Num nr) e)

opMul abe =
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNumber a >>= \ na ->
    asNumber b >>= \ nb ->
    newNumWire >>= \ nr ->
    emitNode (Mul (na,nb) nr) >>
    return (Prod (Num nr) e)

opNeg ae =
    asProd ae >>= \ (a,e) ->
    asNumber a >>= \ na ->
    newNumWire >>= \ nr ->
    emitNode (Neg na nr) >>
    return (Prod (Num nr) e)

opInv ae = 
    asProd ae >>= \ (a,e) ->
    asNumber a >>= \ na ->
    assertNonZero na >>
    newNumWire >>= \ nr ->
    emitNode (Inv na nr) >>
    return (Prod (Num nr) e)

opDivMod bae = 
    asProd bae >>= \ (b, ae) ->
    asProd ae >>= \ (a,e) ->
    asNumber b >>= \ divisor ->
    asNumber a >>= \ dividend -> 
    assertNonZero divisor >>
    newNumWire >>= \ quotient ->
    newNumWire >>= \ remainder ->
    emitNode (DivMod (dividend,divisor) (quotient,remainder)) >>
    return (Prod (Num remainder) (Prod (Num quotient) e))

assertNonZero :: NumWire -> MkGraph ()
assertNonZero r = 
    newBoolWire >>= \ b -> 
    emitNode (IsNonZero r b) >> 
    boolAssert b

opApply bxe =
    asProd bxe >>= \ (b,xe) ->
    asProd xe >>= \ (x,e) ->
    asCode b >>= \ cb ->
    let src = cb_src cb in
    newLabel >>= \ x' ->
    emitNode (Apply (src,x) x') >>
    return (Prod (Var x') e)

mkIdentityFn :: MkGraph CodeBlock
mkIdentityFn = CodeBlock <$>  newSrcConst [] 

opCond bse =
    asProd bse >>= \ (b,se) ->
    asProd se >>= \ (s,e) ->
    asSum  s >>= \ (inY, x, y) ->
    asCode b >>= \ cb ->
    let src = cb_src cb in
    (boolAssert <*> boolNot (cb_rel cb)) >>
    boolNot inY >>= \ inX ->
    newLabel >>= \ x' ->
    emitNode (CondAp (inX,src,x) x' >>
    return (Prod (Sum inY (Var x') y) e)

opQuote xe =
    asProd xe >>= \ (x,e) ->
    (boolNot <*> boolCopyable x) >>= \ aff ->
    (boolNot <*> boolDroppable x) >>= \ rel ->
    newSrcWire >>= \ src ->
    emitNode (Quote x src) >>
    let cb = CodeBlock { cb_src = src, cb_rel = rel, cb_aff = aff } in
    return (Prod (Block cb) e)

opCompose yxe =
    asProd yxe >>= \ (yz,xe) ->
    asProd xe >>= \ (xy,e) ->
    asCode xy >>= \ cbxy ->
    asCode yz >>= \ cbyz ->
    boolOr (cb_rel xy) (cb_rel yz) >>= \ rel ->
    boolOr (cb_aff xy) (cb_aff yz) >>= \ aff ->
    newSrcWire >>= \ src ->
    emitNode (Compose (cb_src xy, cb_src yz) src) >>
    let cbxz = CodeBlock { cb_src = src, cb_aff = aff, cb_rel = rel } in
    return (Prod (Block cbxz) e)

opAff be = 
    asProd be >>= \ (b,e) ->
    asCode b >>= \ cb ->
    newBoolConst True >>= \ aff ->
    let cb' = cb { cb_aff = aff } in
    return (Prod (Block cb') e)

opRel be =
    asProd be >>= \ (b,e) ->
    asCode b >>= \ cb ->
    newBoolConst True >>= \ rel ->
    let cb' = cb { cb_rel = rel } in
    return (Prod (Block cb') e)

opDistrib ase =
    asProd ase >>= \ (a,se) ->
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (inC,b,c) ->
    let ab = Prod a b in
    let ac = Prod a c in
    let s' = Sum inC ab ac in
    return (Prod s' e)

opFactor se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (inCD,ab,cd) ->
    asProd ab >>= \ (a,b) ->
    asProd cd >>= \ (c,d) ->
    let sac = Sum inCD a c in
    let sbd = Sum inCD b d in
    return (Prod sac (Prod sbd e))

opMerge se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ sum ->
    newWireLabel >>= \ r ->
    emitNode (Merge sum r) >>
    return (Prod (Var r) e)

opAssert se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (inB,a,b) ->
    boolAssert inB >>
    -- should I include a transition in the graph,
    -- e.g. supporting `Maybe a` to just `a`?
    return (Prod b e)

opGT yxe =
    asProd yxe >>= \ (y,xe) ->
    asProd xe >>= \ (x,e) ->
    asNumber y >>= \ ny ->
    asNumber x >>= \ nx ->
    newBoolWire >>= \ bGT ->
    emitNode (GreaterThan (y,x) bGT) >>
    let onFalse = Prod (Num ny) (Num nx) in
    let onTrue  = Prod (Num nx) (Num ny) in
    let sum = Sum bGT onFalse onTrue in
    return (Prod sum e)

opIntroNum e = 
    newNumConst 0 >>= \ n -> 
    return (Prod (Num n) e)

opDigit d xe =
    asProd xe >>= \ (x,e) ->
    asNumber x >>= \ nx ->
    newNumConst (fromIntegral d) >>= \ nd ->
    newNumConst 10 >>= \ nD ->
    newNumWire >>= \ nxD ->
    newNumWire >>= \ r ->
    emitNode (Mul (nx,nD) nxD) >> -- x*10
    emitNode (Add (nxD,nd) r) >>  -- (x*10)+d
    return (Prod (Num r) e)


-- we'll actually create one wire per character...
-- (consequently, this is a very big operation)
--
-- We should be able to recover structure later.
textToWire :: String -> MkGraph Wire
textToWire [] =
    newBoolConst True >>= \ bDone ->
    newVoid >>= \ v ->
    return (Sum bDone v Unit)
textToWire (c:cs) =
    newBoolConst False >>= \ bDone ->
    textToWire cs >>= \ csWire ->
    newNumConst (fromIntegral (fromEnum c)) >>= \ cWire ->
    return (Sum bDone (Prod cWire csWire) Unit) 

asProd :: Wire -> MkGraph (Wire,Wire)
asUnit :: Wire -> MkGraph ()
asNumber :: Wire -> MkGraph NumWire
asSum :: Wire -> MkGraph (BoolWire, Wire, Wire)
asCode :: Wire -> MkGraph CodeBundle

asProd (Prod a b) = return (a,b)
asProd (Var w) = elab v (Prod <$> newVarWire <*> newVarWire) >>= asProd
asProd v = fail $ "product expected @ " ++ tydesc v

asUnit Unit = return ()
asUnit (Var w) = elab w (pure Unit) >>= asUnit
asUnit v = fail $ "expecting unit @ " ++ tydesc v

asNumber (Num a) = return a
asNumber (Var w) = elab w (Num <$> newNumber) >>= \ asNum 
asNumber v = fail $ "expecting number @ " ++ tydesc v

asSum (Sum c a b) = return (c,a,b)
asSum (Var w) = elab w (Sum <$> newBool <*> newVar <*> newVar) >>= asSum
asSum v = fail $ "expecting sum @ " ++ tydesc v 

asCode (Code cb) = return cb
asCode (Var w) = elab w (Code <$> newCodeBundle) >>= asCode where
asCode v = fail $ "expecting code @ " ++ tydesc v

elab :: WireLabel -> MkGraph Wire -> MkGraph Wire
elab v mkW = mkW >>= \ w -> emitNode (Elab v w) >> return w


boolOr  :: BoolWire -> BoolWire -> MkGraph BoolWire
boolAnd :: BoolWire -> BoolWire -> MkGraph BoolWire
boolNot :: BoolWire -> MkGraph BoolWire
boolAssert :: BoolWire -> MkGraph ()
boolDroppable :: Wire -> MkGraph BoolWire
boolCopyable :: Wire -> MkGraph BoolWire

boolOr a b = newBoolWire >>= \ r -> emitNode (BoolOr (a,b) r) >> return r
boolAnd a b = newBoolWire >>= \ r -> emitNode (BoolAnd (a,b) r) >> return r
boolNot b = newBoolWire >>= \ r -> emitNode (BoolNot b r) >> return r
boolAssert b = emitNode (BoolAssert b ())

boolDroppable (Var v) = newBoolWire >>= \ r -> emitNode (BoolDroppable v r) >> return r
boolDroppable (Num _) = newBoolConst True
boolDroppable (Block cb) = boolNot (cb_rel cb)
boolDroppable (Prod a b) = boolAnd <*> boolDroppable a <*> boolDroppable b 
boolDroppable Unit = newBoolConst True
boolDroppable (Sum _c a b) = boolAnd <*> boolDroppable a <*> boolDroppable b
boolDroppable (Seal _s v) = boolDroppable v

boolCopyable (Var v) = newBoolWire >>= \ r -> emitNode (BoolCopyable v r) >> return r
boolCopyable (Num _) = newBoolConst True
boolCopyable (Block cb) = boolNot (cb_aff cb)
boolCopyable (Prod a b) = boolAnd <*> boolCopyable a <*> boolCopyable b
boolCopyable Unit = newBoolConst True
boolCopyable (Sum _c a b) = boolAnd <*> boolCopyable a <*> boolCopyable b
boolCopyable (Seal _s v) = boolCopyable v

newVarWire :: MkGraph Wire
newWireLabel :: MkGraph WireLabel
newNumWire :: MkGraph NumWire
newSrcWire :: MkGraph SrcWire
newBoolWire :: MkGraph BoolWire
newCodeBundle :: MkGraph CodeBundle
newBoolConst :: Bool -> MkGraph BoolWire
newNumConst :: Rational -> MkGraph NumWire
newSrcConst :: [Op] -> MkGraph SrcWire
newVoid :: MkGraph Wire

newVarWire = Var <$> newWireLabel
newWireLabel = newLabel
newNumWire = newLabel
newSrcWire = newLabel
newBoolWire = newLabel
newCodeBundle = CodeBundle <$> newSrcWire <*> newBoolWire <*> newBoolWire
newBoolConst b = newBoolWire >>= \ r -> emitNode (BoolConst b r) >> return r
newNumConst n = newNumWire >>= \ r -> emitNode (NumConst n r) >> return r
newSrcConst ops = newSrcWire >>= \ r -> emitNode (SrcConst ops r) >> return r
newVoid = newWireLabel >>= \ lbl -> emitNode (Void () lbl) >> return (Var lbl)

