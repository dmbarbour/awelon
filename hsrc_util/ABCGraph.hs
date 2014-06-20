{-# LANGUAGE PatternGuards #-} 

-- | Translate ABC code into a box-and-wire graph
-- (to later include steps for simplification and optimization)
--
-- An earlier effort at this collapsed to cumulative complexity,
-- presumably because it attempted too many responsibilities, such
-- as partial evaluation, inlining, and conditionals.
--
-- Sadly, conditional behavior doesn't translate conveniently from ABC
-- into Haskell. This can be difficult for the VRWLCDFMK operators. One
-- option is to model all wires as potentially inactive, and perhaps to
-- track the activity of individual wires.
--
-- Note: one naive effort modeled sums using (Bool,a,b) triples, but 
-- did not tie the boolean condition together with the a,b values. This
-- was unfortunate, as it failed to compose when we have 'deep' sums in
-- the structure of the a or b values. I am going to need to rewrite the
-- graph model to more precisely track activity (liveness). I would also
-- like to integrate constants more precisely, as a simple form of partial
-- evaluation (regardless of redundancy). 
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
    , Wire(..),Label(..),Node(..)
    , CodeBundle(..), WireLabel, NumWire, BoolWire, SrcWire
    , nodeInputs, nodeOutputs, wireLabels
    ) where

import Control.Applicative
import Control.Monad 
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity

import ABC.Operators

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
    } deriving (Eq,Show)

type WireLabel = Label Wire
type NumWire = Label Rational
type BoolWire = Label Bool
type SrcWire = Label [Op]
newtype Label n = Label { lbNum :: Integer } deriving (Ord,Eq)

-- NOTE: copy, drop, and most data plumbing operations are implicit
-- (nodes do assert elements are copyable or droppable)
data Node
    = Void () WireLabel -- a wire from nowhere (via `V`)...

    -- structure
    | ElabSum WireLabel (BoolWire,WireLabel,WireLabel)
    | ElabProd WireLabel (WireLabel,WireLabel)
    | ElabNum WireLabel NumWire
    | ElabCode WireLabel CodeBundle
    | ElabUnit WireLabel ()
    | ElabSeal String WireLabel WireLabel

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
    | Merge (BoolWire,Wire,Wire) WireLabel -- (cond,onFalse,onTrue) result

    -- extended behaviors
    | Invoke String Wire WireLabel
    deriving (Show)

abc2graph :: [Op] -> Either String (WireLabel,[Node],Wire)
abc2graph = evalMkGraph ([],0) . mkGraph

mkGraph :: [Op] -> MkGraph (WireLabel,[Node],Wire)
mkGraph ops =
    newWireLabel >>= \ w0 ->
    runABC ops (Var w0) >>= \ wf ->
    -- TODO: simplify; optimize; elaborate w0
    --  but I can do this later
    gets (reverse . fst) >>= \ nodes ->
    return (w0,nodes,wf)

type MkGraph = StateT GCX (ErrorT String Identity)
type GCX = ([Node],Integer)
type Box = Wire -> MkGraph Wire

evalMkGraph :: GCX -> MkGraph a -> Either String a
evalMkGraph gcx = runIdentity . runErrorT . flip evalStateT gcx

newLabel :: MkGraph (Label t)
newLabel =
    get >>= \ (ns,s) ->
    let s' = s + 1 in
    s' `seq` put (ns,s') >>
    return (Label s')

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
    asSum av >>= \ (inV,a,_v) ->
    boolNot inV >>= boolAssert >>
    return a

opBL ops v = mkBlock ops >>= \ cb -> return (Prod (Block cb) v)

-- make a block (initially copyable and droppable)
mkBlock :: [Op] -> MkGraph CodeBundle
mkBlock ops = CodeBundle <$> newSrcConst ops 
                         <*> newBoolConst False
                         <*> newBoolConst False 

opTL txt v = textToWire txt >>= \ txtWire -> return (Prod txtWire v)

opTok s@(':':_) w = return (Seal s w)
opTok ('.':s) w = unseal (':':s) w
opTok tok w = newLabel >>= \ r -> emitNode (Invoke tok w r) >> return (Var r)

unseal :: String -> Box
unseal s (Seal s' w) | (s == s') = return w
unseal s (Var w) = 
    newWireLabel >>= \ r -> 
    emitNode (ElabSeal s w r) >> 
    return (Var r)
unseal s v = fail $ "expecting {:"++s++"} @ " ++ tydesc v

opCopy ae =
    asProd ae >>= \ (a,e) ->
    boolCopyable a >>= boolAssert >>
    return (Prod a (Prod a e))
opDrop ae =
    asProd ae >>= \ (a,e) ->
    boolDroppable a >>= boolAssert >>
    return e

opAdd abe =
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNumber a >>= \ na ->
    asNumber b >>= \ nb ->
    numAdd na nb >>= \ nr ->
    return (Prod (Num nr) e)

opMul abe =
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNumber a >>= \ na ->
    asNumber b >>= \ nb ->
    numMul na nb >>= \ nr ->
    return (Prod (Num nr) e)

opNeg ae =
    asProd ae >>= \ (a,e) ->
    asNumber a >>= \ na ->
    numNeg na >>= \ nr ->
    return (Prod (Num nr) e)

opInv ae = 
    asProd ae >>= \ (a,e) ->
    asNumber a >>= \ na ->
    numInv na >>= \ nr ->
    return (Prod (Num nr) e)

opDivMod bae = 
    asProd bae >>= \ (b, ae) ->
    asProd ae >>= \ (a,e) ->
    asNumber b >>= \ divisor ->
    asNumber a >>= \ dividend -> 
    numDivMod dividend divisor >>= \ (quotient,remainder) ->
    return (Prod (Num remainder) (Prod (Num quotient) e))

numAdd, numMul :: NumWire -> NumWire -> MkGraph NumWire
numNeg, numInv :: NumWire -> MkGraph NumWire
numDivMod :: NumWire -> NumWire -> MkGraph (NumWire,NumWire)

numAdd na nb = newNumWire >>= \ nc -> emitNode (Add (na,nb) nc) >> return nc
numMul na nb = newNumWire >>= \ nc -> emitNode (Mul (na,nb) nc) >> return nc
numNeg na = newNumWire >>= \ nr -> emitNode (Neg na nr) >> return nr
numInv na = 
    newNumWire >>= \ nr -> 
    assertNonZero na >>
    emitNode (Inv na nr) >> 
    return nr
numDivMod dividend divisor = 
    newNumWire >>= \ quotient ->
    newNumWire >>= \ remainder ->
    assertNonZero divisor >>
    emitNode (DivMod (dividend,divisor) (quotient,remainder)) >>
    return (quotient,remainder)



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

opCond bse =
    asProd bse >>= \ (b,se) ->
    asProd se >>= \ (s,e) ->
    asSum  s >>= \ (inY, x, y) ->
    asCode b >>= \ cb ->
    let src = cb_src cb in
    boolNot (cb_rel cb) >>= boolAssert >>
    boolNot inY >>= \ inX ->
    newLabel >>= \ x' ->
    emitNode (CondAp (inX,src,x) x') >>
    let s' = Sum inY (Var x') y in
    return (Prod s' e)

opQuote xe =
    asProd xe >>= \ (x,e) ->
    boolCopyable x >>= boolNot >>= \ aff ->
    boolDroppable x >>= boolNot >>= \ rel ->
    newSrcWire >>= \ src ->
    emitNode (Quote x src) >>
    let cb = CodeBundle { cb_src = src, cb_rel = rel, cb_aff = aff } in
    return (Prod (Block cb) e)

opCompose yxe =
    asProd yxe >>= \ (yz,xe) ->
    asProd xe >>= \ (xy,e) ->
    asCode xy >>= \ cbxy ->
    asCode yz >>= \ cbyz ->
    boolOr (cb_rel cbxy) (cb_rel cbyz) >>= \ rel ->
    boolOr (cb_aff cbxy) (cb_aff cbyz) >>= \ aff ->
    newSrcWire >>= \ src ->
    emitNode (Compose (cb_src cbxy, cb_src cbyz) src) >>
    let cbxz = CodeBundle { cb_src = src, cb_aff = aff, cb_rel = rel } in
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
    asSum s >>= \ (b,x,y) ->
    newWireLabel >>= \ r ->
    emitNode (Merge (b,x,y) r) >>
    return (Prod (Var r) e)

opAssert se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (inB,_a,b) ->
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
    emitNode (GreaterThan (ny,nx) bGT) >>
    let onFalse = Prod (Num ny) (Num nx) in
    let onTrue  = Prod (Num nx) (Num ny) in
    let s' = Sum bGT onFalse onTrue in
    return (Prod s' e)

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
    return (Sum bDone (Prod (Num cWire) csWire) Unit) 

asProd :: Wire -> MkGraph (Wire,Wire)
asUnit :: Wire -> MkGraph ()
asNumber :: Wire -> MkGraph NumWire
asSum :: Wire -> MkGraph (BoolWire, Wire, Wire)
asCode :: Wire -> MkGraph CodeBundle

asProd (Prod a b) = return (a,b)
asProd (Var w) = 
    newWireLabel >>= \ a ->
    newWireLabel >>= \ b ->
    emitNode (ElabProd w (a,b)) >>
    return (Var a, Var b)
asProd v = fail $ "product expected @ " ++ tydesc v

asUnit Unit = return ()
asUnit (Var w) = emitNode (ElabUnit w ()) >> return ()
asUnit v = fail $ "expecting unit @ " ++ tydesc v

asNumber (Num a) = return a
asNumber (Var w) = newNumWire >>= \ r -> emitNode (ElabNum w r) >> return r
asNumber v = fail $ "expecting number @ " ++ tydesc v

asSum (Sum c a b) = return (c,a,b)
asSum (Var w) = 
    newBoolWire >>= \ c ->
    newWireLabel >>= \ a ->
    newWireLabel >>= \ b ->
    emitNode (ElabSum w (c,a,b)) >>
    return (c,Var a, Var b)
asSum v = fail $ "expecting sum @ " ++ tydesc v 

asCode (Block cb) = return cb
asCode (Var w) = newCodeBundle >>= \ r -> emitNode (ElabCode w r) >> return r
asCode v = fail $ "expecting code @ " ++ tydesc v

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
boolDroppable (Prod a b) = 
    boolDroppable a >>= \ droppableA ->
    boolDroppable b >>= \ droppableB ->
    boolAnd droppableA droppableB
boolDroppable Unit = newBoolConst True
boolDroppable (Sum _c a b) = 
    boolDroppable a >>= \ droppableA ->
    boolDroppable b >>= \ droppableB ->
    boolAnd droppableA droppableB
boolDroppable (Seal _s v) = boolDroppable v

boolCopyable (Var v) = newBoolWire >>= \ r -> emitNode (BoolCopyable v r) >> return r
boolCopyable (Num _) = newBoolConst True
boolCopyable (Block cb) = boolNot (cb_aff cb)
boolCopyable (Prod a b) = 
    boolCopyable a >>= \ copyableA ->
    boolCopyable b >>= \ copyableB ->
    boolAnd copyableA copyableB
boolCopyable Unit = newBoolConst True
boolCopyable (Sum _c a b) = 
    boolCopyable a >>= \ copyableA ->
    boolCopyable b >>= \ copyableB ->
    boolAnd copyableA copyableB
boolCopyable (Seal _s v) = boolCopyable v

newWireLabel :: MkGraph WireLabel
newNumWire :: MkGraph NumWire
newSrcWire :: MkGraph SrcWire
newBoolWire :: MkGraph BoolWire
newCodeBundle :: MkGraph CodeBundle
newBoolConst :: Bool -> MkGraph BoolWire
newNumConst :: Rational -> MkGraph NumWire
newSrcConst :: [Op] -> MkGraph SrcWire
newVoid :: MkGraph Wire

newWireLabel = newLabel
newNumWire = newLabel
newSrcWire = newLabel
newBoolWire = newLabel
newCodeBundle = CodeBundle <$> newSrcWire <*> newBoolWire <*> newBoolWire
newBoolConst b = newBoolWire >>= \ r -> emitNode (BoolConst b r) >> return r
newNumConst n = newNumWire >>= \ r -> emitNode (NumConst n r) >> return r
newSrcConst ops = newSrcWire >>= \ r -> emitNode (SrcConst ops r) >> return r
newVoid = newWireLabel >>= \ lbl -> emitNode (Void () lbl) >> return (Var lbl)



instance Show (Label n) where 
    showsPrec _ (Label n) = showChar '_' . shows n

instance Show Wire where 
    showsPrec _ (Var n) = shows n
    showsPrec _ (Num n) = shows n
    showsPrec _ (Block cb) = shows (cb_src cb)
    showsPrec _ (Prod a b) = 
        showChar '(' . shows a . 
        showChar '*' . shows b .
        showChar ')'
    showsPrec _ Unit = showString "1"
    showsPrec _ (Sum _ a b) =
        showChar '(' . shows a .
        showChar '+' . shows b .
        showChar ')'
    showsPrec _ (Seal s v) = 
        shows v . 
        showChar '{' . showString s . showChar '}'


nodeInputs, nodeOutputs :: Node -> [Integer]
wireLabels :: Wire -> [Integer]

nodeInputs (ElabSum  (Label n) _) = [n]
nodeInputs (ElabProd (Label n) _) = [n]
nodeInputs (ElabNum  (Label n) _) = [n]
nodeInputs (ElabCode (Label n) _) = [n]
nodeInputs (ElabUnit (Label n) _) = [n]
nodeInputs (ElabSeal _ (Label n) _) = [n]
nodeInputs (Void () _) = []
nodeInputs (NumConst _ _) = []
nodeInputs (Add (Label a, Label b) _) = [a,b]
nodeInputs (Neg (Label a) _) = [a]
nodeInputs (Mul (Label a, Label b) _) = [a,b]
nodeInputs (Inv (Label a) _) = [a]
nodeInputs (DivMod (Label a, Label b) _) = [a,b]
nodeInputs (IsNonZero (Label a) _) = [a]
nodeInputs (GreaterThan (Label a, Label b) _) = [a,b]
nodeInputs (BoolConst _ _) = []
nodeInputs (BoolOr (Label a, Label b) _) = [a,b]
nodeInputs (BoolAnd (Label a, Label b) _) = [a,b]
nodeInputs (BoolNot (Label a) _) = [a]
nodeInputs (BoolCopyable (Label a) _) = [a]
nodeInputs (BoolDroppable (Label a) _) = [a]
nodeInputs (BoolAssert (Label a) _) = [a]
nodeInputs (SrcConst _ _) = []
nodeInputs (Quote w _) = wireLabels w
nodeInputs (Compose (Label a, Label b) _) = [a,b]
nodeInputs (Apply (Label a, w) _) = a : wireLabels w
nodeInputs (CondAp (Label c, Label b, w) _) = c : b : wireLabels w
nodeInputs (Merge (Label c, a, b) _) = c : (wireLabels a ++ wireLabels b)
nodeInputs (Invoke _ w _) = wireLabels w

nodeOutputs (ElabSum _ (Label c,Label a,Label b)) = [c,a,b]
nodeOutputs (ElabProd _ (Label a,Label b)) = [a,b]
nodeOutputs (ElabNum _ (Label n)) = [n]
nodeOutputs (ElabCode _ cb) = wireLabels (Block cb)
nodeOutputs (ElabUnit _ ()) = []
nodeOutputs (ElabSeal _ _ (Label w)) = [w]
nodeOutputs (Void _ (Label w)) = [w]
nodeOutputs (NumConst _ (Label n)) = [n]
nodeOutputs (Add _ (Label n)) = [n]
nodeOutputs (Neg _ (Label n)) = [n]
nodeOutputs (Mul _ (Label n)) = [n]
nodeOutputs (Inv _ (Label n)) = [n]
nodeOutputs (DivMod _ (Label q, Label r)) = [q,r]
nodeOutputs (IsNonZero _ (Label b)) = [b]
nodeOutputs (GreaterThan _ (Label b)) = [b]
nodeOutputs (BoolConst _ (Label b)) = [b]
nodeOutputs (BoolOr _ (Label b)) = [b]
nodeOutputs (BoolAnd _ (Label b)) = [b]
nodeOutputs (BoolNot _ (Label b)) = [b]
nodeOutputs (BoolCopyable _ (Label b)) = [b]
nodeOutputs (BoolDroppable _ (Label b)) = [b]
nodeOutputs (BoolAssert _ ()) = []
nodeOutputs (SrcConst _ (Label s)) = [s]
nodeOutputs (Quote _ (Label s)) = [s]
nodeOutputs (Compose _ (Label s)) = [s]
nodeOutputs (Apply _ (Label r)) = [r]
nodeOutputs (CondAp _ (Label r)) = [r]
nodeOutputs (Merge _ (Label r)) = [r]
nodeOutputs (Invoke _ _ (Label r)) = [r]

wireLabels = flip wl [] where
    wl (Var v) = em v
    wl (Num n) = em n
    wl (Block cb) = em (cb_src cb) . em (cb_aff cb) . em (cb_rel cb)
    wl (Prod a b) = wl a . wl b
    wl Unit = id
    wl (Sum c a b) = em c . wl a . wl b
    wl (Seal _ v) = wl v
    em lb lst = lbNum lb : lst

