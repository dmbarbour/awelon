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
    , Wire(..), Node(..)
    , Label(..), CW(..)
    , CodeBundle(..), WireLabel, NumWire, BoolWire, SrcWire
    , textToWire, wireToText
    , nodeInputs, nodeOutputs, wireLabels
    ) where

import Control.Applicative
import Control.Monad 
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity
import Data.Ratio
import qualified Data.List as L
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

{- Thoughts: it might be easier to optimize if I keep more
    information about the construction of each value, e.g.

data CodeExpr 
    = CodeQuote Wire
    | CodeCompose CodeExpr CodeExpr
    | CodeConst [Op]
    | CodeVar (Label CodeExpr)
data BoolExpr
    = BoolNot BoolExpr
    | BoolOr BoolExpr BoolExpr
    | BoolAnd BoolExpr BoolExpr
    | BoolConst Bool
    | BoolVar (Label BoolExpr)
-}

type WireLabel = Label Wire
type BoolLabel = Label Bool
type NumLabel = Label Rational
type SrcLabel = Label [Op]
type NumWire = CW Rational
type BoolWire = CW Bool
type SrcWire = CW [Op]
data CW a = Stat !a | Dyn !(Label a) deriving (Show,Ord,Eq)
newtype Label n = Label { lbNum :: Integer } deriving (Ord,Eq)

-- NOTE: copy, drop, and most data plumbing operations are implicit
-- (nodes do assert elements are copyable or droppable)
data Node
    = Void () WireLabel -- a wire from nowhere (via `V`)...

    -- structure
    | ElabSum WireLabel (BoolLabel,WireLabel,WireLabel)
    | ElabProd WireLabel (WireLabel,WireLabel)
    | ElabNum WireLabel NumLabel
    | ElabCode WireLabel (SrcLabel,BoolLabel,BoolLabel) -- (src,rel,aff)
    | ElabUnit WireLabel ()
    | ElabSeal String WireLabel WireLabel

    -- math
    | Add (NumWire,NumWire) NumLabel
    | Neg NumLabel NumLabel
    | Mul (NumWire,NumWire) NumLabel
    | Inv NumLabel NumLabel
    | DivMod (NumWire,NumWire) (NumLabel,NumLabel) -- (dividend,divisor)→(quotient,remainder)
    | IsNonZero NumLabel BoolLabel
    | GreaterThan (NumWire,NumWire) BoolLabel -- (x,y) (x>y)
    
    -- booleans (for sums, affine, relevant, safety)
    | BoolOr (BoolLabel,BoolLabel) BoolLabel
    | BoolAnd (BoolLabel,BoolLabel) BoolLabel
    | BoolNot BoolLabel BoolLabel
    | BoolCopyable WireLabel BoolLabel
    | BoolDroppable WireLabel BoolLabel

    -- assertions
    | BoolAssert String BoolLabel () 

    -- higher order programming
    | Quote Wire SrcLabel
    | Compose (SrcWire,SrcWire) SrcLabel -- (x→y,y→z) x→z
    | Apply (SrcWire,Wire) WireLabel
    
    -- conditional behavior
    | CondAp (BoolLabel,SrcWire,Wire) WireLabel
    | Merge (BoolLabel,Wire,Wire) WireLabel -- (cond,onFalse,onTrue) result

    -- extended behaviors
    | Invoke String Wire WireLabel
    deriving (Show)

abc2graph :: [Op] -> Either String (WireLabel,[Node],Wire)
abc2graph = evalMkGraph ([],0) . mkGraph

mkGraph :: [Op] -> MkGraph (WireLabel,[Node],Wire)
mkGraph ops =
    newLabel >>= \ w0 ->
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
    boolNot inV >>= boolAssert "C" >>
    return a

opBL ops v = mkBlock ops >>= \ cb -> return (Prod (Block cb) v)

-- make a block (initially copyable and droppable)
mkBlock :: [Op] -> MkGraph CodeBundle
mkBlock ops = CodeBundle <$> newSrcConst ops 
                         <*> newBoolConst False
                         <*> newBoolConst False 

opTL txt v = textToWire txt >>= \ tw -> return (Prod tw v)

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

wireToText :: Wire -> Maybe String
wireToText (Sum (Stat False) (Prod (Num (Stat n)) cs) _void) = 
    (:) <$> numToChar n <*> wireToText cs
wireToText (Sum (Stat True) _void Unit) = Just []
wireToText _ = Nothing

numToChar :: Rational -> Maybe Char
numToChar r | charRange r = Just $! (toEnum . fromIntegral . numerator) r
numToChar _ = Nothing

charRange :: Rational -> Bool
charRange r = (1 == d) && (0 <= n) && (n <= 0x10ffff) where
    d = denominator r
    n = numerator r

opTok s@(':':_) w = return (Seal s w)
opTok ('.':s) w = unseal (':':s) w
opTok tok w = newLabel >>= \ r -> emitNode (Invoke tok w r) >> return (Var r)

unseal :: String -> Box
unseal s (Seal s' w) | (s == s') = return w
unseal s (Var w) = 
    newLabel >>= \ r -> 
    emitNode (ElabSeal s w r) >> 
    return (Var r)
unseal s v = fail $ "expecting sealed value{"++s++"} @ " ++ tydesc v

opCopy ae =
    asProd ae >>= \ (a,e) ->
    boolCopyable a >>= boolAssert "copyable" >>
    return (Prod a (Prod a e))
opDrop ae =
    asProd ae >>= \ (a,e) ->
    boolDroppable a >>= boolAssert "droppable" >>
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
numAddC, numMulC :: Rational -> NumWire -> MkGraph NumWire
numNeg, numInv, numInv' :: NumWire -> MkGraph NumWire
numDivMod, numDivMod' :: NumWire -> NumWire -> MkGraph (NumWire,NumWire)

numAdd (Stat a) b = numAddC a b
numAdd a (Stat b) = numAddC b a
numAdd a b = newLabel >>= \ c -> emitNode (Add (a,b) c) >> return (Dyn c)

numAddC n (Stat v) = return (Stat (n+v))
numAddC 0 v = return v
numAddC r v = newLabel >>= \ c -> emitNode (Add (Stat r,v) c) >> return (Dyn c)

numMul (Stat a) b = numMulC a b
numMul a (Stat b) = numMulC b a
numMul a b  = newLabel >>= \ c -> emitNode (Mul (a,b) c) >> return (Dyn c)

numMulC n (Stat v) = return (Stat (n*v))
numMulC 1 v = return v
numMulC r v = newLabel >>= \ c -> emitNode (Mul (Stat r,v) c) >> return (Dyn c)

numNeg (Stat a) = return (Stat (negate a))
numNeg (Dyn a)  = newLabel >>= \ b -> emitNode (Neg a b) >> return (Dyn b)

numInv v = assertNonZero v >> numInv' v
numInv' (Stat a) = return (Stat (recip a))
numInv' (Dyn a)  = newLabel >>= \ r -> emitNode (Inv a r) >> return (Dyn r)

numDivMod a b = assertNonZero b >> numDivMod' a b
numDivMod' (Stat a) (Stat b) =
    let (q,r) = abcDivMod a b in
    return (Stat q, Stat r)
numDivMod' a b =
    newLabel >>= \ qu ->
    newLabel >>= \ rm ->
    emitNode (DivMod (a,b) (qu,rm)) >>
    return (Dyn qu, Dyn rm)

assertNonZero :: NumWire -> MkGraph ()
assertNonZero (Stat 0) = fail "static divide by zero"
assertNonZero (Stat _) = return ()
assertNonZero (Dyn a) = 
    newLabel >>= \ b -> 
    emitNode (IsNonZero a b) >> 
    boolAssert "non-zero divisor" (Dyn b)

opApply bxe =
    asProd bxe >>= \ (b,xe) ->
    asProd xe >>= \ (x,e) ->
    asCode b >>= \ cb ->
    let src = cb_src cb in
    apply src x >>= \ x' ->
    return (Prod x' e)

-- note: 'apply' will inline obviously non-recursive code
apply :: SrcWire -> Wire -> MkGraph Wire
apply (Stat ops) arg | nonRecursive ops = runABC ops arg
apply src arg = 
    newLabel >>= \ result ->
    emitNode (Apply (src,arg) result) >>
    return (Var result)

-- a very conservative estimate of non-recursive operation...
--
-- Presumably, it wouldn't be too difficult to develop a more
-- precise test, but this will do for now. We can't have a 
-- fixpoint without copying a value containing a block.
nonRecursive :: [Op] -> Bool
nonRecursive = L.notElem Op_copy

opCond bse =
    asProd bse >>= \ (b,se) ->
    asProd se >>= \ (s,e) ->
    asSum  s >>= \ (inY, x, y) ->
    asCode b >>= \ cb ->
    let src = cb_src cb in
    boolNot (cb_rel cb) >>= boolAssert "op ? with relevant block" >>
    boolNot inY >>= \ inX ->
    condAp inX src x >>= \ x' ->
    let s' = Sum inY x' y in
    return (Prod s' e)

condAp :: BoolWire -> SrcWire -> Wire -> MkGraph Wire
condAp (Stat False) _ _ = newVoid 
condAp (Stat True) src arg = apply src arg
condAp (Dyn cond) src arg =
    newLabel >>= \ result ->
    emitNode (CondAp (cond,src,arg) result) >>
    return (Var result)

opQuote xe =
    asProd xe >>= \ (x,e) ->
    boolCopyable x >>= boolNot >>= \ aff ->
    boolDroppable x >>= boolNot >>= \ rel ->
    newLabel >>= \ src ->
    emitNode (Quote x src) >>
    let cb = CodeBundle { cb_src = Dyn src, cb_rel = rel, cb_aff = aff } in
    return (Prod (Block cb) e)

opCompose yxe =
    asProd yxe >>= \ (yz,xe) ->
    asProd xe >>= \ (xy,e) ->
    asCode xy >>= \ cbxy ->
    asCode yz >>= \ cbyz ->
    composeSrc (cb_src cbxy) (cb_src cbyz) >>= \ src ->
    boolOr (cb_rel cbxy) (cb_rel cbyz) >>= \ rel ->
    boolOr (cb_aff cbxy) (cb_aff cbyz) >>= \ aff ->
    let cbxz = CodeBundle { cb_src = src, cb_aff = aff, cb_rel = rel } in
    return (Prod (Block cbxz) e)

composeSrc :: SrcWire -> SrcWire -> MkGraph SrcWire
composeSrc (Stat xy) (Stat yz) = return (Stat (xy++yz))
composeSrc xy yz = 
    newLabel >>= \ src -> 
    emitNode (Compose (xy,yz) src) >> 
    return (Dyn src)

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
    mergeSum b x y >>= \ r ->
    return (Prod r e)

-- if we know we're in left or right, keep that info
mergeSum :: BoolWire -> Wire -> Wire -> MkGraph Wire
mergeSum (Stat False) a _ = return a
mergeSum (Stat True)  _ b = return b
mergeSum (Dyn c) a b = mergeSum' c a b

-- maintain as much known structure as feasible
mergeSum' :: BoolLabel -> Wire -> Wire -> MkGraph Wire
mergeSum' _ (Var a) (Var b) | (a == b) = return (Var a)
mergeSum' _ (Num a) (Num b) | (a == b) = return (Num a)
mergeSum' _ (Block a) (Block b) | (a == b) = return (Block a)
mergeSum' c (Prod a1 a2) (Prod b1 b2) = 
    mergeSum' c a1 b1 >>= \ r1 ->
    mergeSum' c a2 b2 >>= \ r2 ->
    return (Prod r1 r2)
mergeSum' _ Unit Unit = return Unit
mergeSum' c (Sum aInR aL aR) (Sum bInR bL bR) | (aInR == bInR) =
    mergeSum' c aL bL >>= \ rL ->
    mergeSum' c aR bR >>= \ rR ->
    return (Sum aInR rL rR)
mergeSum' c (Sum aInR aL aR) (Sum bInR bL bR) =
    let sumInR = Dyn c in
    boolNot sumInR >>= \ sumInL ->
    boolAnd sumInL aInR >>= \ inRofA ->
    boolAnd sumInR bInR >>= \ inRofB ->
    boolOr inRofA inRofB >>= \ inR ->
    mergeSum' c aL bL >>= \ rL ->
    mergeSum' c aR bR >>= \ rR ->
    return (Sum inR rL rR)
mergeSum' c (Seal s a) (Seal s' b) | (s == s') =
    mergeSum' c a b >>= \ r ->
    return (Seal s r)
mergeSum' c a b =
    newLabel >>= \ r ->
    emitNode (Merge (c,a,b) r) >>
    return (Var r)

opAssert se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (inB,_a,b) ->
    boolAssert "K" inB >>
    -- should I include a transition in the graph,
    -- e.g. supporting `Maybe a` to just `a`?
    return (Prod b e)

opGT xye =
    asProd xye >>= \ (x,ye) ->
    asProd ye >>= \ (y,e) ->
    asNumber x >>= \ nx ->
    asNumber y >>= \ ny ->
    testGT ny nx >>= \ bGT -> -- testing y > x (such that `#4 #3 >` is true)
    let onFalse = Prod (Num ny) (Num nx) in
    let onTrue  = Prod (Num nx) (Num ny) in
    let s = Sum bGT onFalse onTrue in
    return (Prod s e)

testGT :: NumWire -> NumWire -> MkGraph BoolWire
testGT (Stat x) (Stat y) = return (Stat (x > y))
testGT x y = newLabel >>= \ b -> emitNode (GreaterThan (x,y) b) >> return (Dyn b)

opIntroNum e = 
    newNumConst 0 >>= \ n -> 
    return (Prod (Num n) e)

opDigit d xe =
    asProd xe >>= \ (x,e) ->
    asNumber x >>= numMulC 10 >>= numAddC (fromIntegral d) >>= \ x' ->
    return (Prod (Num x') e)

asProd :: Wire -> MkGraph (Wire,Wire)
asUnit :: Wire -> MkGraph ()
asNumber :: Wire -> MkGraph NumWire
asSum :: Wire -> MkGraph (BoolWire, Wire, Wire)
asCode :: Wire -> MkGraph CodeBundle

asProd (Prod a b) = return (a,b)
asProd (Var w) = 
    newLabel >>= \ a ->
    newLabel >>= \ b ->
    emitNode (ElabProd w (a,b)) >>
    return (Var a, Var b)
asProd v = fail $ "product expected @ " ++ tydesc v

asUnit Unit = return ()
asUnit (Var w) = emitNode (ElabUnit w ()) >> return ()
asUnit v = fail $ "expecting unit @ " ++ tydesc v

asNumber (Num a) = return a
asNumber (Var w) = newLabel >>= \ r -> emitNode (ElabNum w r) >> return (Dyn r)
asNumber v = fail $ "expecting number @ " ++ tydesc v

asSum (Sum c a b) = return (c,a,b)
asSum (Var w) = 
    newLabel >>= \ c ->
    newLabel >>= \ a ->
    newLabel >>= \ b ->
    emitNode (ElabSum w (c,a,b)) >>
    return (Dyn c,Var a, Var b)
asSum v = fail $ "expecting sum @ " ++ tydesc v 

asCode (Block cb) = return cb
asCode (Var w) = 
    newLabel >>= \ src ->
    newLabel >>= \ rel ->
    newLabel >>= \ aff ->
    emitNode (ElabCode w (src,rel,aff)) >>
    let cb = CodeBundle { cb_src = Dyn src, cb_rel = Dyn rel, cb_aff = Dyn aff } in
    return cb
asCode v = fail $ "expecting code @ " ++ tydesc v

boolOr  :: BoolWire -> BoolWire -> MkGraph BoolWire
boolAnd :: BoolWire -> BoolWire -> MkGraph BoolWire
boolNot :: BoolWire -> MkGraph BoolWire
boolAssert :: String -> BoolWire -> MkGraph ()
boolDroppable :: Wire -> MkGraph BoolWire
boolCopyable :: Wire -> MkGraph BoolWire

boolOr a b | (a == b) = return a
boolOr (Stat False) b = return b
boolOr a (Stat False) = return a
boolOr (Stat True) _ = return (Stat True)
boolOr _ (Stat True) = return (Stat True)
boolOr (Dyn a) (Dyn b) = 
    newLabel >>= \ r -> 
    emitNode (BoolOr (a,b) r) >> 
    return (Dyn r)

boolAnd a b | (a == b) = return a
boolAnd (Stat True) b = return b
boolAnd a (Stat True) = return a
boolAnd (Stat False) _ = return (Stat False)
boolAnd _ (Stat False) = return (Stat False)
boolAnd (Dyn a) (Dyn b) = 
    newLabel >>= \ r -> 
    emitNode (BoolAnd (a,b) r) >> 
    return (Dyn r)

boolNot (Stat a) = return (Stat (not a))
boolNot (Dyn a) = newLabel >>= \ r -> emitNode (BoolNot a r) >> return (Dyn r)

boolAssert _ (Stat True) = return ()
boolAssert msg (Stat False) = fail $ "static assertion failure: " ++ msg
boolAssert msg (Dyn cond) = emitNode (BoolAssert msg cond ())

boolDroppable (Var v) = newLabel >>= \ r -> emitNode (BoolDroppable v r) >> return (Dyn r)
boolDroppable (Num _) = newBoolConst True
boolDroppable (Block cb) = boolNot (cb_rel cb)
boolDroppable (Prod a b) = 
    boolDroppable a >>= \ droppableA ->
    boolDroppable b >>= \ droppableB ->
    boolAnd droppableA droppableB
boolDroppable Unit = newBoolConst True
boolDroppable (Sum (Stat True) _a b) = boolDroppable b
boolDroppable (Sum (Stat False) a _b) = boolDroppable a
boolDroppable (Sum inB a b) =
    boolNot inB >>= \ inA ->
    boolDroppable a >>= \ da ->
    boolDroppable b >>= \ db ->
    boolAnd inA da >>= \ dropA ->
    boolAnd inB db >>= \ dropB ->
    boolOr dropA dropB
boolDroppable (Seal _s v) = boolDroppable v

boolCopyable (Var v) = newLabel >>= \ r -> emitNode (BoolCopyable v r) >> return (Dyn r)
boolCopyable (Num _) = newBoolConst True
boolCopyable (Block cb) = boolNot (cb_aff cb)
boolCopyable (Prod a b) = 
    boolCopyable a >>= \ copyableA ->
    boolCopyable b >>= \ copyableB ->
    boolAnd copyableA copyableB
boolCopyable Unit = newBoolConst True
boolCopyable (Sum (Stat True) _a b) = boolCopyable b
boolCopyable (Sum (Stat False) a _b) = boolCopyable a
boolCopyable (Sum inB a b) = 
    boolNot inB >>= \ inA ->
    boolCopyable a >>= \ ca ->
    boolCopyable b >>= \ cb ->
    boolAnd inA ca >>= \ copyA ->
    boolAnd inB cb >>= \ copyB ->
    boolOr copyA copyB
boolCopyable (Seal _s v) = boolCopyable v

newBoolConst :: Bool -> MkGraph BoolWire
newNumConst :: Rational -> MkGraph NumWire
newSrcConst :: [Op] -> MkGraph SrcWire
newVoid :: MkGraph Wire

newBoolConst = return . Stat
newNumConst = return . Stat
newSrcConst = return . Stat

newVoid = 
    newLabel >>= \ lbl -> 
    emitNode (Void () lbl) >> 
    return (Var lbl)

instance Show (Label n) where 
    showsPrec _ (Label n) = showChar '_' . shows n

instance Show Wire where 
    showsPrec _ = showString . tydesc


nodeInputs, nodeOutputs :: Node -> [Integer]
wireLabels :: Wire -> [Integer]

nodeInputS, nodeOutputS :: Node -> State [Integer] ()
wireLabelS :: Wire -> State [Integer] ()

nodeInputs = flip execState [] . nodeInputS
nodeOutputs = flip execState [] . nodeOutputS
wireLabels = flip execState [] . wireLabelS

em :: Label a -> State [Integer] ()
em (Label n) = modify (n:)

cw :: CW a -> State [Integer] ()
cw (Dyn lbl) = em lbl
cw (Stat _) = return ()

nodeInputS (ElabSum  v _) = em v
nodeInputS (ElabProd v _) = em v
nodeInputS (ElabNum  v _) = em v
nodeInputS (ElabCode v _) = em v
nodeInputS (ElabUnit v _) = em v
nodeInputS (ElabSeal _ v _) = em v
nodeInputS (Void () _) = return ()
nodeInputS (Add (a,b) _) = cw a >> cw b
nodeInputS (Neg a _) = em a
nodeInputS (Mul (a,b) _) = cw a >> cw b
nodeInputS (Inv a _) = em a
nodeInputS (DivMod (a,b) _) = cw a >> cw b
nodeInputS (IsNonZero a _) = em a
nodeInputS (GreaterThan (a,b) _) = cw a >> cw b
nodeInputS (BoolOr (a,b) _) = em a >> em b
nodeInputS (BoolAnd (a,b) _) = em a >> em b
nodeInputS (BoolNot a _) = em a
nodeInputS (BoolCopyable a _) = em a
nodeInputS (BoolDroppable a _) = em a
nodeInputS (BoolAssert _ a _) = em a
nodeInputS (Quote w _) = wireLabelS w
nodeInputS (Compose (a,b) _) = cw a >> cw b
nodeInputS (Apply (src, arg) _) = cw src >> wireLabelS arg
nodeInputS (CondAp (cond, src, arg) _) = em cond >> cw src >> wireLabelS arg 
nodeInputS (Merge (c, a, b) _) = em c >> wireLabelS a >> wireLabelS b
nodeInputS (Invoke _ w _) = wireLabelS w

nodeOutputS (ElabSum _ (c,a,b)) = em c >> em a >> em b
nodeOutputS (ElabProd _ (a,b)) = em a >> em b
nodeOutputS (ElabNum _ n) = em n
nodeOutputS (ElabCode _ (src,rel,aff)) = em src >> em rel >> em aff
nodeOutputS (ElabUnit _ ()) = return ()
nodeOutputS (ElabSeal _ _ w) = em w
nodeOutputS (Void _ w) = em w
nodeOutputS (Add _ r) = em r
nodeOutputS (Neg _ r) = em r
nodeOutputS (Mul _ r) = em r
nodeOutputS (Inv _ r) = em r
nodeOutputS (DivMod _ (q,r)) = em q >> em r
nodeOutputS (IsNonZero _ r) = em r
nodeOutputS (GreaterThan _ r) = em r
nodeOutputS (BoolOr _ r) = em r
nodeOutputS (BoolAnd _ r) = em r
nodeOutputS (BoolNot _ r) = em r
nodeOutputS (BoolCopyable _ r) = em r
nodeOutputS (BoolDroppable _ r) = em r
nodeOutputS (BoolAssert _ _ ()) = return ()
nodeOutputS (Quote _ r) = em r
nodeOutputS (Compose _ r) = em r
nodeOutputS (Apply _ r) = em r
nodeOutputS (CondAp _ r) = em r
nodeOutputS (Merge _ r) = em r
nodeOutputS (Invoke _ _ r) = em r

wireLabelS (Var v) = em v
wireLabelS (Num n) = cw n
wireLabelS (Block cb) = cw (cb_src cb) >> cw (cb_aff cb) >> cw (cb_rel cb)
wireLabelS (Prod a b) = wireLabelS a >> wireLabelS b
wireLabelS Unit = return ()
wireLabelS (Sum c a b) = cw c >> wireLabelS a >> wireLabelS b
wireLabelS (Seal _ v) = wireLabelS v
