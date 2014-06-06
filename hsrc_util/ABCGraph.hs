
-- | Translate ABC code into a box-and-wire graph
--
-- An earlier effort at this collapsed to cumulative complexity,
-- presumably because it attempted too many responsibilities (such
-- as partial evaluation and inlining). This design is simpler but
-- will require extra passes.

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

data Node
    = Elab WireLabel Wire -- elaborate structure of a wire
    | Void () WireLabel -- a wire from nowhere (via `V`)...

    -- math
    | NumConst Rational NumWire
    | Add  (NumWire,NumWire) NumWire
    | Neg  NumWire NumWire
    | Mul  (NumWire,NumWire) NumWire
    | Inv  NumWire NumWire
    | DivMod (NumWire,NumWire) (NumWire,NumWire) -- (dividend,divisor)→(quotient,remainder)
    
    -- booleans (for sums, affine, relevant)
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
    | Compose (SrcWire,SrcWire) SrcWire
    | Apply (SrcWire,Wire) WireLabel
    
    -- conditional behavior
    | CondAp (BoolWire,SrcWire,Wire) WireLabel
    | Merge  Wire Wire
    | GreaterThan (NumWire,NumWire) BoolWire
    | IsNonZero NumWire BoolWire

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
boolDroppable (Num _) = newConstBool True
boolDroppable (Block cb) = boolNot (cb_rel cb)
boolDroppable (Prod a b) = boolAnd <*> boolDroppable a <*> boolDroppable b 
boolDroppable Unit = 
boolDroppable (Sum _c a b) = boolAnd <*> boolDroppable a <*> boolDroppable b
boolDroppable (Seal _s v) = boolDroppable v

bool

    = Var WireLabel
    | Num NumWire
    | Block CodeBundle
    | Prod Wire Wire
    | Unit
    | Sum BoolWire Wire Wire -- (false+true) order
    | Seal String Wire



newVarWire :: MkGraph Wire
newNumWire :: MkGraph NumWire
newSrcWire :: MkGraph SrcWire
newBoolWire :: MkGraph BoolWire
newCodeBundle :: MkGraph CodeBundle

newConstBool :: Bool -> MkGraph BoolWire
newConstNum :: Rational -> MkGraph NumWire

newVarWire = Var <$> newLabel
newNumWire = newLabel
newSrcWire = newLabel
newBoolWire = newLabel
newCodeBundle = CodeBundle <$> newSrcWire <*> newBoolWire <*> newBoolWire

