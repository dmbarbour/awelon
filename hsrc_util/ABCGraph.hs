
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
    = Elab WireLabel Wire

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
    | BoolCopyable Wire BoolWire
    | BoolDroppable Wire BoolWire

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

runMkGraph :: GCX -> MkGraph a -> Either String (a,GCX)
evalMkGraph :: MkGraph a -> Either String a
gcx0 :: GCX

runMkGraph gcx op = runIdentity $ runErrorT $ runStateT op gcx
evalMkGraph = runIdentity . runErrorT . flip evalStateT gcx0
gcx0 = ([],0)

newLabel :: MkGraph (Label t)
newLabel =
    get >>= \ (nodes,sym) ->
    let sym' = sym + 1 in
    put (nodes,sym') >>
    return $! (Label sym')

emitNode :: Node -> MkGraph ()
emitNode n = modify $ \ (ns,s) -> (n:ns,s)

newVarWire :: MkGraph Wire
newNumWire :: MkGraph NumWire
newSrcWire :: MkGraph SrcWire
newBoolWire :: MkGraph BoolWire

newVarWire = Var <$> newLabel
newNumWire = newLabel
newSrcWire = newLabel
newBoolWire = newLabel

newConstBool :: Bool -> MkGraph BoolWire
newConst
newConstNum :: Rational -> MkGraph NumWire



