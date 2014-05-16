
-- | This is an experimental module for an ABC intermediate language
-- based on a graph-based representation of the ABC program. An ABC
-- program is translated into a graph-based code, where the edges 
-- carry ABC values and the vertices represent program operations.
--
-- This is similar to some box-and-wire graphical dataflow languages.
-- Indeed, I'll use the following terminology:
-- 
--    a box is a generic subgraph that takes a bundle of wires as input
--     and generates a similar bundle as output. A primitive box (which
--     becomes a vertex in the generated dataflow graph) is called a node. 
--
--    a wire is a dumb dataflow path for a value. Some wires may carry
--     constant values. A complex structure of wires is sometimes called
--     a bundle. In general, any wire may be a bundle of wires; wire and
--     bundle are not formally distinguished. 
--
-- An ABC subprogram corresponds to a box. We focus on boxes - which 
-- represent subgraphs with clear inputs and outputs - because they are
-- composable. 
--
-- Much dataflow becomes implicit in a box and wire representation. This
-- can make the graph easier to optimize in some ways. The intention for
-- this graph based representation is:
--
--   * partial evaluation and inlining
--   * simplified type checking
--   * simplify some optimizations
--   * compilation for JIT or similar
--
-- ABC does present a challenge with respect to cyclic behavior. 
--
-- A useful observation is that, modulo quotation, there is generally a 
-- small, finite number of blocks that can be generated in a given context
-- by simple composition. If we can statically detect these blocks, we can
-- perhaps create a useful context of named subprograms as a basis for 
-- loops. 
--
-- The challenge, then, is translating these loops back into ABC code, 
-- e.g. by explicitly encoding a fixpoint behaviors.
--
module ABC.Graph 
    ( abc2graph
    , Box
    , Wire(..), Bundle, CodeBundle(..)
    , NumWire, BoolWire, BoxWire, SrcWire, BoxWire
    , textToWire, wireToText
    , NodeLabel, WireLabel, BoxLabel
    , Node(..)
    , nodeInputWires
    , nodeOutputWires
    , MkGraph, runMkGraph, evalMkGraph
    , GCX(..), freshGraphContext
    ) where

import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity

import Data.Ratio
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Set (Set)

import ABC.Operators

newtype Label n = Label Integer deriving (Ord,Eq)
type NodeLabel = Label Node
type WireLabel = Label Wire
type BoxLabel  = Label Box

type MkGraph = StateT GCX (ErrorT String Identity) 
runMkGraph :: GCX -> MkGraph a -> Either String (a,GCX)
evalMkGraph :: MkGraph a -> Either String a

runMkGraph gcx op = runIdentity $ runErrorT $ runStateT op gcx
evalMkGraph = runIdentity . runErrorT . flip evalStateT freshGraphContext

-- | stateful context
data GCX = GCX
    { gcx_gensym  :: !Integer -- next label (of any type)
    , gcx_nodes   :: M.Map NodeLabel Node -- labeled nodes
    , gcx_progs   :: M.Map [Op] BoxLabel
    , gcx_subs    :: M.Map BoxLabel Box   
    , gcx_elab    :: M.Map EdgeLabel Wire -- elaborations for edge labels
    }

-- | a generic wire or bundle of wires
data Wire
    = Var  !Label
    | Num  NumWire
    | Code CodeBundle
    | Pair Wire Wire | Unit
    | Sum  BoolWire Wire Wire
    | Seal String Wire

type CWire a = Either WireLabel a

data CodeBundle = CodeBundle
    { cb_src :: SrcWire
    , cb_aff :: BoolWire
    , cb_rel :: BoolWire
    , cb_box :: BoxWire
    }

type Label    = Integer
type LWire    = (BoolWire,Wire)
type Bundle   = Wire
type NumWire  = CWire Rational
type SrcWire  = CWire [Op]
type BoxWire  = CWire Box
type BoolWire = CWire Bool 

-- | a 'box' is a generic model of a subgraph
type Box = Bundle -> MkGraph Bundle

-- | a 'node' is a primitive, labeled box. So far, this is mostly a
-- subset of ABC. However, these nodes do not include inputs that 
-- are not necessary.
--
-- This is essentially a subset of ABC. However, where ABC operators
-- include the environment (e.g. `+ :: (N(a)*(N(b)*e))→N(a+b)*e`),
-- these operators do not include any arguments they do not need. 
-- Further, some additional nodes support supplementary behaviors or
-- assertions.
--
-- Also, some additional nodes are included for supplementary behavior,
-- such as ass
-- 
data Node  -- typically (label inputs outputs)
    -- math nodes
    = N_add (NumWire,NumWire) NumWire -- N(a)*N(b) → N(a+b)
    | N_neg NumWire NumWire -- N(a) → N(-a)
    | N_mul (NumWire,NumWire) NumWire -- N(a)*N(b) → N(a*b)
    | N_inv NumWire NumWire -- N(a) → N(1/a)
    | N_divMod (NumWire,NumWire) (NumWire,NumWire) -- N(dividend)*N(divisor)→N(quotient)*N(remainder)

    -- boolean manipulation nodes 
    | N_boolOr  (BoolWire,BoolWire) BoolWire
    | N_boolAnd (BoolWire,BoolWire) BoolWire
    | N_boolNot BoolWire BoolWire
    | N_boolAssert BoolWire ()

    -- program manipulation nodes
    | N_apply (CodeBundle,Wire) Wire -- (a→a')*a → a'; also used for 'cond'
    | N_quote Wire CodeBundle -- a → (s→(a*s))
    | N_comp (CodeBundle,CodeBundle) CodeBundle -- (a→b)*(b→c) → (a→c)
    -- | N_aff CodeBundle CodeBundle -- (a→b) → (a→b)' ; mark affine (no copy)
    -- | N_rel CodeBundle CodeBundle -- (a→b) → (a→b)' ; mark relevant (no drop)

    -- conditional behavior...
    -- | N_distrib -- (a*(b+c)) → ((a*b)+(a*c))
    -- | N_factor -- ((a*b)+(c*d)) → ((a+c)*(b+d))
    | N_cond (BoolWire,CodeBundle,Wire) Wire
    | N_merge (Wire,Wire) Wire -- (a+a') → a
    -- | N_assert (Wire,Wire) Wire -- (a+b) → b
    | N_gt (NumWire,NumWire) BoolWire -- N(a)*N(b) → (a>b)?
    | N_nonZero NumWire BoolWire


    -- extended node primitives
    | N_tok String   Wire Wire -- ABC's {tokens}; type not locally known
    | N_sub BoxLabel Wire Wire -- call a named local subroutine
    deriving (Eq, Ord)

instance Show (Label n) where 
    showsPrec _ (Label n) = showChar '_' . shows n

instance Show Wire where
    showsPrec _ (Var lbl) = showChar 'v' . shows lbl
    showsPrec _ (wireToText -> Just txt) = shows (show txt)
    showsPrec _ (Num (C r)) = 
        let num = numerator r in
        let den = denominator r in
        let sd = if (1==den) then showChar '/' . shows den else id in
        showChar 'N' . showChar '(' . shows num . sd . showChar ')'
    showsPrec _ (Num (V lbl)) = showChar 'n' . shows lbl
    showsPrec _ (Code (C _)) = showString "[box]"
    showsPrec _ (Code (V lbl)) = showString "[box " . shows lbl . showChar ']'
    showsPrec _ (Pair a b) = showChar '(' . shows a . showChar '*' . shows b . showChar ')'
    showsPrec _ Unit = showString "unit"
    showsPrec _ (Sum (la,a) (lb,b)) = 
        let sZero c = if c then showChar '~' else id in
        showChar '(' . 
        sZero la . shows a . showChar '+' . 
        sZero lb . shows b . showChar ')'
    showsPrec _ (Seal tok a) = shows a . showString tok

wireToText :: Wire -> Maybe String
wireToText (Sum (C True) (Prod c cs) _) = (:) <$> wireToChar c <*> wireToText cs
wireToText (Sum (C False) _ Unit) = pure ""

wireToChar :: Wire -> Maybe Char
wireToChar (Num (C r)) | inCharRange r = (Just . toEnum . fromInteger . numerator) r
wireToChar _ = Nothing

inCharRange :: Rational -> Bool
inCharRange r = (1 == denominator r) && (0 <= n) && (n <= 0x10ffff) where
    n = numerator r

textToWire :: String -> MkGraph Wire
textToWire (c:cs) =
    let cWire = (Num . C . fromIntegral . fromEnum) c in
    textToWire cs >>= \ csWire ->
    return (Sum (C True) (Prod cWire csWire) Unit)
textToWire [] = newSumInR Unit 

-- wrap a value into a sum with a labeled void
newSumInL, newSumInR :: Wire -> MkGraph Wire
newSumInL w = newVar >>= \ v -> return (Sum (C True)  w v)
newSumInR w = newVar >>= \ v -> return (Sum (C False) v w)

-- create a fresh variable
newVar :: MkGraph Wire
newVar = newLabel >>= return . Var

newLabel :: MkGraph (Label n) 
newLabel = 
    get >>= \ gcx ->
    let lNum = 1 + gcx_gensym gcx in
    put (gcx { gcx_gensym = lNum }) >>
    return lNum


freshGraphContext :: GCX
freshGraphContext = GCX
    { gcx_gensym = 0
    , gcx_nodes = M.empty
    , gcx_progs = M.empty
    , gcx_subs = M.empty
    , gcx_elab = M.empty
    }




