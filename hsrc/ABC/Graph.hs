{-# LANGUAGE ViewPatterns #-} 

-- | This is an experimental module for an ABC intermediate language
-- based on a graph-based representation of the ABC program. An ABC
-- program is translated into a graph-based code, where the edges 
-- carry ABC values and the vertices represent program operations.
--
-- This is similar to some box-and-wire graphical dataflow languages.
-- Indeed, I'll use the following terminology:
-- 
--    a box is a generic subgraph that takes a bundle of wires as input
--     and generates a similar bundle as output. A primitive box (what
--     becomes a vertex in the graph) is called a node. 
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
module ABC.Graph 
    ( abc2graph
   
    , textToWire, wireToText
 
    , Wire(..), Label, LWire, Bundle
    , Box
    , Node(..),ConnectedNode
    , MkGraph, runMkGraph
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

-- | an abstract label specific to abstract graph g

type MkGraph = StateT GCX (ErrorT String Identity) 
runMkGraph :: GCX -> MkGraph a -> Either String (a,GCX)
runMkGraph gcx op = runIdentity $ runErrorT $ runStateT op gcx

-- | stateful context
data GCX = GCX
    { gcx_gensym :: !Integer -- next label
    , gcx_elab   :: M.Map Integer Wire
    , gcx_graph  :: [CNode]
    }
freshGraphContext :: GCX
freshGraphContext = GCX 0 M.empty []

-- | a generic wire or bundle of wires
data Wire
    = Var  !Label
    | Num  !NumWire
    | Code !CodeBundle
    | Pair !Wire !Wire | Unit
    | Sum  !LWire !LWire
    | Seal String !Wire

data CWire a = C !a | V !Label

data CodeBundle = CodeBundle
    { cb_src :: SrcWire
    , cb_aff :: BoolWire
    , cb_rel :: BoolWire
    , cb_box :: BoxWire
    }

type Label    = Integer
type LWire    = (Bool,Wire)
type Bundle   = Wire
type NumWire  = CWire Rational
type SrcWire  = CWire [Op]
type BoxWire  = CWire Box
type BoolWire = CWire Bool 

-- | a 'box' is a generic model of a subgraph
type Box = Bundle -> MkGraph Bundle

-- | a 'node' is a primitive, labeled box. The wires are provided in
-- context - e.g. `(Node,(Wire,Wire))` - so really the node is just
-- the label. The ABC tokens, e.g. {xyzzy}, essentially extend the
-- set of primitive nodes.
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
data Node
    = N_copy -- a → (a*a) 
    | N_drop -- a → 1
    | N_add -- N(a)*N(b) → N(a+b)
    | N_neg -- N(a) → N(-a)
    | N_mul -- N(a)*N(b) → N(a*b)
    | N_inv -- N(a) → N(1/a)
    | N_divMod -- N(dividend)*N(divisor)→N(quotient)*N(remainder)
    | N_apply -- (a→a')*a → a'; also used for 'cond'
    | N_quote -- a → (s→(a*s))
    | N_comp -- (a→b)*(b→c) → (a→c)
    | N_aff -- (a→b) → (a→b)' ; mark affine (no copy)
    | N_rel -- (a→b) → (a→b)' ; mark relevant (no drop)
    | N_merge -- (a+a') → a
    | N_assert -- (a+b) → b
    | N_gt -- N(a)*N(b) → (N(b)*N(a))+(N(a)*N(b))  
    | N_tok String -- {xyzzy}; any to any
    deriving (Eq, Ord)

instance Show Node where
    showsPrec _ N_copy = showString "copy"
    showsPrec _ N_drop = showString "drop"
    showsPrec _ N_add = showString "a+b"
    showsPrec _ N_neg = showString "(-a)"
    showsPrec _ N_mul = showString "a*b"
    showsPrec _ N_inv = showString "1/a"
    showsPrec _ N_divMod = showString "divMod"
    showsPrec _ N_apply = showString "apply"
    showsPrec _ N_quote = showString "quote"
    showsPrec _ N_comp = showString "comp"
    showsPrec _ N_aff = showString "affine"
    showsPrec _ N_rel = showString "relevant"
    showsPrec _ N_merge = showString "merge"
    showsPrec _ N_assert = showString "assert"
    showsPrec _ N_gt = showString "a>b"
    showsPrec _ (N_tok s) = showChar '{' . showString s . showChar '}'

-- | a node instance in a graph, with (input,output) wires
data CNode = CN !Wire !Node !Wire
    deriving (Eq, Ord)

instance Show CNode where
    showsPrec _ (CN input node output) = 
        shows input . showString "--" . 
        shows node . showString "->" . 
        shows output

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
wireToText (Sum (True, Prod (w2c -> Just c) cs) (False,_)) = (c:)<$>wireToText cs
wireToText (Sum (False,_) (True,Unit)) = pure ""
w2c :: Wire -> Maybe Char
w2c (Num (C r)) = 
    let n = numerator r in
    let ok = (1 == denominator r) && (0 <= n) && (n <= 0x10ffff) in
    if ok then Just (toEnum (fromInteger n)) else Nothing
w2c _ = Nothing

textToWire :: String -> MkGraph Wire
textToWire (c:cs) =
    newSumInL (Num (C (charToRational c))) >>= \ w1 ->
    textToWire cs >>= \ w2 ->
    return (Pair w1 w2)
textToWire [] = newSumInR Unit 

newSumInL, newSumInR :: Wire -> MkGraph Wire
newSumInL w = newVar >>= \ v -> return (Sum (True,w) (False,v))
newSumInR w = newVar >>= \ v -> return (Sum (False,v) (True,w))

newVar :: MkGraph Wire
newVar = newLabel >>= return . Var

newLabel :: MkGraph Label
newLabel = 
    get >>= \ gcx -> 
    let lbl = 1 + gcx_gensym gcx in
    let gcx' = gcx { gcx_gensym = lbl } in
    put gcx' >> return lbl








