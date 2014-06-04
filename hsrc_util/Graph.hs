{-# LANGUAGE ViewPatterns, PatternGuards #-}

-- | This is an experimental module for an ABC intermediate language
-- involving a graph-based representation of the ABC program. The idea
-- is to simplify translation into other languages - such as Haskell -
-- while minimizing intermediate allocations. 
--
-- Currently, the translation is minimal, and uses a boxes-and-wires 
-- metaphor. Sophisticated optimizations on the graph should be feasible
-- (leveraging ABC's causal commutativity and spatial idempotence) but
-- are unlikely to actually be developed (at least not in Haskell).
--
-- Even with this minimalism, it takes a surprising amount of work to
-- perform a decent language translation....
--
module ABC.Graph 
    ( abc2graph
    , Box
    , Wire(..), CodeBundle(..)
    , NumWire, BoolWire, SrcWire
    , textToWire, wireToText
    , Label, NodeLabel, WireLabel, BoxLabel
    , Node(..)
    , nodeInputWires, nodeOutputWires
    , MkGraph, runMkGraph, evalMkGraph
    , GCX, gcx0
    ) where

import Control.Applicative
import Control.Monad 
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer.Strict
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
evalMkGraph = runIdentity . runErrorT . flip evalStateT gcx0

-- | stateful context
data GCX = GCX
    { gcx_gensym  :: !Integer -- to generate unique labels (any type)
    , gcx_nodes   :: [Node] -- primitive operations in graph
    , gcx_elab    :: M.Map WireLabel Wire -- elaborations for wire structure
    , gcx_progs   :: M.Map [Op] BoxLabel -- to help detect cycles
    , gcx_subs    :: M.Map BoxLabel Box -- pre-implemented boxes
    }

-- | a generic wire or bundle of wires
data Wire
    = Var  !WireLabel
    | Num  NumWire
    | Code CodeBundle
    | Prod Wire Wire | Unit
    | Sum  BoolWire Wire Wire  -- (true+false) to avoid 'not' operations @ ?,C
    | Seal String Wire
    -- Note: for sum compositions, outer booleans override inner booleans.


type CWire a = Either WireLabel a

data CodeBundle = CodeBundle
    { cb_src  :: SrcWire
    , cb_copy :: BoolWire -- may copy (not affine)
    , cb_drop :: BoolWire -- may drop (not relevant)
    }

type NumWire  = CWire Rational
type SrcWire  = CWire [Op]
type BoolWire = CWire Bool 

-- | a 'box' is a generic model of a subgraph
type Box = Wire -> MkGraph Wire


-- | a 'node' is a primitive, labeled box, roughly corresponding to
-- ABC operators. A graph essentially consists of a set of nodes.
data Node  -- typically (label inputs outputs)
    -- math nodes
    = N_add (NumWire,NumWire) NumWire -- N(a)*N(b) → N(a+b)
    | N_neg NumWire NumWire -- N(a) → N(-a)
    | N_mul (NumWire,NumWire) NumWire -- N(a)*N(b) → N(a*b)
    | N_inv NumWire NumWire -- N(a) → N(1/a)
    | N_divMod (NumWire,NumWire) (NumWire,NumWire) -- N(dividend)*N(divisor)→N(quotient)*N(remainder)

    -- boolean manipulation nodes 
    | N_boolOr  (WireLabel,WireLabel) WireLabel
    | N_boolAnd (WireLabel,WireLabel) WireLabel
    | N_boolNot WireLabel WireLabel

    -- safety tests
    | N_boolAssert WireLabel ()  -- from K,C  (same as assert true)
    | N_eqvAssert (Wire,Wire) () -- from {&≡}
    | N_eqvNumAssert (NumWire,NumWire) ()
    | N_eqvBoolAssert (WireLabel,WireLabel) ()

    -- substructural type info
    | N_boolCopyable WireLabel WireLabel -- var in, bool out
    | N_boolDroppable WireLabel WireLabel -- var in, bool out

    -- program manipulation nodes
    | N_apply (WireLabel,Wire) WireLabel -- (a→a')*a → a'; also used for 'cond'
    | N_quote Wire SrcWire -- a → (s→(a*s))
    | N_comp (SrcWire,SrcWire) SrcWire -- (a→b)*(b→c) → (a→c)

    -- conditional behavior...
    | N_cond (SrcWire,Wire) Wire
    | N_merge Wire Wire -- (a+a') → a
    | N_gt (NumWire,NumWire) BoolWire -- N(a)*N(b) → (a>b)?
    | N_nonZero NumWire BoolWire

    -- extended node primitives
    | N_tok String   Wire Wire -- ABC's {tokens}; type not locally known
    | N_sub BoxLabel Wire Wire -- call a named local subroutine
    deriving (Show)

instance Show (Label n) where 
    showsPrec _ (Label n) = showChar '_' . shows n

instance Show Wire where
    showsPrec _ (Var lbl) = showChar 'v' . shows lbl
    showsPrec _ (wireToText -> Just txt) = shows (show txt)
    showsPrec _ (Num (Right r)) = 
        let num = numerator r in
        let den = denominator r in
        let sd = if (1==den) then showChar '/' . shows den else id in
        showChar 'N' . showChar '(' . shows num . sd . showChar ')'
    showsPrec _ (Num  (Left lbl)) = showChar 'n' . shows lbl
    showsPrec _ (Code _) = showString "[box]"
    showsPrec _ (Prod a b) = showChar '(' . shows a . showChar '*' . shows b . showChar ')'
    showsPrec _ Unit = showString "unit"
    showsPrec _ (Sum (Right True) a b) =
        showString "(0*" . shows a . showChar '+' . shows b . showChar ')'
    showsPrec _ (Sum (Right False) a b) =
        showChar '(' . shows a . showString "+0*" . shows b . showChar ')'
    showsPrec _ (Sum _ a b) =
        showChar '(' . shows a . showChar '+' . shows b . showChar ')'
    showsPrec _ (Seal tok a) = shows a . showChar '{' . showString tok . showChar '}'

wireToText :: Wire -> Maybe String
wireToText (Sum (Right True) (Prod c cs) _) = (:) <$> wireToChar c <*> wireToText cs
wireToText (Sum (Right False) _ Unit) = pure ""

wireToChar :: Wire -> Maybe Char
wireToChar (Num (Right r)) | inCharRange r = (Just . toEnum . fromInteger . numerator) r
wireToChar _ = Nothing

inCharRange :: Rational -> Bool
inCharRange r = (1 == denominator r) && (0 <= n) && (n <= 0x10ffff) where
    n = numerator r

textToWire :: String -> MkGraph Wire
textToWire (c:cs) =
    let cWire = (Num . Right . fromIntegral . fromEnum) c in
    textToWire cs >>= \ csWire ->
    return (Sum (Right True) (Prod cWire csWire) Unit)
textToWire [] = newSumInR Unit 

-- wrap a value into a sum with a labeled void
newSumInL, newSumInR :: Wire -> MkGraph Wire
newSumInL w = newVar >>= \ v -> return (Sum (Right True)  w v)
newSumInR w = newVar >>= \ v -> return (Sum (Right False) v w)

-- create a fresh variable
newVar :: MkGraph Wire
newVar = Var <$> newLabel

newCWire :: MkGraph (CWire a)
newCWire = Left <$> newLabel

newBool :: MkGraph BoolWire
newNumber :: MkGraph NumWire
newSrc :: MkGraph SrcWire
newCodeBundle :: MkGraph CodeBundle
newBool = newCWire
newNumber = newCWire
newSrc = newCWire
newCodeBundle = CodeBundle <$> newSrc <*> newBool <*> newBool

newLabel :: MkGraph (Label n) 
newLabel = 
    get >>= \ gcx ->
    let lNum = 1 + gcx_gensym gcx in
    put (gcx { gcx_gensym = lNum }) >>
    return (Label lNum)

gcx0 :: GCX
gcx0 = GCX
    { gcx_gensym = 0
    , gcx_nodes = []
    , gcx_progs = M.empty
    , gcx_subs = M.empty
    , gcx_elab = M.empty
    }

-- | compute a graph from ABC code with initial wire
abc2graph :: [Op] -> Box
abc2graph ops w = abc2sub ops >> runABC ops w 

-- generate a subroutine
abc2sub :: [Op] -> MkGraph BoxLabel
abc2sub ops =
    get >>= \ gcx ->
    let progs = gcx_progs gcx in
    case M.lookup ops progs of
        Just box -> return box
        Nothing -> 
            newLabel >>= \ box ->
            let progs' = M.insert ops box progs in
            let subs' = M.insert box (runABC ops) (gcx_subs gcx) in
            let gcx' = gcx { gcx_progs = progs', gcx_subs = subs' } in
            put gcx' >> 
            return box

-- call a named subprogram
callSubroutine :: BoxLabel -> Box
callSubroutine lbl arg = 
    newVar >>= \ result ->
    emitNode (N_sub lbl arg result) >>
    return result

-- add code to the graph
emitNode :: Node -> MkGraph ()
emitNode node = modify $ \ gcx -> gcx { gcx_nodes = (node : gcx_nodes gcx) }

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

opL = onFst opL'
opR = onFst opR'
opW = onFst opW'
opZ = onFst opZ'
opV = onFst opV'
opC = onFst opC'

onFst :: Box -> Box
onFst f ae = asProd ae >>= \ (a,e) -> f a >>= \ a' -> return (Prod a' e)

opL',opR',opW',opZ',opV',opC' :: Box

opL' abc = 
    asSum abc >>= \ (inA, a, bc) ->
    asSum bc >>= \ (inB_unless_inA, b, c) ->
    boolOr inA inB_unless_inA >>= \ inAB ->
    return (Sum inAB (Sum inA a b) c)
opR' abc =
    asSum abc >>= \ (inAB, ab, c) ->
    asSum ab >>= \ (inA_when_inAB, a, b) ->
    boolAnd inAB inA_when_inAB >>= \ inA ->
    return (Sum inA a (Sum inAB b c))
opW' abc =
    asSum abc >>= \ (inA,a,bc) ->
    asSum bc >>= \ (inB_unless_inA,b,c) ->
    boolNot inA >>= \ notInA ->
    boolAnd notInA inB_unless_inA >>= \ inB ->
    return (Sum inB b (Sum inA a c))
opZ' abcd =
    asSum abcd >>= \ (inA,a,bcd) ->
    opW' bcd >>= \ cbd ->
    return (Sum inA a cbd)
opV' a = newVar >>= Sum (Right True) a
opC' av =
    asSum av >>= \ (inA,a,v) ->
    boolAssert inA >> 
    return a

-- access components of a wire; also, infer structure of wires
asProd :: Wire -> MkGraph (Wire,Wire)
asUnit :: Wire -> MkGraph ()
asNumber :: Wire -> MkGraph NumWire
asSum :: Wire -> MkGraph (BoolWire, Wire, Wire)
asCode :: Wire -> MkGraph CodeBundle

asProd (Prod a b) = return (a,b)
asProd (Var w) = autoElab w (Prod <$> newVar <*> newVar) >>= asProd
asProd v = fail $ "expecting product @ " ++ show v

asUnit Unit = return ()
asUnit (Var w) = autoElab w (pure Unit) >>= asUnit
asUnit v = fail $ "expecting unit @ " ++ show v

asNumber (Num a) = return a
asNumber (Var w) = autoElab w (Num <$> newNumber) >>= \ asNum 
asNumber v = fail $ "expecting number @ " ++ show v

asSum (Sum c a b) = return (c,a,b)
asSum (Var w) = autoElab w (Sum <$> newBool <*> newVar <*> newVar) >>= asSum
asSum v = fail $ "expecting sum @ " ++ show v 

asCode (Code cb) = return cb
asCode (Var w) = autoElab w (Code <$> newCodeBundle) >>= asCode where
asCode v = fail $ "expecting code @ " ++ show v

autoElab :: WireLabel -> MkGraph Wire -> MkGraph Wire
autoElab w newW =
    get >>= \ gcx ->
    case M.lookup w (gcx_elab gcx) of
        Just w' -> return w' 
        Nothing -> 
            newW >>= \ w' ->
            let elab' = M.insert w w' (gcx_elab gcx) in
            put (gcx { gcx_elab = elab' }) >>
            return w'

boolOr  :: BoolWire -> BoolWire -> MkGraph BoolWire
boolAnd :: BoolWire -> BoolWire -> MkGraph BoolWire
boolNot :: BoolWire -> MkGraph BoolWire
boolAssert :: BoolWire -> MkGraph ()

boolOr (Right True) _ = return (Right True)
boolOr _ (Right True) = return (Right True)
boolOr (Right False) w = return w
boolOr w (Right False) = return w
boolOr (Left x) (Left y) = 
    newLabel >>= \ z ->
    emitNode (N_boolOr (x,y) z) >>
    return (Left z)

boolAnd (Right True) w = return w
boolAnd w (Right True) = return w
boolAnd (Right False) _ = return (Right False)
boolAnd _ (Right False) = return (Right False)
boolAnd (Left x) (Left y) =
    newLabel >>= \ z ->
    emitNode (N_boolAnd (x,y) z) >> 
    return (Left z)

boolNot (Right b) = return (Right (not b))
boolNot (Left x) =
    newBool >>= \ z ->
    emitNode (N_boolNot x z) >>
    return (Left z)

boolAssert (Right True) = return ()
boolAssert (Right False) = fail "static assertion failure"
boolAssert (Left b) = emitNode (N_boolAssert b)

boolCopyable :: Wire -> MkGraph BoolWire
boolCopyable (Num _) = return (Right True)
boolCopyable (Code cb) = cb_copy cb
boolCopyable (Prod a b) = boolAnd <*> boolCopyable a <*> boolCopyable b
boolCopyable Unit = return (Right True)
boolCopyable (Sum _ a b) = boolAnd <*> boolCopyable a <*> boolCopyable b
boolCopyable (Seal _ v) = boolCopyable v
boolCopyable (Var v) =
    newLabel >>= \ vCopyable ->
    emitNode (N_boolCopyable v vCopyable) >>
    return (Left vCopyable)

boolDroppable :: Wire -> MkGraph BoolWire
boolDroppable (Num _) = return (Right True)
boolDroppable (Code cb) = cb_drop cb
boolDroppable (Prod a b) = boolAnd <*> boolDroppable a <*> boolDroppable b
boolDroppable Unit = return (Right True)
boolDroppable (Sum _ a b) = boolAnd <*> boolDroppable a <*> boolDroppable b
boolDroppable (Seal _ v) = boolDroppable v
boolDroppable (Var v) =
    newLabel >>= \ vDroppable ->
    emitNode (N_boolDroppable v vDroppable) >>
    return (Left vDroppable)

opBL ops = return . Prod . Code . cb where
    cb = CodeBundle { cb_src = Right ops
                    , cb_copy = Right True
                    , cb_drop = Right True }

opTL txt = return . Prod . (textToWire txt)

-- todo: optimize this for common known cases
-- such as &≡ (assert equivalence)
opTok sealer@(':':_) w = return (Seal sealer w)
opTok ('.':s) w = unseal (':':s) w 
opTok tok w =
    newVar >>= \ v ->
    emitNode (N_tok tok w v) >>
    return v

unseal :: String -> Box
unseal s (Seal s' w) | (s == s') = return w
unseal s (Var w) = autoElab w (Seal s <$> newVar) >>= unseal s
unseal s v = fail $ "expecting sealed {:"++s++"} @ " ++ show v

opCopy ae =
    asProd ae >>= \ (a,e) ->
    boolCopyable a >>= \ mayCopy ->
    boolAssert mayCopy >>
    return (Prod a (Prod a e))

opDrop ae =
    asProd ae >>= \ (a,e) ->
    boolDroppable a >>= \ mayDrop ->
    boolAssert mayDrop >>
    return e

opAdd abe =
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNumber a >>= \ na ->
    asNumber b >>= \ nb ->
    numberAdd na nb >>= \ nc ->
    return (Prod (Num nc) e)

numberAdd :: NumWire -> NumWire -> MkGraph NumWire
numberAdd (Right a) (Right b) = return $ Right (a+b)
numberAdd a b =
    newNumber >>= \ c ->
    emitNode (N_add (a,b) c) >>
    return c

opNeg ae =
    asProd ae >>= \ (a,e) ->
    asNumber a >>= \ na ->
    numberNeg na >>= \ nc ->
    return (Prod (Num nc) e)

numberNeg :: NumWire -> MkGraph NumWire
numberNeg (Right a) = return $ Right (negate a)
numberNeg v =
    newNumber >>= \ r ->
    emitNode (N_neg v r) >>
    return r

opMul abe = 
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNumber a >>= \ na ->
    asNumber b >>= \ nb ->
    numberMul na nb >>= \ nc ->
    return (Prod (Num nc) e)

numberMul :: NumWire -> NumWire -> MkGraph NumWire
numberMul (Right a) (Right b) = return $ Right (a*b)
numberMul a b =
    newNumber >>= \ r ->
    emitNode (N_mul (a,b) r) >> 
    return r

opInv ae =
    asProd ae >>= \ (a,e) ->
    asNumber a >>= \ na ->
    numberInv na >>= \ nc ->
    return (Prod (Num nc) e)

numberInv :: NumWire -> MkGraph NumWire
numberInv (Right a) =
    if (0 == a) then fail "invert (/) on zero" else
    return (Right (recip a))
numberInv v =
    newNumber >>= \ r ->
    emitNode (N_inv v r) >>
    return r

opDivMod bae =
    asProd bae >>= \ (b,ae) ->
    asProd ae >>= \ (a,e) ->
    asNumber b >>= \ nb ->
    asNumber a >>= \ na ->
    numberDivMod na nb >>= \ (nq,nr) -> 
    return (Prod (Num nr) (Prod (Num nq) e))

numberDivMod :: NumWire -> NumWire -> MkGraph (NumWire, NumWire)
numberDivMod _ (Right b) | (0 == b) = fail $ "divmod (Q) by 0"
numberDivMod (Right a) (Right b) =
    let (q,r) = divModQ a b in
    return (Right (fromIntegral q), Right r)
numberDivMod a b =
    
    

opApply bxe =
    asProd bxe >>= \ (b,xe) ->
    asProd xe >>= \ (x,e) ->
    asCode b >>= \ cb ->
    callCode b x >>= \ x' ->
    return (Prod x' e)

callCode :: SrcWire -> Box
callCode (Right abc) x = abc2sub abc >>= flip callSubroutine x
callCode (Left w) x =
    newVar >>= \ x' ->
    emitNode (N_apply (w,x) x') >>
    return x'

opCond  bxye =
    asProd bxye >>= \ (b,xye) ->
    asProd xye >>= \ (xy,e) ->
    asSum xy >>= \ (inY,x,y) ->
    asCode b >>= \ cb ->
    boolAssert (cb_drop cb) >>
    


    | N_quote Wire SrcWire -- a → (s→(a*s))
    | N_comp (SrcWire,SrcWire) SrcWire -- (a→b)*(b→c) → (a→c)
        



opApply,opCond,opQuote,opCompose,opAff,opRel :: Box
opDistrib,opFactor,opMerge,opAssert :: Box
opGT :: Box
opIntroNum :: Box
opDigit :: Int -> Box




-- TODO: figure out these sophisticated booleans
--  * assume inner sums don't account for outer conditions
-- processing the graph...
nodeInputWires, nodeOutputWires :: Node -> Set WireLabel
nodeInputWires = execWriter . emitInputWires
nodeOutputWires = execWriter . emitOutputWires

type SetWriter e = Writer (Set e)

emit :: (Ord e) => e -> SetWriter e ()
emit = tell . Set.singleton

emitInputWires, emitOutputWires :: Node -> SetWriter WireLabel ()
emitWire :: Wire -> SetWriter WireLabel ()

emitWire (Var l) = emit l
emitWire (Num n) = emitCWire n
emitWire (Code cb) = 
    emitCWire (cb_src cb) >> 
    emitCWire (cb_copy cb) >> 
    emitCWire (cb_drop cb)
emitWire (Prod a b) = emitWire a >> emitWire b
emitWire (Sum c a b) = emitCWire c >> emitWire a >> emitWire b
emitWire (Seal _ a) = emitWire a

emitCWire :: CWire a -> SetWriter WireLabel ()
emitCWire (Left l) = emit l
emitCWire (Right _) = return ()

emitInputWires (N_add (a,b) _) = emitCWire a >> emitCWire b
emitInputWires (N_neg a _) = emitCWire a
emitInputWires (N_mul (a,b) _) = emitCWire a >> emitCWire b
emitInputWires (N_inv a _) = emitCWire a
emitInputWires (N_divMod (a,b) _) = emitCWire a >> emitCWire b
emitInputWires (N_boolOr (a,b) _) = emit a >> emit b
emitInputWires (N_boolAnd (a,b) _) = emit a >> emit b
emitInputWires (N_boolNot a _) = emit a
emitInputWires (N_boolAssert a _) = emit a
emitInputWires (N_apply (src,arg) _) = emitCWire src >> emitWire arg
emitInputWires (N_quote v _) = emitWire v
emitInputWires (N_comp (a,b) _) = emitCWire a >> emitCWire b
emitInputWires (N_cond (c,src,arg) _) = emitCWire c >> emitCWire src >> emitWire arg
emitInputWires (N_merge (a,b) _) = emitWire a >> emitWire b
emitInputWires (N_gt (a,b) _) = emitCWire a >> emitCWire b
emitInputWires (N_nonZero a _) = emitCWire a
emitInputWires (N_tok _ a _) = emitWire a
emitInputWires (N_sub _ a _) = emitWire a

emitOutputWires (N_add _ c) = emitCWire c
emitOutputWires (N_neg _ a') = emitCWire a'
emitOutputWires (N_mul _ c) = emitCWire c
emitOutputWires (N_inv _ a') = emitCWire a'
emitOutputWires (N_divMod _ (q,r)) = emitCWire q >> emitCWire r
emitOutputWires (N_boolOr _ c) = emit c
emitOutputWires (N_boolAnd _ c) = emit c
emitOutputWires (N_boolNot _ b) = emit b
emitOutputWires (N_boolAssert _ ()) = return ()
emitOutputWires (N_apply _ w) = emitWire w
emitOutputWires (N_quote _ src) = emitCWire src
emitOutputWires (N_comp _ xz) = emitCWire xz
emitOutputWires (N_cond _ w) = emitWire w
emitOutputWires (N_merge _ w) = emitWire w
emitOutputWires (N_gt _ c) = emitCWire c
emitOutputWires (N_nonZero _ b) = emitCWire b
emitOutputWires (N_tok _ _ w) = emitWire w
emitOutputWires (N_sub _ _ w) = emitWire w
