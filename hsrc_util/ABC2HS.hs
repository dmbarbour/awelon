-- a mediocre, but not entirely naive, translation 
-- from Awelon bytecode into Haskell code.
--
-- current design challenge: 
--
--   dealing with 'sum' types...
--   1) without bloating code too much
--   2) without losing partial evaluations
--
-- It might require some sort of opportunistic merge of cases
-- and/or of subprograms (sequences that are similar or identical
-- across use-cases. I wonder also if I can leverage some sort of
-- choice conservation concept, e.g. identify subprograms that do
-- not change the number of choices (sums) from input to output.
--
-- I think that handling up to some small, fixed maximum number of
-- choices would likely be sufficient for performance. If I cover
-- three choices (eight paths) in a single subprogram, that could
-- address nearly all 'inner loops'. I guess a second consideration
-- would be how 'long' I hold these choices (how many intermediate
-- operations). 
--
-- OTOH, perhaps it would be better to start by accepting some code
-- bloat, then later find ways to reduce it by selecting subprograms?
-- This would result in more 'specialized' code to start. 
--
-- Another potential approach is to build a dataflow graph, then 
-- build the Haskell program from the graph. This extra layer of
-- indirection might simplify the translation.
--
-- But let's try a relatively simple design to start:
--
--   1) break a program into sequences of prodOps vs. sumOps
--   2) process sumOps using naive model
--   3) process prodOps using simple combination
--
-- This approach leaves a lot to be desired, e.g. it cannot fully
-- optimize a fixpoint process. But it should simplify a lot of
-- "straight-line" code.
--
-- Other thoughts:
--
-- The output of an invocation cannot be locally guaranteed unless
-- I use a known translation. The output of an invocation probably
-- should be mapped to a special no-expansion variable? Alternatively,
-- accept the potential type errors at runtime and separate the whole
-- type-checking issue...
--
module ABC2HS (abc2hs) where

import Control.Applicative
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
type MkProg a     = StateT ProgCX (ErrorT ErrorMsg Identity) a
type MkProgC a    = StateT ProgC (ListT MkProg) a

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
    -- gcContext [p0] >>       -- garbage collect subprograms
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
    runProgC pc0 (defProgC ops (Var 0)) >>= \ lResults ->
    if null lResults then fail $ "BADLY TYPED: " ++ show ops else
    addProgDef (mkProgDef pN lResults) >>
    return pN

runProgC :: ProgC -> MkProgC a -> MkProg [(a,ProgC)]
runProgC pc0 op = runListT (runStateT op pc0)

-- a program definition currently consists of:
--  (1) a name for this program
--  (2) ProgC - internal operations (invoke, let)
--  (3) the final return expression
-- There may be multiple cases in the end.
data ProgDef = ProgDef !Name [(ProgC,Expr)]

mkProgDef :: Name -> [(Expr,ProgC)] -> ProgDef
mkProgDef = ProgDef

defProgC :: [Op] -> Expr -> MkProgC Expr
defProgC = error "todo! defProgC"

data ProgC = ProgC

data Expr
    = Var {-# UNPACK #-} !Sym

type Sym = Integer




{-


{-    
    showDef (nm,impl) =
        showString nm . showString " :: (Runtime m) => Prog m" . p .
        showString impl
        showl (showCase nm) cases . 
        unexpected nm (fmap pca_ptrn cases) . p . p
    showCase nm pca =
        showString nm . showChar ' ' . shows (pca_ptrn pca) .
        showString " = " . shows (pca_prog pca) . p
    unexpected nm ptrns =
        -- attempt to provide decent debugging info
        if coversAll ptrns then id else
        showString nm . showString " other = fail $ " .
        showString "( shows other .\
                    \ showString \" ∉ \" .\
                    \ showString " . shows (show ptrns) . 
        showString ") []"

-}

-- a subprogram, e.g. a case for a given function
data ProgCase = ProgCase
    { pca_ptrns  :: M.Map Symbol Ptrn
    , pca_prog   :: [ProgStep] 
    , pca_gensym :: !Symbol
    }
type Symbol = Integer




-- okay... 




-- define a program, or use an existing definition if
-- it was already installed. 
defProg :: [Op] -> MkProg Name
defProg ops = case splitSumProd ops of
    [Left sumOps] -> defSumProg sumOps
    [Right prodOps] -> defProdProg prodOps
    [] -> return "return"
    mixedOps -> 
        nameProg ops >>= \ (bExists, pN) ->
        if bExists then return pN else
        mapM (either defSumProg defProdProg) mixedOps >>= \ subs ->
        addCase pN (joinSubs subs)

addCase :: Name -> PCase -> MkProg ()
addCase pN pca = modify $ \ 



joinSubs :: [Name] -> PCase
joinSubs (nm0:nms) = nm0 ++ show (Var 0) . joinSubs' nms
joinSubs [] = "return " ++ show (Va
joinSubs (nm1:nms@(_:_)) = nm1 ++ ">=>" ++ joinSubs nms
joinSubs (nm:[]) = nm
joinSubs [] = "return"

-- find the name for a prog; return also whether it already existed
nameProg :: [Op] -> MkProg (Bool,Name)
nameProg ops = 
    get >>= \ cx ->
    let progs = pcx_progs cx in
    case M.lookup ops progs of
        Just pN -> return (True,pN)
        Nothing ->
            let pN = 'p' : show (M.size progs) in
            let progs' = M.insert ops pN progs in
            let cx' = cx { pcx_progs = progs' } in
            put cx' >> return (False,pN)

implProg :: Name -> String -> MkProg ()
implProg pN hsCode = modify $ \ cx ->
    let defs' = M.insert pN hsCode (pcx_defs cx) in
    cx { pcx_defs = defs' }


-- add a program to the context, return the new name and whether
-- it already exists. Does not implement the program.
addProg :: [Op] -> MkProg (Bool,Name)
addProg ops = 
            
genProg ops

-- define a text object, or reuse an existing one

genProg :: Name -> [Op] -> MkProg ()
genProg pN ops =
    mapM (either prodSub sumProg) (splitOps ops) >>= \ subs ->
    
    let subs = splitOps ops in

genProg pN ops =
    let 



        
-- test whether a set of patterns covers all cases.
-- for now, this is sufficient for the patterns actually
-- generated (but would be incomplete for general use). 
coversAll :: [Ptrn] -> Bool
coversAll [Var _] = True
coversAll _ = False

-- create one big expression of type:
--   (Runtime m) => V m -> m (V m)
-- using 'let' expressions as desired.
--

pca0 :: ProgCase
pca0 = ProgCase 
    { pca_ptrns = M.empty
    , pca_prog  = []
    , pca_gensym = 0
    }










data Ptrn
    = Var Integer -- variable
    | Num Integer
    | Prod Ptrn Ptrn | Unit -- products
    | SumL Ptrn | SumR Ptrn | Void
    | Sealed Token Ptrn
    deriving (Eq,Ord)






-- let's try a "make it work, quick!" approach for now
-- we'll make it 'right' later. 
--
-- Consider the following desing 
--
--   (a) duplication of non-variable patterns requ
data Expr
    = Var String
    | Prod Expr Expr | Unit
    | Sum  Expr Expr | Void
    | Merged Expr Expr
    | Sealed String Expr




-- DESIGN:
--
-- We start by assuming the input is a simple variable "v0". 
--
--   to unify two patterns, possibly generating new variables
--   to create an intermediate variable (let..in)
--   to invoke an object...
--
-- 
--   
--  
--   to store an intermediate v0
--   
--
-- Conceptually, the initial input to the program is a *pattern*.
-- We build up this input pattern from step to step, while generating
-- an output pattern. The idea is to eventually generate code of the
-- form:
--
--   prog :: (Runtime m) => Prog m
--   prog (P (P (N a) (P (N b) c)) = return (P (N (a+b)) c)
--   prog v = error $ code ++ " @ " ++ show v
--
-- ... and do this while ensuring some minimal set of computations.
--
-- Potentially, we could also have pure functions of the form:
--
--   fn1 :: V m -> Either String (V m)
--   fn1 (P (P (N a) (P (N b) c)) = Right (P (N (a+b)) c)
--   fn1 v = Left (code ++ " @ " ++ show v)
--
-- Then use these for the pure parts, without involving the toplevel
-- monad before necessary.



-}


