-- a mediocre, but not entirely naive, translation 
-- from Awelon bytecode into Haskell code.
module ABC2HS (abc2hs, abc2prog) where

import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.List as L
import ABC.Operators

type ModuleName = String 
type ErrorMsg   = String
type Name       = String
type Token      = String
-- import Control.Applicative

abc2hs :: ModuleName -> [Op] -> Either ErrorMsg String
abc2hs modName ops = hsResource modName <$> abc2prog ops

hsResource :: ModuleName -> (ProgName,String) -> String
hsResource modName (mainProgName,progDefs) = prefix progDefs where
    p = showChar '\n'
    prefix  = showMod.p.showImps.p.showRsc.p
    showMod = 
        showString "module " .
        showString modName .
        showString " ( resource ) where "
    showImps = 
        showString "import ABC.Imperative.Prelude".p
    showRsc =
        showString "resource :: Resource".p.
        showString "resource = Resource ".
        showString mainProgName . p

type MkProg a = StateT ProgCX (ErrorT ErrorMsg Identity) a

runMkProg :: ProgCX -> MkProg a -> (Either ErrorMsg a)
runMkProg cx op = runIdentity $ runErrorT $ evalStateT op cx

-- Generate a set of toplevel Haskell names, along with a name for
-- the main program. A 'program' in this case has type:
--
--    (Runtime m) => V m -> m (V m)
--
-- This is the toplevel command to get started. 
abc2prog :: [Op] -> Either ErrorMsg (Name,String)
abc2prog = runMkProg pcx0 . mkProg

mkProg :: [Op] -> MkProg (Name,String)
mkProg ops =
    defProg ops >>= \ p0 ->
    get >>= \ cx -> 
    return (p0, prog2hs cx)

-- define a program, or use an existing definition if
-- it was already installed. 
defProg :: [Op] -> MkProg Name
defProg ops = 
    gets pcx_progs >>= \ mDefs ->
    case M.lookup ops mDefs of
        Just pN -> return pN
        Nothing -> 
            let pN = ('p' : show (M.size mDefs)) in
            let mDefs' = M.insert ops pN mDefs in
            withState (\s -> s { pcx_progs = mDefs' }) >>
            genProg pN ops 

-- define a text object, or reuse an existing one
defText :: String -> MkProg Name
defText txt =
    gets pcx_texts >>= \ mTxts ->
    case M.lookup txt mTxts of
        Just tN -> return tN
        Nothing ->
            let tN = ('t' : show (M.size mTxts)) in
            let mTxts' = M.insert txt tN mTxts in
            withState (\s -> s { pcx_texts = mTxts' })

prog2hs :: ProgCX -> String
prog2hs cx = showHs [] where
    p = showChar '\n'
    showHs = showDefs.showTexts
    lDefs = M.toList (pcx_defs cx)
    lTexts = M.toList (pcx_texts cx)
    showDefs = showl showDef lDefs
    showTexts = showl showText lTexts
    showl f (x:xs) = f x . showl f xs
    showl [] = id
    showDef (nm,cases) =
        showString nm . showString " :: (Runtime m) => Prog m" . p .
        showl (showCase nm) cases . unexpected nm cases . p.p
    showCase nm (ptrn,prog) =
        showString nm . showChar ' ' . shows ptrn .
        showString " = " . shows prog . p
    unexpected nm cases =
        -- attempt to provide decent debugging info
        if coversAll cases then id else
        showString nm . 
        showString " other = fail $ ( showString \"received \" .\
                                    \ shows other .\
                                    \ showString \"expecting \" .\
                                    \ showString " .
        shows (show (fmap fst cases)) .
        showString ") []
    showText (txt,nm) = 
        showString nm . showString " :: String" . p .
        showString nm . showString " = " . shows txt . p.p
        
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
data ProgCX = ProgCX
    { pcx_progs :: M.Map [Op] Name      -- cycle detection!
    , pcx_defs  :: M.Map Name [MatchCase]
    , pcx_texts :: M.Map String Name
    }
type MatchCase = (Ptrn,Prog)

pcx0 :: ProgCX
pcx0 = ProgCX 
    { pcx_progs = M.empty
    , pcx_defs  = M.empty
    , pcx_texts = M.empty
    }

-- this Prog assumes variables may have been captured
-- e.g. by 
--
-- so it has type `(Runtime m) => m (V m)` with implicit
-- context of a captured value. The concatenative nature
-- of ABC is sacrificed at this point for performance in
-- the target language, Haskell.
data Prog 
    = Call Name Expr       -- subroutines; captured cycles
    | Invoke Token Expr    -- ABC's effects model
    | Bind Prog Name Prog  -- prog1 >>= \ name -> prog2
    | Return Expr          -- return expr
    | Let Name Expr Prog   -- let name = expr in prog

-- a 
data Ptrn


data Ptrn
    = Pv Name -- variable
    | Pp Ptrn Ptrn -- P
    | Pl Ptrn -- L
    | Pr Ptrn -- R
    | Psv String Ptrn -- SV
    | P






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
