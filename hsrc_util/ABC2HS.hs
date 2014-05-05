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
            implProg pN ops 

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
prog2hs cx = 





    

implProg :: 
    
    
    gets pcx_progs >>= \ lProgs ->
            let pName = ('p' : show (L.length lProgs)) in
            installProg pName ops >>
            return pName

            
            
            
            
    




{-
    p = showChar '\n'
    slns f (x:xs) = f x . p . showAll f xs
    slns _ [] = id
    showProgs = slns showProg (pcx_hsProgs cx)
    showTexts = slns showText (pcx_hsTexts cx)
    showBlocks = slns showBlock (pcx_hsBlocks cx)
    showProg (pN,code) = 
        showString pN . showString " :: (Runtime m) => Prog m" .
        
        showString " = "
-}
    



defProg :: [Op] -> MkProg Name
defProg ops =
    



-- create one big expression of type:
--   (Runtime m) => V m -> m (V m)
-- using 'let' expressions as desired.
--
data ProgCX = ProgCX
    { pcx_progs :: M.Map [Op] Name      -- cycle detection!
    , pcx_defs  :: M.Map Name [PCase]
    , pcx_texts :: M.Map String Name
    }
type PCase = (Pattern,HsCode)

pcx0 :: ProgCX
pcx0 = ProgCX 
    { pcx_progs   = []
    , pcx_defs    = M.empty
    }






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

     
indent,indent' :: String -> String -> String
indent ws ss = ws ++ indent' ws ss
indent' ws ('\n':ss) = '\n' : indent ws ss
indent' ws (c:ss) = c : indent' ws ss
indent' _ [] = ""
