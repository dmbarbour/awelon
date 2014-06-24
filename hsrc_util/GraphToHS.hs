{-# LANGUAGE ViewPatterns, PatternGuards #-}
-- | Compile a graphical IR to Haskell text to support JIT.
--
-- This is for an *imperative* interpretation of the ABC graph.
--
module GraphToHS 
    ( abc2hs
    ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity
import Data.Ratio
import qualified Data.Map as M
--import ABC.Imperative.Value
import ABC.Operators
import ABCGraph
import Util (indent)

data CX = CX
    { cx_subs :: M.Map [Op] ProgName -- subroutines...
    , cx_txts :: M.Map String TextName -- texts
    , cx_code :: [HaskellDef] -- toplevel definitions
    }
type ProgName = String
type TextName = String
type ModuleName = String
type ModuleString = String
type HaskellDef = String
type ErrorString = String

type MkHS = StateT CX (ErrorT ErrorString Identity)
evalMkHS :: MkHS a -> Either ErrorString a
evalMkHS = runIdentity . runErrorT . flip evalStateT cx0

cx0 :: CX
cx0 = CX M.empty M.empty []

-- | abc2hs takes a module name and the primary resource
-- it will export both the 'resource' and perhaps the
-- 'source' (an ABC string). 
abc2hs :: ModuleName -> [Op] -> Either ErrorString ModuleString
abc2hs modName ops = evalMkHS $ 
    mkSub ops >>= \ mainFn ->
    gets cx_code >>= \ subDefs ->
    return (moduleText modName mainFn subDefs)

moduleText :: ModuleName -> ProgName -> [HaskellDef] -> ModuleString
moduleText modName mainFn defs = fullTxt "" where
    fullTxt = hdr.p.imps.p.p.rsc.p.(ss defs).p
    -- lang = showString "{-# LANGUAGE NoImplicitPrelude #-}"
    hdr = showString "module " . showString modName . p .
          showString "    ( resource ) where "
    imps = showString "import ABC.Imperative.Prelude"
    rsc = showString "resource :: Resource" . p .
          showString "resource = Resource " . showString mainFn . p
    ss (d:ds) = showString d . p . ss ds
    ss [] = id
    p = showChar '\n'

mkSub :: [Op] -> MkHS ProgName
mkSub ops =
    get >>= \ cx ->
    let m = cx_subs cx in
    case M.lookup ops m of
        Just pn -> return pn
        Nothing ->
            let pn = 'p' : show (M.size m) in
            let m' = M.insert ops pn m in
            let cx' = cx { cx_subs = m' } in
            put cx' >> 
            defSub pn ops >>
            return pn

defSub :: ProgName -> [Op] -> MkHS ()
defSub pn ops = 
    case abc2graph ops of
        Left err -> fail $ err ++ " @ " ++ show (BL ops)  
        Right g -> buildSubTxt pn g >>= emitCode

emitCode :: HaskellDef -> MkHS ()
emitCode def = modify $ \ cx ->
    let code' = def : cx_code cx in
    cx { cx_code = code' }

buildSubTxt :: ProgName -> (WireLabel,[Node],Wire) -> MkHS HaskellDef
buildSubTxt pn (w0,ns,wf) =
    mkHS wf ns >>= \ bodyTxt ->
    return (progText pn w0 bodyTxt)

-- full haskell text from appropriately sorted graph
mkHS :: Wire -> [Node] -> MkHS HaskellDef
mkHS (Var s) (n0@(Apply (src,arg) s') : nodes) | (s == s') = -- tail-call
    if not (null nodes) then mkHS (Var s) (nodes ++ [n0]) else -- move tail call to end
    mkWirePattern arg >>= \ ap ->
    case src of
        Dyn lbl -> return $ "b_prog " ++ show lbl ++ " " ++ ap
        Stat ops -> mkSub ops >>= \ pn -> return $ pn ++ " " ++ ap
mkHS w [] = 
    mkWirePattern w >>= \ wp ->
    return $ "return " ++ wp
mkHS w (n:ns) = 
    mkNHS n >>= \ op ->
    mkHS w ns >>= \ ops ->
    return (op ++ ('\n' : ops))

-- capture a wire into a value (for output)
mkWirePattern :: Wire -> MkHS HaskellDef
mkWirePattern (wireToText -> Just txt) =
    addText txt >>= \ tn -> -- raw text as a value
    return $ "(textToVal " ++ tn ++ ")"
mkWirePattern (Var lbl) = return $ show lbl
mkWirePattern (Num n) = 
    mkNumPattern n >>= \ pn ->
    return $ "(N " ++ pn ++ ")"
mkWirePattern (ABCGraph.Block cb) = 
    mkBlockPattern cb >>= \ b ->
    return $ "(B " ++ b ++ ")"
mkWirePattern (Prod a b) =
    mkWirePattern a >>= \ pa ->
    mkWirePattern b >>= \ pb ->
    return $ "(P " ++ pa ++ " " ++ pb ++ ")"
mkWirePattern Unit = return "U"
mkWirePattern (Sum (Stat False) a _void) =
    mkWirePattern a >>= \ pa ->
    return $ "(L " ++ pa ++ ")"
mkWirePattern (Sum (Stat True) _void b) =
    mkWirePattern b >>= \ pb ->
    return $ "(R " ++ pb ++ ")"
mkWirePattern (Sum (Dyn c) a b) =
    mkWirePattern a >>= \ pa ->
    mkWirePattern b >>= \ pb ->
    return $ "(sum3toV " ++ show c ++ " " ++ pa ++ " " ++ pb ++ ")"
mkWirePattern (Seal s v) =
    addText s >>= \ tn ->
    mkWirePattern v >>= \ pv -> 
    return $ "(S " ++ tn ++ " " ++ pv ++ ")"
    
mkNumPattern :: NumWire -> MkHS HaskellDef
mkNumPattern (Stat n) = return (showR n)
mkNumPattern (Dyn lbl) = return (show lbl)

showR :: Rational -> String
showR r | (1 == denominator r) = showI (numerator r)
showR r =  "(" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ ")"

showI :: Integer -> String 
showI n | (n >= 0) = show n
showI n = "(" ++ show n ++ ")"

mkBlockPattern :: CodeBundle -> MkHS HaskellDef
mkBlockPattern cb =
    mkBoolPattern (cb_rel cb) >>= \ rel ->
    mkBoolPattern (cb_aff cb) >>= \ aff -> 
    mkCodePattern (cb_src cb) >>= \ b ->
    return $ "(" ++ b ++ " { b_rel = " ++ rel ++ ", b_aff = " ++ aff ++ "})"

mkBoolPattern :: BoolWire -> MkHS HaskellDef
mkBoolPattern (Stat b) = return $ show b
mkBoolPattern (Dyn lbl) = return $ show lbl

-- obtain a Block value
mkCodePattern :: SrcWire -> MkHS HaskellDef
mkCodePattern (Stat ops) =
    mkSub ops >>= \ pn ->
    addText (show ops) >>= \ tn ->
    return $ "(blockVal " ++ tn ++ " " ++ pn ++ ")"
mkCodePattern (Dyn lbl) = return $ show lbl

-- obtain a `V cx -> cx (V cx)` program
mkProgPattern :: SrcWire -> MkHS HaskellDef
mkProgPattern (Stat ops) = mkSub ops
mkProgPattern (Dyn lbl) = return $ "(b_prog " ++ show lbl ++ ")"

-- Translate nodes to fragments of monadic Haskell code.
-- This is monadic mostly to support `SrcConst`.
mkNHS :: Node -> MkHS HaskellDef
mkNHS (Void () w) = return $ "let " ++ show w ++ " = voidVal "
mkNHS (ElabSum w (c,a,b)) = return $ 
    "let (" ++ show c ++ "," ++ show a ++ "," ++ show b ++ ") = exSum3 " ++ show w
mkNHS (ElabProd w (a,b)) = return $ 
    "let (" ++ show a ++ "," ++ show b ++ ") = exProd " ++ show w
mkNHS (ElabNum w n) = return $ 
    "let " ++ show n ++ " = exNum " ++ show w
mkNHS (ElabCode w (b,k,f)) = return $
    "let (" ++ show b ++ "," ++ show k ++ "," ++ show f ++ ") = exBKF " ++ show w
mkNHS (ElabUnit w ()) = 
    return $ "-- note: " ++ show w ++ " should be unit"
mkNHS (ElabSeal s w v) = 
    addText s >>= \ tn ->
    return $  "let " ++ show v ++ " = exSeal " ++ tn ++ " " ++ show w
    -- show v ++ " <- exSeal " ++ show s ++ " " ++ show w
mkNHS (Add (a,b) c) = 
    mkNumPattern a >>= \ pa ->
    mkNumPattern b >>= \ pb ->
    return $ "let " ++ show c ++ " = " ++ pa ++ " + " ++ pb
mkNHS (Neg a b) = return $ "let " ++ show b ++ " = negate " ++ show a
mkNHS (Mul (a,b) c) = 
    mkNumPattern a >>= \ pa ->
    mkNumPattern b >>= \ pb ->
    return $ "let " ++ show c ++ " = " ++ pa ++ " * " ++ pb
mkNHS (Inv a b) = return $ "let " ++ show b ++ " = recip " ++ show a
mkNHS (DivMod (a,b) (q,r)) = 
    mkNumPattern a >>= \ pa ->
    mkNumPattern b >>= \ pb ->
    return $ "let (" ++ show q ++ "," ++ show r ++ ") = abcDivMod " ++ pa ++ " " ++ pb 
mkNHS (IsNonZero n b) = return $ "let " ++ show b ++ " = (0 /= " ++ show n ++ ")"
mkNHS (GreaterThan (x,y) b) = 
    mkNumPattern x >>= \ px ->
    mkNumPattern y >>= \ py ->
    return $ "let " ++ show b ++ " = " ++ px ++ " > " ++ py
mkNHS (BoolOr (a,b) c) = return $ "let " ++ show c ++ " = (" ++ show a ++ " || " ++ show b ++ ")"
mkNHS (BoolAnd (a,b) c) = return $ "let " ++ show c ++ " = (" ++ show a ++ " && " ++ show b ++ ")"
mkNHS (BoolNot a b) = return $ "let " ++ show b ++ " = not " ++ show a
mkNHS (BoolCopyable a b) = return $ "let " ++ show b ++ " = copyable " ++ show a
mkNHS (BoolDroppable a b) = return $ "let " ++ show b ++ " = droppable " ++ show a
mkNHS (BoolAssert msg b ()) =
    addText msg >>= \ tn ->
    return $ "rtAssert " ++ tn ++ " " ++ show b 
mkNHS (Quote w b) = 
    mkWirePattern w >>= \ pw ->
    return $ "let " ++ show b ++ " = quoteVal " ++ pw
mkNHS (Compose (xy,yz) xz) =
    mkCodePattern xy >>= \ pxy ->
    mkCodePattern yz >>= \ pyz ->
    return $ "let " ++ show xz ++ " = bcomp " ++ pxy ++ " " ++ pyz 
mkNHS (Apply (src, arg) result) =
    mkProgPattern src >>= \ prog ->
    mkWirePattern arg >>= \ parg ->
    return $ show result ++ " <- " ++ prog ++ " " ++ parg
mkNHS (CondAp (c,src,arg) result) = 
    mkProgPattern src >>= \ prog ->
    mkWirePattern arg >>= \ parg ->
    return $ show result ++ " <- condAp " ++ show c ++ " " ++ prog ++ " " ++ parg
mkNHS (Merge (c,a,b) r) = 
    mkWirePattern a >>= \ pa ->
    mkWirePattern b >>= \ pb ->
    return $ "let " ++ show r ++ " = mergeSum3 " ++ show c ++ " " ++ pa ++ " " ++ pb
mkNHS (Invoke s w r) = 
    addText s >>= \ tn ->
    mkWirePattern w >>= \ wp ->
    return $ show r ++ " <- invoke " ++ tn ++ " " ++ wp


progText :: ProgName -> WireLabel -> String -> HaskellDef
progText pn w0 body = (hdr.p.onMatch.p)"" where
    hdr = showString pn . showString " :: (Runtime cx) => Prog cx"
    onMatch = showString pn . showChar ' ' . shows w0 . 
              showString " = do \n" . showString (indent "  " body)
    p = showChar '\n'

addText :: String -> MkHS TextName 
addText s = 
    get >>= \ cx ->
    let m = cx_txts cx in
    case M.lookup s m of
        Just tn -> return tn
        Nothing ->
            let tn = 't' : show (M.size m) in
            let m' = M.insert s tn m in
            let cx' = cx { cx_txts = m' } in
            put cx' >>
            defText tn s >>
            return tn

defText :: TextName -> String -> MkHS ()
defText tn s = emitCode (showProg "") where
    showProg = hdr.p.body.p
    hdr = showString tn . showString " :: String "
    body = showString tn . showString " = " . shows s
    p = showChar '\n'
