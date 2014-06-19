
-- | Compile a graphical IR to Haskell text to support JIT.
--
-- This is an *imperative* interpretation of the ABC graph.
--
module GraphToHS 
    ( abc2hs
    ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity
import qualified Data.Map as M
import ABC.Imperative.Value
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
    fullTxt = lang.p.mod.p.imps.p.rsc.p.(ss defs).p
    lang = showString "{-# LANGUAGE NoImplicitPrelude #-}"
    mod = showString "module " . showString modName .
          showString " ( resource ) where "
    imps = showString "import ABC.Imperative.Prelude"
    rsc = showString "resource :: Resource" . p .
          showString "resource = Resource " . showString mainFn
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
        Left err -> fail $ err ++ " @ " ++ shows (BL ops)  
        Right g -> buildSubTxt pn g >>= emitCode

emitCode :: HaskellDef -> MkHS ()
emitCode def = modify $ \ cx ->
    let code' = def : cx_code cx in
    cx { cx_code = code' }

buildSubTxt :: ProgName -> (WireLabel,[Node],Wire) -> MkHS HaskellDef
buildSubTxt pn (w0,ns,wf) =
    mkSubBody wf ns >>= \ bodyTxt ->
    return (progText pn w0 bodyTxt)

progText :: ProgName -> WireLabel -> String -> HaskellDef
progText pn w0 body = (hdr.p.onMatch.p)"" where
    hdr = showString pn . showString " :: (Runtime cx) => Prog cx"
    onMatch = showString pn . showChar ' ' . shows w0 . 
              showString " = \n" . showString (indent "  " body)
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
