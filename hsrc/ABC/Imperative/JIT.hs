
-- | JIT support for functional & imperative interpretation of ABC.
--
-- The basic concept is to take an ABC subprogram, compile it to 
-- Haskell code, compile it further to Machine code using GHC, then
-- dynamically load the resulting module. A similar approach might
-- later use LLVM or ATS or C as target languages.
--
-- This module does not provide an ABC-to-Haskell compiler, but does
-- handle the other aspects. Haskell modules are named for the ABC
-- resource token, and may acyclically rely upon other resources.
--
--
--
module ABC.Imperative.JIT 
    ( abcToModuleName, tokenToModuleName
    ) where

import qualified Data.List as L
--import qualified Data.Text as T
--import qualified Filesystem as FS
--import qualified Filesystem.Path.CurrentOS as FS
--import qualified Control.Exception as Err

import ABC.Operators
import ABC.Resource

-- | compute a cryptographically unique module name for ABC code
-- 
-- This uses the same token generated for separate compilation.
abcToModuleName :: [Op] -> String
abcToModuleName = tokenToModuleName . abcResourceToken

-- | compute a module name from the hash of the ABC code
-- (lossy, but retains 320 bits from 384 for uniqueness)
tokenToModuleName :: String -> String
tokenToModuleName ('#':s) = 
    let (a,a') = L.splitAt 2 (fmap modChar s) in
    let (b,b') = L.splitAt 2  a' in
    let (c,r ) = L.splitAt 28 b' in
    let sa = showChar 'A' . showString a in
    let sb = showChar 'B' . showString b in
    let sc = showChar 'C' . showString c in
    let dot = showChar '.' in
    (sa.dot.sb.dot.sc.dot)('R':r)
tokenToModuleName s = "Ext." ++ tokenToModuleName ('#':s)

-- squeeze a hash into a module name
modChar :: Char -> Char
modChar c | ('A' <= c) && (c <= 'Z') = toEnum (32 + fromEnum c)
modChar c | ('a' <= c) && (c <= 'z') = c
modChar c | ('0' <= c) && (c <= '9') = c
modChar _ = '_' -- c should only be '_' or '-' if input is from base64

{-

tryDef :: a -> IO a -> IO a
tryDef def op = op `Err.catch` onExceptionReturn def where

onExceptionReturn :: a -> Err.SomeException -> IO a
onExceptionReturn v _ = return v
-}

{-

-- 'System.Plugins.load' seems to not be MT-safe 
-- (i.e. fails with +RTS -N3 and asynch {&compile})
-- so I'm just going to force it to be single-threaded.
loadMutex :: MVar ThreadId
loadMutex = unsafePerformIO newEmptyMVar
{-# NOINLINE loadMutex #-}

withLoadMutex :: IO a -> IO a
withLoadMutex action =
    myThreadId >>= putMVar loadMutex >>
    (action `Err.finally` takeMVar loadMutex)

-- | Use Haskell's plugins module to just-in-time compile code.
--
-- The given ops should be pre-optimized and pre-simplified before
-- reaching this final JIT compilation step. 
--
-- This module will load an object haskell or object file if it 
-- already exists at the target location, essentially caching the
-- compiled form. However, this does require a little trust that
-- nobody is doing silly things like tweaking the generated files.
--
-- Since plugins work through the filesystem, this first creates
-- a resource in the filesystem (under the AO_TEMP directory), then 
-- builds it externally before loading the result. Resources are
-- uniquely named based on a secure hash of the ABC that generates
-- them.
--
-- It isn't entirely clear to me how GC of loaded plugins works.
-- For now, I'll just hope it works. 
--
-- This may fail if any operation cannot be completed.
--
-- TODO: consider using `RuntimeJIT` instead of `Runtime` to
-- enable some flexible compile-time interpretation of tokens.
--
abc_jit :: (Runtime m) => [Op] -> IO (Prog m)
abc_jit ops =
    let rn = opsToModName ops in
    let (dn,pre) = pathAndPrefix rn in
    getJitTmpDir >>= \ jitDir ->
    let rscDir  = jitDir FS.</> dn in
    let prefix  = rscDir FS.</> pre in
    let abcFile = prefix FS.<.> T.pack "abc" in -- awelon bytecode
    let hsFile  = prefix FS.<.> T.pack "hs" in -- haskell code
    let hiFile  = prefix FS.<.> T.pack "hi" in -- haskell interface
    let oFile   = prefix FS.<.> T.pack "o" in  -- system object code 
    let createTheResource =
            FS.createTree rscDir >>
            FS.writeTextFile abcFile (T.pack (show ops)) >>
            either fail (FS.writeTextFile hsFile . T.pack) (abc2hs rn ops)
    in
    let makeArgs = 
            ["-outputdir",FS.encodeString jitDir
            ,"-i","-i"++FS.encodeString jitDir
            ,"-Wall","-Werror"
            ,"-fno-warn-unused-imports"
            ,"-fno-warn-missing-signatures"
            ,"-fno-warn-unused-binds"
            ,"-fno-warn-unused-matches"
            ,"-package","ao"
            ,"-O2"
            ]
    in
    let makeTheObjectFile =
            Sys.make (FS.encodeString hsFile) makeArgs >>= \ makeStatus ->
            case makeStatus of
                Sys.MakeFailure errs -> fail $ "MAKE ERROR: " ++ (L.unlines errs)
                Sys.MakeSuccess _ objFile ->
                    let everythingIsAwesome = (oFile == FS.decodeString objFile) in
                    unless everythingIsAwesome $ fail $ 
                        "GHC not generating the anticipated '.o' file!"
    in
    FS.isFile hsFile >>= \ hsExists ->
    FS.isFile hiFile >>= \ hiExists ->
    FS.isFile oFile  >>= \ objExists ->
    unless hsExists createTheResource >>
    unless (hiExists && objExists) makeTheObjectFile >>
    let loadRsc = Sys.load (FS.encodeString oFile) [] [] "resource" in 
    withLoadMutex loadRsc >>= \ loadStatus ->
    case loadStatus of
        Sys.LoadFailure errs -> fail $ "LOAD ERROR: " ++ (L.unlines errs)
        Sys.LoadSuccess _ rsc -> return (asProg rsc)

-- | create a unique module name for a given ABC program
--
-- currently, generates a module name of the form:
--
--    A01.B23.C456789abcdefghijklmnopqrstuvwxyz....
--
-- This is cryptographically unique (300 bits of SHA3-384) using
-- base32 to encode the hash. It is named such that we potentially
-- can import and reuse subprograms. The name also reduces fan-out
-- by a factor of about a million, thus reducing filesystem burden.
--
opsToModName :: [Op] -> ModuleName
opsToModName ops = 
    let un = uniqueStr ops in
    let (an,un') = L.splitAt 2 un in
    let (bn,cn) = L.splitAt 2 un' in
    let sa = showChar 'A' . showString an in
    let sb = showChar 'B' . showString bn in
    let d = showChar '.' in
    (sa.d.sb.d)('C':cn)

-- Haskell module name to path and prefix (GHC conventions)
-- e.g. Foo.Bar.Baz → (Foo/Bar,Baz)
pathAndPrefix :: ModuleName -> (FS.FilePath,FS.FilePath)
pathAndPrefix = pp FS.empty where
    pp fp mn = case L.break (=='.') mn of
        (prefix,[]) -> (fp,FS.fromText (T.pack prefix))
        (fp',('.':mn')) -> pp (fp FS.</> (FS.fromText (T.pack fp'))) mn'
        _ -> error "illegal state for JIT.pathAndPrefix"


abc2hs_auto :: [Op] -> Either Error String
abc2hs_auto ops = abc2hs (opsToModName ops) ops

{-

abc2hs :: [Op] -> Either Error String
abc2hs ops = abc2hs' (opsToModName ops) ops where

abc2hs' :: ModuleName -> [Op] -> Either Error String
abc2hs' rn ops = Right $ abc2hs_naive rn ops

abc2hs_imports_naive :: [String]
abc2hs_imports_naive = 
    ["ABC.Imperative.Prelude"
    ]

abc2hs_naive :: ModuleName -> [Op] -> String
abc2hs_naive modName ops = (showHdr . showRsc . showFtr) "" where
    showHdr = modHdr . showChar '\n' . 
              showImports abc2hs_imports_naive . showChar '\n'
    modHdr = showString "module " . showString modName . 
             showString "\n    (resource) where"
    showImports (x:xs) = showString "import " . showString x . 
                         showChar '\n' . showImports xs
    showImports [] = id
    showRsc = 
        showString "resource :: Resource\n" .
        showString "resource = Resource (" .
        ops2hs_naive ops . showChar ')'
    showFtr = showString "\n\n"

ops2hs_naive :: [Op] -> ShowS
ops2hs_naive [] = showString "return"
ops2hs_naive (Op_ap:Op_c:[]) = showString "apc"
ops2hs_naive (op:[]) = op2hs_naive op
ops2hs_naive (op:ops) = op2hs_naive op . showString ">=>" . ops2hs_naive ops

opMapNaive :: M.Map Op String
opMapNaive = M.fromList $
    [(Op_l,"l"),(Op_r,"r"),(Op_w,"w"),(Op_z,"z"),(Op_v,"v"),(Op_c,"c")
    ,(Op_L,"sL"),(Op_R,"sR"),(Op_W,"sW"),(Op_Z,"sZ"),(Op_V,"sV"),(Op_C,"sC")
    ,(Op_copy,"cp"),(Op_drop,"rm")
    ,(Op_add,"add"),(Op_neg,"neg"),(Op_mul,"mul"),(Op_inv,"inv"),(Op_divMod,"divQ")
    ,(Op_ap,"ap"),(Op_cond,"co"),(Op_quote,"qu"),(Op_comp,"o")
    ,(Op_rel,"k"),(Op_aff,"f")
    ,(Op_distrib,"sD"),(Op_factor,"sF"),(Op_merge,"sM"),(Op_assert,"sK")
    ,(Op_gt,"gt")
    ,(Op_introNum,"n0")
    ,(Op_0,"d0"),(Op_1,"d1"),(Op_2,"d2"),(Op_3,"d3"),(Op_4,"d4")
    ,(Op_5,"d5"),(Op_6,"d6"),(Op_7,"d7"),(Op_8,"d8"),(Op_9,"d9")
    ,(Op_SP,"return"),(Op_LF,"return")
    ]
inOpMap :: Op -> Maybe String
inOpMap = flip M.lookup opMapNaive

op2hs_naive :: Op -> ShowS
op2hs_naive (inOpMap -> Just s) = showString s
op2hs_naive (TL s) = showString "tl" . shows s
op2hs_naive (Tok s) = showString "tok" . shows s
op2hs_naive (BL ops) = showString "bl" . opsStr . progVal where
    opsStr = shows (show ops) -- show all ops in a string
    progVal = showChar '(' . ops2hs_naive ops . showChar ')'
op2hs_naive op = error $ "op2hs_naive missing def for " ++ show op

-}

try :: IO a -> IO (Either Err.SomeException a)
try = Err.try -- type forced

tryJust :: IO a -> IO (Maybe a)
tryJust op = either (const Nothing) (Just) <$> try op

-}