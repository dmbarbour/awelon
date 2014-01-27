{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances #-}

-- | Parsing and processing for ABC code.
--
-- The new processor for ABC code involves automatically converting
-- code to a Haskell function from `(V c) -> c (V c)`, where `c` is
-- the monadic context. In general, errors are raised out-of-band.
-- Users must provide their own invoker for running capabilities.
-- 
-- The reader for ABC code is provided as Parsec parsers. It will 
-- parse AMBC code, too, but AMBC with more than one option will fail
-- at run time. 
--
-- This implementation is not intended for heavy-duty use. It does not
-- optimize code, nor does it provide any parallelism. The intention is
-- to have a solution fast enough for bootstrapping. If this doesn't
-- work, the next step would be compiling an AO dictionary to Haskell
-- (to leverage GHC's optimizer).
--
module AO.ABC
    ( parseABC, runABC, runAMBC
    , simplifyABC
    , Invoker
    , module AO.V
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.List as L
import qualified Text.Parsec as P
import qualified Data.Map as M
import Text.Parsec.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import AO.V
import AO.Op

-- PARSE 
parseABC :: (P.Stream s m Char) => P.ParsecT s u m (S.Seq Op)

-- RUN 
--  an 'invoker' in this case translates capability text into
--  user actions. However, the invoker isn't necessary for a 
--  few common capabilities:
--     seal   {$foo}  (for arbitrary token foo) 
--     unseal {/foo}  (for arbitrary token foo)
type Invoker m = Text -> V m -> m (V m)
runABC :: (Monad m) => Invoker m -> S.Seq Op -> (V m -> m (V m))
runAMBC :: (MonadPlus m) => Invoker m -> S.Seq Op -> (V m -> m (V m))

-- MAP OF OPERATION SEQUENCES 
--  includes ALL single-character ops
--  plus (potentially) common useful multi-character operations
--  the latter allows Haskell to partially optimize
opsMap :: (Monad c) => M.Map [Op] (V c -> c (V c))
opsMap = M.fromList $ 
    [ opc ' ' op_sp -- identity functions
    , opc '\n' op_lf
    , opc 'l' op_l -- data shuffling
    , opc 'r' op_r
    , opc 'w' op_w
    , opc 'z' op_z
    , opc 'v' op_v
    , opc 'c' op_c
    , opc 'L' op_L
    , opc 'R' op_R
    , opc 'W' op_W
    , opc 'Z' op_Z
    , opc 'V' op_V
    , opc 'C' op_C
    , opc '^' op_copy
    , opc '%' op_drop
    , opc '$' op_apply -- blocks
    , opc '\'' op_quote
    , opc 'o' op_compose
    , opc 'k' op_rel
    , opc 'f' op_aff
    , opc '+' op_add -- math
    , opc '*' op_mul
    , opc '-' op_negate
    , opc '/' op_recip
    , opc 'Q' op_Q
    , opc '?' op_cond -- sum types
    , opc 'D' op_D
    , opc 'F' op_F
    , opc 'M' op_M
    , opc 'K' op_K
    , opc 'P' op_P
    , opc 'S' op_S
    , opc 'B' op_B
    , opc 'N' op_N
    , opc '>' op_GT
    , opc '#' op_num  -- pseudoliteral numbers
    , opc '0' op_0
    , opc '1' op_1
    , opc '2' op_2
    , opc '3' op_3
    , opc '4' op_4
    , opc '5' op_5
    , opc '6' op_6
    , opc '7' op_7
    , opc '8' op_8
    , opc '9' op_9

    -- short sequences of 'rw' and 'wl' are very common
    , opL "rw" (ops_rw)
    , opL "rwr" (op_r >=> op_w >=> op_r)
    , opL "wrw" (op_w >=> op_r >=> op_w)
    , opL "rwrw" (ops_rw >=> ops_rw)
    , opL "rwrwrw" (ops_rw >=> ops_rw >=> ops_rw)
    , opL "wl" (ops_wl)
    , opL "lwl" (op_l >=> op_w >=> op_l)
    , opL "wlw" (op_w >=> op_l >=> op_w)
    , opL "wlwl" (ops_wl >=> ops_wl)
    , opL "wlwlwl" (ops_wl >=> ops_wl >=> ops_wl)

    -- optimizing 'inline' and 'dip' are especially important
    , opL "vr$c" ops_inline
    , opL "vrwr$wlc" ops_dip

    -- prim not is common; prim swap is just didactic
    , opL "VRWLC" ops_not
    , opL "vrwlc" ops_swap

    -- miscellaneous common sequences
    , opL "zw" (op_z >=> op_w)
    , opL "wz" (op_w >=> op_z)
    , opL "rz" (op_r >=> op_z)
    , opL "zl" (op_z >=> op_l)
    , opL "zr" (op_z >=> op_r)
    , opL "lz" (op_l >=> op_z)
    , opL "rr"  (op_r >=> op_r)
    , opL "ll"  (op_l >=> op_l)

    , opL "rzl" (op_r >=> op_z >=> op_l)
    , opL "lzr" (op_l >=> op_z >=> op_r)
    , opL "wzw" (op_w >=> op_z >=> op_w)
    , opL "zwz" (op_z >=> op_w >=> op_z)
    , opL "wzl" (op_w >=> op_z >=> op_l)
    , opL "rzw" (op_r >=> op_z >=> op_w)
    , opL "zwl" (op_z >=> op_w >=> op_l)
    , opL "rwz" (op_r >=> op_w >=> op_z)
    , opL "wrz" (op_w >=> op_r >=> op_z)
    , opL "zlw" (op_z >=> op_l >=> op_w)

    , opL "rzlw" (op_r >=> op_z >=> op_l >=> op_w)
    , opL "wrzl" (op_w >=> op_r >=> op_z >=> op_l)
    , opL "rwrz" (ops_rw >=> op_r >=> op_z)
    , opL "zlwl" (op_z >=> op_l >=> op_w >=> op_l)
    , opL "rwzl" (ops_rw >=> op_z >=> op_l)
    , opL "rzwl" (op_r >=> op_z >=> ops_wl)
    , opL "wrzw" (op_w >=> op_r >=> op_z >=> op_w)
    , opL "wzlw" (op_w >=> op_z >=> op_l >=> op_w)

    , opL "vrwv" (op_v >=> op_r >=> op_w >=> op_v)
    , opL "cwlc" (op_c >=> op_w >=> op_l >=> op_c)
    , opL "vrr" (ops_vrr)
    , opL "llc" (op_l >=> op_l >=> op_c)

    -- helpers for 'apply'
    , opL "vrrvrrz" (ops_vrr >=> ops_vrr >=> op_z)
    , opL "wlcllc" (op_w >=> op_l >=> op_c >=> op_l >=> op_l >=> op_c)
    ]

opc :: (Monad m) => Char -> (V m -> m (V m)) -> ([Op], V m -> m (V m))
opc c action = ([Op c], action)

opL :: (Monad m) => [Char] -> (V m -> m (V m)) -> ([Op], V m -> m (V m))
opL s action = (map Op s, action)

ops_inline, ops_dip, 
 ops_not, ops_swap,
 ops_rw, ops_wl, ops_vrr
    :: (Monad m) => V m -> m (V m)

-- inline and dip are two very important optimizations
ops_inline (P (B _ abc) x) = abc_comp abc x
ops_inline v = fail ("vr$c (prim inline) @ " ++ show v)

ops_dip (P h (P (B _ abc) x)) = abc_comp abc x >>= \ x' -> return (P h x')
ops_dip v = fail ("vrwr$wlc (prim dip) @ " ++ show v)

ops_not (P (L a) e) = return (P (R a) e)
ops_not (P (R b) e) = return (P (L b) e)
ops_not v = fail ("VRWLC (prim not) @ " ++ show v)

ops_swap (P a b) = return (P b a)
ops_swap v = fail ("vrwlc (prim swap) @ " ++ show v)

ops_rw = op_r >=> op_w
ops_wl = op_w >=> op_l
ops_vrr = op_v >=> op_r >=> op_r

-- Greedily recognize and run sequences of matchable operations.
-- Adding common sequences or specializations to the map should
-- generally improve performance.
runOps :: (Monad m) => (Op -> (V m -> m (V m))) -> [Op] -> (V m -> m (V m))
runOps _ [] = return
runOps rop l@(op:ops) = 
    let run1 = rop op >=> runOps rop ops in
    case largestMatch l opsMap of
        Nothing -> run1
        Just (k, opFound) -> 
            case L.stripPrefix k l of
                Nothing -> error ("invalid match: " ++ show k ++ " @ " ++ show l)
                Just ops' -> opFound >=> runOps rop ops'

-- find the largest non-empty match for a list
-- assumes at least first element must match
largestMatch :: (Ord a) => [a] -> M.Map [a] b -> Maybe ([a],b)
largestMatch [] _ = Nothing
largestMatch l@(op1:_) mFull =
    let kLower = [op1] in
    let kUpper = l in
    let (_, mb1, mUpper) = M.splitLookup kLower mFull in
    let (mInner, mbk, _) = M.splitLookup kUpper mUpper in
    mbdist kUpper mbk <|>   -- upper bound is exact match
    lmInner l mInner <|>    -- finite search of map against key
    mbdist kLower mb1       -- lower bound is match one element
    -- don't even try an empty match

-- at lmInner, we should have a small, finite sequence of keys
-- that are near matches to the original list. Search for the
-- largest such match.
lmInner :: (Ord a) => [a] -> M.Map [a] b -> Maybe ([a],b)
lmInner l m = 
    let fPrefix = const . L.isPrefixOf l in
    let mf = M.filterWithKey fPrefix m in
    if M.null mf then Nothing else
    Just $ M.findMax mf 

mbdist :: a -> Maybe b -> Maybe (a,b)
mbdist a mb = (,) <$> pure a <*> mb


-----------------------------------------------------
-- TRANSLATE ABC OPERATIONS TO MONADIC CODE
-----------------------------------------------------

-- compile/run a non-matchable operation
runOpABC :: (Monad m) => Invoker m -> Op -> (V m -> m (V m))
runOpABC _ (TL text) = return . P (textToVal text)
runOpABC invoke (BL ops) = return . P (B kf0 abc) where
    abc = ABC { abc_code = ops, abc_comp = runABC invoke ops }
runOpABC invoke (Invoke tok) =
    case T.uncons tok of
        Just ('$', seal) -> op_invoke_seal seal
        Just ('/', seal) -> op_invoke_unseal seal
        _ -> invoke tok
runOpABC invoke (AMBC [singleton]) = runABC invoke singleton
runOpABC _ (AMBC _) = const $ fail "AMBC not supported by runABC"
-- we only reach `Op c` if it wasn't a prefix in our map, i.e. unknown 
runOpABC _ (Op c) = \ v -> fail ("unknown operator! " ++ (c : " @ ") ++ show v)

-- run all ops
runABC invoke = runOps (runOpABC invoke) . simpl . S.toList

--------------------------------------
-- AMBC can be executed similarly to ABC, except for 
-- how it processes the AMBC operation. AMBC requires
-- MonadPlus to handle the options. (The intention is that
-- AMBC eventually resolve to a safe program at compile-time.)
--------------------------------------

-- compile/run a single op
runOpAMBC :: (MonadPlus m) => Invoker m -> Op -> (V m -> m (V m))
runOpAMBC _ (TL text) = return . P (textToVal text)
runOpAMBC invoke (Invoke tok) =
    case T.uncons tok of
        Just ('$', seal) -> op_invoke_seal seal
        Just ('/', seal) -> op_invoke_unseal seal
        _ -> invoke tok
runOpAMBC invoke (BL ops) = return . P (B kf0 ambc) where
    ambc = ABC { abc_code = ops, abc_comp = runAMBC invoke ops }
runOpAMBC invoke (AMBC [singleton]) = runAMBC invoke singleton
runOpAMBC invoke (AMBC options) = 
    let compiledOptions = fmap (runAMBC invoke) options in
    \ arg -> msum $ map ($ arg) compiledOptions 
-- we only reach `Op c` if it wasn't a prefix in our map, i.e. unknown 
runOpAMBC _ (Op c) = \ v -> fail ("unknown operator! " ++ (c : " @ ") ++ show v)

runAMBC invoke = runOps (runOpAMBC invoke) . simpl . S.toList

--------------------------------------
-- SIMPLIFICATION
--------------------------------------

simplifyABC :: S.Seq Op -> S.Seq Op
simplifyABC = S.fromList . fmap simplBL . simpl . S.toList

simplBL :: Op -> Op
simplBL (BL ops) = BL (simplifyABC ops)
simplBL (AMBC options) = AMBC (fmap simplifyABC options)
simplBL op = op

-- a simplistic simplifier
simpl :: [Op] -> [Op] 
simpl [] = []
simpl (Op ' ' : ops) = simpl ops
simpl (Op '\n' : ops) = simpl ops
simpl (Op 'l' : Op 'r' : ops) = simpl ops
simpl (Op 'r' : Op 'l' : ops) = simpl ops
simpl (Op 'w' : Op 'w' : ops) = simpl ops
simpl (Op 'z' : Op 'z' : ops) = simpl ops
simpl (Op 'v' : Op 'c' : ops) = simpl ops 
-- from wzw = zwz
simpl (Op 'w' : Op 'z' : Op 'w' : Op 'z' : ops) = simpl (Op 'z' : Op 'w' : simpl ops)
simpl (Op 'z' : Op 'w' : Op 'z' : Op 'w' : ops) = simpl (Op 'w' : Op 'z' : simpl ops)
-- from rwlz = zrwl
simpl (Op 'r' : Op 'w' : Op 'l' : Op 'z' : Op 'r' : ops)
    = simpl (Op 'z' : Op 'r' : Op 'w' : ops)
simpl (Op 'z' : Op 'r' : Op 'w' : Op 'l' : Op 'z' : ops)
    = simpl (Op 'r' : Op 'w' : Op 'l' : ops)
simpl (Op 'l' : Op 'z' : Op 'r' : Op 'w' : Op 'l' : ops)
    = simpl (Op 'w' : Op 'l' : Op 'z' : ops)
-- from wlzrw = lzr
simpl (Op 'w' : Op 'l' : Op 'z' : Op 'r' : Op 'w' : ops)
    = simpl (Op 'l' : Op 'z' : Op 'r' : ops)
-- inline singleton ambc
simpl (AMBC [singleton] : ops) = simpl ((S.toList singleton) ++ ops)
-- continue simplification (single pass)
simpl (op : ops) = 
    let ops' = simpl ops in
    let k = simplWindow in
    let tkops = take k ops in
    let (tkops', dkops') = splitAt k ops' in
    if (tkops == tkops') then (op : ops') else
    simpl (op : tkops') ++ dkops'

-- window for deep simplification
simplWindow :: Int
simplWindow = 16


--------------------------------------
-- PARSERS
--------------------------------------

-- parseABC :: (P.Stream s m t) => P.ParsecT s u m ABC
parseABC = S.fromList <$> P.manyTill parseOp P.eof

parseOp, parseCharOp, parseBlockLit,
         parseTextLit, parseInvocation, parseAmb
    :: (P.Stream s m Char) => P.ParsecT s u m Op

parseOp = op P.<?> "ABC op" where
    op = parseCharOp P.<|> 
         parseBlockLit P.<|>
         parseTextLit P.<|>
         parseInvocation P.<|>
         parseAmb

parseCharOp = Op <$> P.oneOf opCodeList
parseBlockLit = BL . S.fromList <$> parseBlockOps where
    parseBlockOps = P.char '[' >> P.manyTill parseOp (P.char ']')
parseInvocation = Invoke . T.pack <$> parseCapText where
    parseCapText = P.char '{' >> P.manyTill (P.satisfy isTokenChar) (P.char '}') 

parseTextLit = TL <$> textLit where
    textLit = 
        P.char '"' >>
        textLine >>= \ lH ->
        P.manyTill contLine (P.char '~') >>= \ lT ->
        return (T.intercalate (T.singleton '\n') (lH : lT))
    textLine = T.pack <$> (P.manyTill P.anyChar (P.char '\n'))
    contLine = (P.char ' ' >> textLine) P.<?> "continuing line of text"

parseAmb = 
    P.char '(' >>
    P.sepBy1 (P.many parseOp) (P.char '|') >>= \ options ->
    P.char ')' >>
    return (AMBC (map S.fromList options))

-- invocation tokens may not contain '{', '}', or LF ('\n')
isTokenChar :: Char -> Bool
isTokenChar c = not (('{' == c) || ('}' == c) || ('\n' == c))






