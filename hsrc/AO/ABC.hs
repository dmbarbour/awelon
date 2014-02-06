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
import qualified Text.Parsec as P
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
runABC, runABC_B :: (Monad m) => Invoker m -> S.Seq Op -> (V m -> m (V m))
runAMBC, runAMBC_B :: (MonadPlus m) => Invoker m -> S.Seq Op -> (V m -> m (V m))

-- run a single operation
runOpC :: (Monad m) => Char -> V m -> m (V m)
runOpC 'w' = op_w
runOpC 'l' = op_l
runOpC 'r' = op_r
runOpC 'z' = op_z
runOpC 'v' = op_v
runOpC 'c' = op_c
runOpC 'W' = op_W
runOpC 'L' = op_L
runOpC 'R' = op_R
runOpC 'Z' = op_Z
runOpC 'V' = op_V
runOpC 'C' = op_C
runOpC '^' = op_copy
runOpC '%' = op_drop
runOpC '$' = op_apply
runOpC '\'' = op_quote
runOpC 'o' = op_compose
runOpC 'k' = op_rel
runOpC 'f' = op_aff
runOpC '+' = op_add
runOpC '*' = op_mul
runOpC '-' = op_negate
runOpC '/' = op_recip
runOpC 'Q' = op_Q
runOpC '?' = op_cond
runOpC 'D' = op_D
runOpC 'F' = op_F
runOpC 'M' = op_M
runOpC 'K' = op_K
runOpC 'P' = op_P
runOpC 'S' = op_S
runOpC 'B' = op_B
runOpC 'N' = op_N
runOpC '>' = op_GT
runOpC '#' = op_num
runOpC '0' = op_0
runOpC '1' = op_1
runOpC '2' = op_2
runOpC '3' = op_3
runOpC '4' = op_4
runOpC '5' = op_5
runOpC '6' = op_6
runOpC '7' = op_7
runOpC '8' = op_8
runOpC '9' = op_9
runOpC ' ' = op_sp
runOpC '\n' = op_lf
runOpC c = \v -> 
    fail $ "unrecognized operator " ++ (c:[]) 
        ++ " @ " ++ show v

-- run a sequence of operations with tail-call optimization
-- for moment, only code of the form `$c]` is optimized
runOps :: (Monad m) => (Op -> (V m -> m (V m))) -> [Op] -> (V m -> m (V m))
runOps _ [] = return 
runOps _ (Op '$' : Op 'c' : []) = tailCall -- very useful
runOps _ (Op '?' : Op 'M' : Op 'c' : []) = tailCallMerge -- experimental
runOps rop (op:ops) = rop op >=> runOps rop ops

-- tail execution of a block (usually due to `inline`)
tailCall :: (Monad m) => (V m -> m (V m))
tailCall (P (B _ abc) (P x U)) = return $ TC (abc_comp abc x)
tailCall v = fail ("$c] (tail call inline) @ " ++ show v)

-- tail conditional execution of a block (experimental, for `inlineLeft`)
tailCallMerge :: (Monad m) => V m -> m (V m)
tailCallMerge (P (B kf abc) (P (L x) U)) | may_drop kf = return $ TC (abc_comp abc x)
tailCallMerge (P (B kf _) (P (R x) U)) | may_drop kf = return x
tailCallMerge v = fail ("?Mc] (tail call merge) @ " ++ show v)


-----------------------------------------------------
-- TRANSLATE ABC OPERATIONS TO MONADIC CODE
-----------------------------------------------------

-- compile/run a non-matchable operation
runOpABC :: (Monad m) => Invoker m -> Op -> (V m -> m (V m))
runOpABC _ (Op c) = runOpC c
runOpABC _ (TL text) = return . P (textToVal text)
runOpABC invoke (BL ops) = return . P (B kf0 abc) where
    abc = ABC { abc_code = ops, abc_comp = runABC_B invoke ops }
runOpABC invoke (Invoke tok) =
    case T.uncons tok of
        Just (':', seal) -> op_invoke_seal seal
        Just ('.', seal) -> op_invoke_unseal seal
        _ -> invoke tok
runOpABC invoke (AMBC [singleton]) = runABC invoke singleton
runOpABC _ (AMBC _) = const $ fail "AMBC not supported by runABC"

-- run ops
runABC_B invoke = runOps (runOpABC invoke) . simpl . S.toList
runABC invoke ops = (runABC_B invoke ops) >=> tcLoop


--------------------------------------
-- AMBC can be executed similarly to ABC, except for 
-- how it processes the AMBC operation. AMBC requires
-- MonadPlus to handle the options. (The intention is that
-- AMBC eventually resolve to a safe program at compile-time.)
--------------------------------------

-- compile/run a single op
runOpAMBC :: (MonadPlus m) => Invoker m -> Op -> (V m -> m (V m))
runOpAMBC _ (Op c) = runOpC c
runOpAMBC _ (TL text) = return . P (textToVal text)
runOpAMBC invoke (Invoke tok) =
    case T.uncons tok of
        Just (':', seal) -> op_invoke_seal seal
        Just ('.', seal) -> op_invoke_unseal seal
        _ -> invoke tok
runOpAMBC invoke (BL ops) = return . P (B kf0 ambc) where
    ambc = ABC { abc_code = ops, abc_comp = runAMBC_B invoke ops }
runOpAMBC invoke (AMBC [singleton]) = runAMBC invoke singleton
runOpAMBC invoke (AMBC options) = 
    let compiledOptions = fmap (runAMBC invoke) options in
    \ arg -> msum $ map ($ arg) compiledOptions 

runAMBC_B invoke = runOps (runOpAMBC invoke) . simpl . S.toList
runAMBC invoke ops = (runAMBC_B invoke ops) >=> tcLoop


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
simpl (Op 'z' : Op 'w' : Op 'z' : ops) = simpl (Op 'w' : Op 'z' : Op 'w' : ops)
-- from lzrw = wlzr
--simpl (Op 'l' : Op 'z' : Op 'r' : Op 'w' : Op 'l' : ops) = simpl (Op 'w' : Op 'l' : Op 'z' : ops)
--simpl (Op 'r' : Op 'w' : Op 'l' : Op 'z' : Op 'r' : ops) = simpl (Op 'z' : Op 'r' : Op 'w' : ops)
--simpl (Op 'w' : Op 'l' : Op 'z' : Op 'r' : Op 'w' : ops) = simpl (Op 'l' : Op 'z' : Op 'r' : ops)
-- continue simplification (single pass)
simpl (AMBC [singleton] : ops) = simpl ((S.toList singleton) ++ ops)
simpl (op : ops) = 
    let ops' = simpl ops in
    let k = simplWindow in
    let tkops = take k ops in
    let (tkops', dkops') = splitAt k ops' in
    if (tkops == tkops') then (op : ops') else
    simpl (op : tkops') ++ dkops'

-- window for deep simplification
simplWindow :: Int
simplWindow = 12

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

