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
    , Invoker, invNull
    , module AO.V
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.List as L
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
invNull :: (Monad m) => Invoker m -- minimal invoker
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
runOps _ (Op '$' : Op 'c' : []) = tailCall -- very useful
runOps rop (op:ops) = rop op >=> runOps rop ops
runOps _ [] = return 

-- tail execution of a block (usually due to `inline`)
tailCall :: (Monad m) => (V m -> m (V m))
tailCall (P (B _ abc) (P x U)) = return $ TC (abc_comp abc x)
tailCall v = fail ("$c] (tail call inline) @ " ++ show v)

-- minimal invoker
invNull txt =
    case T.uncons txt of
        Just ('&', _) -> return
        _ -> \ v -> fail ("{" ++ T.unpack txt ++ "}(?) @ " ++ show v)

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
runABC_B invoke = runOps (runOpABC invoke) . simpl [] . S.toList
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

runAMBC_B invoke = runOps (runOpAMBC invoke) . simpl [] . S.toList
runAMBC invoke ops = (runAMBC_B invoke ops) >=> tcLoop


--------------------------------------
-- SIMPLIFICATION
--------------------------------------

simplifyABC :: S.Seq Op -> S.Seq Op
simplifyABC = S.fromList . simpl [] . S.toList

-- zipper-based simplification
simpl :: [Op] -> [Op] -> [Op]
simpl acc (Op ' ' : ops) = simpl acc ops
simpl acc (Op '\n' : ops) = simpl acc ops
simpl (Op l:ls) (Op r:rs) | opsCancel l r = simpl ls rs
simpl (Op 'w' : Op 'z' : ls) (Op 'z' : rs) =
    simpl ls (Op 'w' : Op 'z' : Op 'w' : rs)
simpl (Op 'W' : Op 'Z' : ls) (Op 'Z' : rs) =
    simpl ls (Op 'W' : Op 'Z' : Op 'W' : rs) 
simpl acc (BL bops : ops) = 
    simpl (BL (simplifyABC bops) : acc) ops where
simpl acc (AMBC [singleton] : ops) = 
    simpl acc (S.toList singleton ++ ops)
simpl acc (AMBC ambOps : ops) = 
    simpl (AMBC (fmap simplifyABC ambOps) : acc) ops where
simpl acc (op:ops) = simpl (op:acc) ops
simpl acc [] = L.reverse acc

opsCancel :: Char -> Char -> Bool
opsCancel 'l' 'r' = True
opsCancel 'r' 'l' = True
opsCancel 'w' 'w' = True
opsCancel 'z' 'z' = True
opsCancel 'v' 'c' = True
opsCancel 'c' 'v' = True
opsCancel 'L' 'R' = True
opsCancel 'R' 'L' = True
opsCancel 'W' 'W' = True
opsCancel 'Z' 'Z' = True
opsCancel 'V' 'C' = True
opsCancel 'C' 'V' = True
opsCancel _ _ = False

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

