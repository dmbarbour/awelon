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
    ( parseABC, parseOp, runABC
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
--  few built-in capabilities:
--     seal   {$foo}  (for arbitrary token foo) 
--     unseal {/foo}  (for arbitrary token foo)
type Invoker m = Text -> V m -> m (V m)
runOp :: (Monad m) => Invoker m -> Op -> (V m -> m (V m))
runABC :: (Monad m) => Invoker m -> S.Seq Op -> (V m -> m (V m))

-- compile/run a single op
runOp _ (Op c) = runOpC c
runOp _ (TL text) = return . prod (textToVal text)
runOp invoke (Invoke tok) =
    case T.uncons tok of
        Just ('$', seal) -> op_invoke_seal seal
        Just ('/', seal) -> op_invoke_unseal seal
        _ -> invoke tok
runOp invoke (BL ops) = return . prod bb where
    bb = B kf0 abc
    abc = ABC { abc_code = ops, abc_comp = runABC invoke ops }
runOp invoke (AMBC [singleton]) = runABC invoke singleton
runOp _ (AMBC _) = const $ fail "AMBC is not supported"

runOpC :: (Monad c) => Char -> V c -> c (V c)
runOpC ' ' = op_sp
runOpC '\n' = op_lf
runOpC 'l' = op_l
runOpC 'r' = op_r
runOpC 'w' = op_w
runOpC 'z' = op_z
runOpC 'v' = op_v
runOpC 'c' = op_c
runOpC 'L' = op_L
runOpC 'R' = op_R
runOpC 'W' = op_W
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
runOpC c = const $ fail (c : " is not a valid ABC operator")


-- run all ops
runABC invoke = S.foldr (>=>) (return) . fmap (runOp invoke)

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

