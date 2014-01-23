{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances #-}

-- | This module provides just enough to interpret ABC or AMBC code.
-- 
-- The code here is... sloppy, to put it mildly. It doesn't pay much
-- attention to performance, nor does it validate the ABC stream for
-- sane types. Error messages are inadequate for complicated work.
-- The API presented to users isn't very symmetric or elegant. 
--
-- TODO: develop support for tail calls, to avoid stack overflows and 
-- reduce memory overheads for large lists.
--
-- There is one concession to performance, which is generic quotation
-- of values - such that the quotation operator is cheap and does not
-- interfere with laziness.
--
-- The reader for ABC code is provided as Parsec parsers. It will 
-- handle AMBC code, too.
--
-- This implementation is not intended for much direct use. Rather,
-- it is a step towards bootrapping AO, ABC, and Awelon project. This
-- is the quick-and-dirty implementation.
--
-- Invocations are supported by monadic operators. AMBC is supported
-- only with MonadPlus. Pure variations are available. Failures will
-- inject an `{&fail}` annotation for debugging purposes. The invoker
-- described here is inadequate for implicit parallelism and flexible
-- scheduling. However, it will be enough for a simplified app model.
--
module AO.ABC
    ( runABC, runAMBC, runPureABC, runPureAMBC
    , parseABC, parseOp -- parse ABC or AMBC
    , readABC  -- parseABC on text
    , opCodeList, inlineOpCodeList
    , module AO.V
    ) where

import Control.Monad
import Data.Ratio
import qualified Text.Parsec as P
import Text.Parsec.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import AO.V

parseABC :: (P.Stream s m Char) => P.ParsecT s u m ABC
readABC :: Text -> Either Error ABC
droppable, copyable, observable :: V -> Bool
is_prod, is_sum, is_number, is_block :: V -> Bool
runABC :: (Monad m) => Invoker m -> V -> ABC -> m V
runPureABC :: V -> ABC -> V
runAMBC :: (MonadPlus m) => Invoker m -> V -> ABC -> m V
runPureAMBC :: V -> ABC -> Maybe V -- also works for AMBC
type Error = Text
type Invoker m = Text -> V -> m V -- used on invocation

-- single character opcodes; a subset are also used by AO
-- for inline ABC, but distinguished here to avoid redundancy
--
-- todo: consider rewriting a few sequences for tail call optimizations:
--    `?c`, `?Mc`, and `$c` or `$c` are good targets
-- either this, or find some way to ensure tail calls from AO (trampoline?)
opCodeList, inlineOpCodeList :: [Char]
opCodeList = " \n0123456789#" ++ inlineOpCodeList
inlineOpCodeList = "lrwzvcLRWZVC%^$'okf+*-/Q?DFMKPSBN>"

-- to cut down on verbose code, and because the 
-- syntax highlighting of 'Just' is distracting...
j :: v -> Maybe v
j = Just 

-- run pure operations; return Nothing if no rule applies
-- (includes sealer/unsealer invocation {$foo} and {/foo}
-- (excludes general invocation, amb, $, ?, ∞, Σ)
runPureOp :: Op -> V -> Maybe V
runPureOp (Qu v) e    = j (P v e)
runPureOp (Op ' ') v  = j v
runPureOp (Op '\n') v = j v
runPureOp (Op 'l') (P a (P b c)) = j (P (P a b) c)
runPureOp (Op 'r') (P (P a b) c) = j (P a (P b c))
runPureOp (Op 'w') (P a (P b c)) = j (P b (P a c))
runPureOp (Op 'z') (P a (P b (P c d))) = j (P a (P c (P b d)))
runPureOp (Op 'v') a = j (P a U)
runPureOp (Op 'c') (P a U) = j a
runPureOp (Op 'L') (P s e) = mbP (sumL s) e
runPureOp (Op 'R') (P s e) = mbP (sumR s) e
runPureOp (Op 'W') (P s e) = mbP (sumW s) e
runPureOp (Op 'Z') (P s e) = mbP (sumZ s) e
runPureOp (Op 'V') (P s e) = j (P (L s) e)
runPureOp (Op 'C') (P (L s) e) = j (P s e)
runPureOp (Op '%') (P x e) | droppable x = j e
runPureOp (Op '^') (P x e) | copyable x = j (P x (P x e))
runPureOp (Op 'o') (P (B yz) (P (B xy) e)) = j (P (B xz) e)
    where xz = Block { b_aff = aff, b_rel = rel, b_code = abc }
          aff = b_aff xy || b_aff yz
          rel = b_rel xy || b_rel yz
          abc = ABC $ inABC (b_code xy) ++ inABC (b_code yz)
runPureOp (Op '\'') (P v e) = j (P (B q) e)
    where q   = Block { b_aff = aff, b_rel = rel, b_code = abc }
          aff = (not . copyable) v
          rel = (not . droppable) v
          abc = ABC [Qu v]
runPureOp (Op 'k') (P (B b) e) = j (P (B b') e) 
    where b' = b { b_rel = True }
runPureOp (Op 'f') (P (B b) e) = j (P (B b') e)
    where b' = b { b_aff = True }
runPureOp (Op '+') (P (N x) (P (N y) e)) = j (P (N (x + y)) e)
runPureOp (Op '*') (P (N x) (P (N y) e)) = j (P (N (x * y)) e)
runPureOp (Op '/') (P (N x) e) | (0 /= x) = j (P (N (recip x)) e)
runPureOp (Op '-') (P (N x) e) = j (P (N (negate x)) e)
runPureOp (Op 'Q') (P (N b) (P (N a) e)) | (0 /= b) =
    let (r,q) = divModQ b a in
    j (P (N r) (P (N (fromIntegral q)) e))
runPureOp (Op 'D') (P a (P (L v) e)) = j (P (L (P a v)) e)
runPureOp (Op 'D') (P a (P (R v) e)) = j (P (R (P a v)) e)
runPureOp (Op 'F') (P (L (P a b)) e) = j (P (L a) (P (L b) e))
runPureOp (Op 'F') (P (R (P c d)) e) = j (P (R c) (P (R d) e))
runPureOp (Op 'M') (P (L a) e) = j (P a e) -- M assumes (a + a)
runPureOp (Op 'M') (P (R a) e) = j (P a e) 
runPureOp (Op 'K') (P (R b) e) = j (P b e) -- K asserts in right
runPureOp (Op 'P') (P x e) = mbP (obs is_prod x) e
runPureOp (Op 'S') (P x e) = mbP (obs is_sum x) e
runPureOp (Op 'B') (P x e) = mbP (obs is_block x) e
runPureOp (Op 'N') (P x e) = mbP (obs is_number x) e
runPureOp (Op '>') (P x (P y e)) =
    case opGT y x of
        Nothing -> Nothing
        Just True -> j (P (R (P x y)) e)
        Just False -> j (P (L (P y x)) e)
runPureOp (Op '#') e = j (P (N 0) e)
runPureOp (Op '0') (P (N x) e) = j (P (N (10*x + 0)) e)
runPureOp (Op '1') (P (N x) e) = j (P (N (10*x + 1)) e)
runPureOp (Op '2') (P (N x) e) = j (P (N (10*x + 2)) e)
runPureOp (Op '3') (P (N x) e) = j (P (N (10*x + 3)) e)
runPureOp (Op '4') (P (N x) e) = j (P (N (10*x + 4)) e)
runPureOp (Op '5') (P (N x) e) = j (P (N (10*x + 5)) e)
runPureOp (Op '6') (P (N x) e) = j (P (N (10*x + 6)) e)
runPureOp (Op '7') (P (N x) e) = j (P (N (10*x + 7)) e)
runPureOp (Op '8') (P (N x) e) = j (P (N (10*x + 8)) e)
runPureOp (Op '9') (P (N x) e) = j (P (N (10*x + 9)) e)
runPureOp (Invoke t) v | sealerCap t = j (S (T.tail t) v)
runPureOp (Invoke t) (S tsv v) | unsealerCap t tsv = j v
runPureOp _ _ = Nothing -- no more rules!

mbP :: Maybe V -> V -> Maybe V
mbP Nothing _ = Nothing
mbP (Just v1) v2 = Just (P v1 v2)

obs :: (V -> Bool) -> V -> Maybe V
obs cond v = 
    if (not . observable) v then Nothing else
    if cond v then Just (R v) else Just (L v)

sumL, sumR, sumW, sumZ :: V -> Maybe V
sumL (L a) = j (L (L a))
sumL (R (L b)) = j (L (R b))
sumL (R (R c)) = j (R c)
sumL _ = Nothing
sumR (L (L a)) = j (L a)
sumR (L (R b)) = j (R (L b))
sumR (R c) = j (R (R c))
sumR _ = Nothing
sumW (L a) = j (R (L a))
sumW (R (L b)) = j (L b)
sumW (R (R c)) = j (R (R c))
sumW _ = Nothing
sumZ (L a) = j (L a)
sumZ (R (L b)) = j (R (R (L b)))
sumZ (R (R (L c))) = j (R (L c))
sumZ (R (R (R d))) = j (R (R (R d)))
sumZ _ = Nothing

-- divModQ b a = (r,q)
--   such that qb + r = a
--             q is integral
--             r is in (b,0] or [0,b)
-- i.e. this is a divMod for rationals
divModQ :: Rational -> Rational -> (Rational, Integer)
divModQ b a = 
    let num = numerator a * denominator b in
    let den = numerator b * denominator a in
    let (qN,rN) = num `divMod` den in
    let denR = denominator a * denominator b in
    (rN % denR, qN)

-- opGreaterThan y x = Just (y > x)
--   Unless it attempts to compare something incomparable.
--   In that case, returns Nothing.
opGT :: V -> V -> Maybe Bool
opGT (P a b) (P a' b') =
    case opGT a a' of
        Nothing -> Nothing
        Just True -> j True
        Just False ->
            case opGT a' a of
                Nothing -> Nothing
                Just True -> j False
                Just False -> opGT b b'
opGT (N y) (N x) = j (y > x)
opGT y x | structGT y x = j True
opGT y x | structGT x y = j False
opGT (L a) (L a') = (opGT a a')
opGT (R b) (R b') = (opGT b b')
opGT U U = j False
opGT _ _ = Nothing

-- greater-than due to structure
structGT :: V -> V -> Bool
structGT (P _ _) x = is_number x || is_sum x
structGT (N _) x = is_sum x
structGT (R _) (L _) = True
structGT _ _ = False

-- trivial sealer/unsealer model
sealerCap :: Text -> Bool
sealerCap tCap = 
    case T.uncons tCap of
        Just ('$', _) -> True
        _ -> False

unsealerCap :: Text -> Text -> Bool
unsealerCap tCap tSV =
    case T.uncons tCap of
        Just ('/',tC') -> (tC' == tSV)
        _ -> False

-- can we drop this value?
droppable (B b) = not (b_rel b)
droppable (P a b) = droppable a && droppable b
droppable (L v) = droppable v
droppable (R v) = droppable v
droppable (N _) = True
droppable (S _ v) = droppable v
droppable U = True

-- can we copy this value?
copyable (B b) = not (b_aff b)
copyable (P a b) = copyable a && copyable b
copyable (L v) = copyable v
copyable (R v) = copyable v
copyable (N _) = True
copyable (S _ v) = copyable v
copyable U = True

-- can we introspect the value's structure?
observable U = False
observable (S _ _) = False
observable _ = True

is_prod (P _ _) = True
is_prod _ = False

is_sum (L _) = True
is_sum (R _) = True
is_sum _ = False

is_block (B _) = True
is_block _ = False

is_number (N _) = True
is_number _ = False

-- a few common effects
--  note: singleton options - i.e. (code) - are applied inline
--     but other uses of AMBC need special consideration.
runBlock :: (Monad m) => (V -> ABC -> m V) -> Op -> V -> Maybe (m V)
runBlock run (Op '$') (P (B b) (P x e)) = Just $ 
    run x (b_code b) >>= \ x' -> return (P x' e)
runBlock run (Op '?') (P (B b) (P (L x) e)) | not (b_rel b) = Just $
    run x (b_code b) >>= \ x' -> return (P (L x') e)
runBlock _ (Op '?') (P (B b) (P (R x) e)) | not (b_rel b) = Just $
    return (P (R x) e)
runBlock run (AMBC [singletonOpt]) v0 = Just $ run v0 singletonOpt
runBlock _ _ _ = Nothing

runBasic :: (Monad m) => (V -> ABC -> m V) -> Op -> V -> Maybe (m V)
runBasic run op v0 =
    case runPureOp op v0 of
        Just vf -> Just (return vf)
        Nothing -> runBlock run op v0

-- runABC :: (Monad m) => (Text -> V -> m V) -> V -> ABC -> m V
-- todo: consider modeling the call stacks more explicitly.
runABC _ v0 (ABC []) = return v0 -- done!
runABC invoke (P (B b) (P x U)) (ABC (Op '$' : Op 'c' : cc)) =
    runABC invoke x (ABC $ inABC (b_code b) ++ cc)
runABC invoke v0 (ABC (op:cc)) = runOp >>= runCC where
    run = runABC invoke
    runOp = maybe (runABC' invoke op v0) id (runBasic run op v0)
    runCC vf = run vf (ABC cc)

-- run a single (possibly impure) ABC
-- (note: ambiguous code here will result in failure).
runABC' :: (Monad m) => Invoker m -> Op -> V -> m V
runABC' invoke (Invoke cap) v0 = invoke cap v0 
runABC' invoke op v0 =
    invoke (T.pack "&fail") v0 >> -- for internal debugging purposes
    fail ("invalid ABC: " ++ (show op) ++ " @ " ++ (show v0))

-- to avoid pain of ghci module name conflicts between mtl and monad-tf...
newtype Id a = Id { runId :: a }
instance Monad Id where { return = Id; (>>=) (Id a) f = f a }

runPureABC v c = runId $ runABC inv v c where
    inv txt v0 = 
        case T.uncons txt of
            Just ('&', _) -> return v0
            _ -> fail ("unknown operation: {" ++ T.unpack txt ++ "}")

-- runAMBC :: (MonadPlus m) => (Text -> V -> m V) -> V -> ABC -> m V
runAMBC _ v0 (ABC []) = return v0 -- done!
runAMBC invoke (P (B b) (P x U)) (ABC (Op '$' : Op 'c' : cc)) =
    runAMBC invoke x (ABC $ inABC (b_code b) ++ cc)
runAMBC invoke v0 (ABC (op:cc)) = runOp >>= runCC where
    run = runAMBC invoke
    runOp = maybe (runAMBC' invoke op v0) id (runBasic run op v0)
    runCC vf = run vf (ABC cc)

runAMBC' :: (MonadPlus m) => Invoker m -> Op -> V -> m V
runAMBC' invoke (Invoke cap) v0 = invoke cap v0
runAMBC' invoke (AMBC opts) v0 = msum $ map (runAMBC invoke v0) opts
runAMBC' invoke _ v0 = invoke (T.pack "&fail") v0 >> mzero

runPureAMBC = runAMBC inv where
    inv txt v0 = 
       case T.uncons txt of
            Just ('&',_) -> return v0
            _ -> Nothing





--------------------------------------
-- PARSERS
--------------------------------------


-- parseABC :: (P.Stream s m t) => P.ParsecT s u m ABC
parseABC = 
    P.manyTill parseOp P.eof >>= \ ops ->
    return (ABC ops)

parseOp, parseCharOp, parseBlockLit,
         parseTextLit, parseInvocation, parseAmb
    :: (P.Stream s m Char) => P.ParsecT s u m Op

parseOp = op P.<?> "ABC op" where
    op = parseCharOp P.<|> 
         parseBlockLit P.<|>
         parseTextLit P.<|>
         parseInvocation P.<|>
         parseAmb

parseCharOp = 
    P.oneOf opCodeList >>= \ c -> 
    return (Op c)

parseBlockLit = 
    P.char '[' >> P.manyTill parseOp (P.char ']') >>=
    return . Qu . B . block . ABC

parseTextLit =
    P.char '"' >>
    parseTextLine >>= \ firstLine ->
    P.manyTill contLine (P.char '~') >>= \ moreLines ->
    let finalText = T.intercalate (T.singleton '\n') (firstLine : moreLines) in 
    return (Qu (textToVal finalText)) 
    where contLine = (P.char ' ' >> parseTextLine) P.<?> "continuing line of text"

parseTextLine :: (P.Stream s m Char) => P.ParsecT s u m Text
parseTextLine =
    P.manyTill (P.satisfy (/= '\n')) (P.char '\n') >>= \ s ->
    return (T.pack s)

parseInvocation = 
    P.char '{' >>
    P.manyTill (P.satisfy isTokenChar) (P.char '}') >>= \ invText ->
    return (Invoke (T.pack invText))

-- invocation tokens may not contain '{', '}', or LF ('\n')
isTokenChar :: Char -> Bool
isTokenChar c = not (('{' == c) || ('}' == c) || ('\n' == c))

parseAmb = 
    P.char '(' >>
    P.sepBy1 (P.many parseOp) (P.char '|') >>= \ opts ->
    P.char ')' >>
    return (AMBC (map ABC opts))

readABC = errtxt . P.runP parseABC () "readABC" 
    where errtxt = onLeft (T.pack . show)
          onLeft f = either (Left . f) Right

