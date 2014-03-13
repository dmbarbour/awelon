{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- dynamic AO/ABC values
module AO.V
    ( V(..), Op(..), ABC(..), KF(..), kf0
    , copyable, droppable, observable
    -- , ToABCV(..), FromABCV(..), toABCVL, fromABCVL
    , valToText, textToVal
    , abcQuote, abcLit
    , quoteNum, quoteNat, divModQ
    , opCodeList, inlineOpCodeList
    , showOps
    ) where

import Control.Applicative
import Data.Monoid
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.List as L

data V c -- ABC's structural types in context c
    = L (V c)    -- sum left
    | R (V c)    -- sum right
    | N {-# UNPACK #-} !Rational -- number
    | P (V c) (V c) -- product; tracks copy/drop allowance
    | B KF (ABC c) -- block
    | U -- unit
    | S !Text (V c)  -- sealed value (via sealer capability)
    -- pseudo-value hacks
    | TC (c (V c))   -- tail-call thunk

-- track affine and relevant properties
-- (these are tracked for blocks and products)
data KF = KF { may_copy :: !Bool, may_drop :: !Bool }

kf0 :: KF
kf0 = KF True True

instance Monoid KF where
    mempty = kf0
    mappend a b = KF { may_copy = (may_copy a && may_copy b)
                     , may_drop = (may_drop a && may_drop b) }

-- a block operates in a monadic context c
data ABC c = ABC
    { abc_code :: (S.Seq Op) -- code for show, structural equality
    , abc_comp :: (V c -> c (V c)) -- compiled form
    }

-- Ops no longer carry values directly. This is the bare
-- minimum for ABC. A compiler may target any category.
data Op
    = Op {-# UNPACK #-} !Char     -- a normal operator
    | TL !Text -- text literal
    | BL !(S.Seq Op) -- block literal
    | Invoke !Text -- {invocation}
    | AMBC ![S.Seq Op]  -- AMBC extension, set of options
    deriving (Ord,Eq) -- structural equality and ordering

-- ABC CODES
opCodeList, inlineOpCodeList :: [Char]
opCodeList = " \n0123456789#" ++ inlineOpCodeList
inlineOpCodeList = "lrwzvcLRWZVC%^$'okf+*-/Q?DFMK>"

-- showABC :: ABC -> Text
showABC :: ABC c -> Text
showABC = showOps . abc_code

showOps :: S.Seq Op -> Text
showOps = T.concat . L.map showOp . S.toList

showOp :: Op -> Text
showOp (Op c) = T.singleton c
showOp (TL text) = txtLit text
showOp (BL ops) = '[' `T.cons` (showOps ops) `T.snoc` ']'
showOp (Invoke text) = '{' `T.cons` text `T.snoc` '}'
showOp (AMBC options) = 
    let opTexts = map showOps options in
    let joinedTexts = T.intercalate (T.singleton '|') opTexts in
    '(' `T.cons` joinedTexts `T.snoc` ')'

abcQuote :: V c -> S.Seq Op
abcQuote (N r) = quoteNum r
abcQuote U = S.fromList intro1 where
    intro1 = (Op 'v' : Op 'v' : Op 'r' : Op 'w' : Op 'l' : Op 'c' : [])
abcQuote (B kf abc) = (addf . addk) bb  where
    bb = (S.singleton . BL . abc_code) abc
    addk = if (not . may_drop) kf then (S.|> (Op 'k')) else id
    addf = if (not . may_copy) kf then (S.|> (Op 'f')) else id
abcQuote (L v) = abcQuote v S.|> (Op 'V')
abcQuote (R v) = abcQuote v S.>< S.fromList intro0 where
    intro0 = (Op 'V' : Op 'V' : Op 'R' : Op 'W' : Op 'L' : Op 'C' : [])
abcQuote (S tok v) = abcQuote v S.|> reifySeal S.|> applySeal where
    reifySeal = (BL . S.singleton . Invoke . T.cons ':') tok
    applySeal = Op '$'
abcQuote v@(P a b) = 
    case valToText v of
        Just txt -> S.singleton (TL txt)
        Nothing -> 
            abcQuote a S.>< 
            (abcQuote b S.|> Op 'w' S.|> Op 'l')
abcQuote (TC _) = error "cannot quote tail-call pseudo-values!"

abcLit :: V c -> Text
abcLit = showOps . abcQuote

txtLit :: Text -> Text
txtLit = wrap . escapeLines . toLines where
    toLines = T.splitOn (T.singleton '\n')
    escapeLines = T.intercalate (T.pack "\n ")
    wrap = (T.cons '"') . (`T.snoc` '~') . (`T.snoc` '\n')

quoteNum :: Rational -> S.Seq Op
quoteNum r =
    if (r < 0) then quoteNum (negate r) S.|> Op '-' else
    let num = numerator r in
    let den = denominator r in
    if (1 == den) then quoteNat num else
    if (1 == num) then quoteNat den S.|> Op '/' else
    quoteNat num S.>< (quoteNat den S.|> Op '/' S.|> Op '*')

quoteNat :: Integer -> S.Seq Op
quoteNat n | (0 < n) =
    let (q,r) = n `divMod` 10 in
    let digit = toEnum (48 + fromIntegral r) in
    quoteNat q S.|> Op digit
quoteNat n | (0 == n) = S.singleton (Op '#')
           | otherwise = quoteNat (negate n) S.|> Op '-'

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

droppable, copyable, observable :: V c -> Bool

-- may we drop this value?
droppable (B kf _) = may_drop kf
droppable (P a b) = droppable a && droppable b
droppable (L v) = droppable v
droppable (R v) = droppable v
droppable (N _) = True
droppable (S _ v) = droppable v
droppable U = True
droppable (TC _) = False

-- may we copy this value?
copyable (B kf _) = may_copy kf
copyable (P a b) = copyable a && copyable b
copyable (L v) = copyable v
copyable (R v) = copyable v
copyable (N _) = True
copyable (S _ v) = copyable v
copyable U = True
copyable (TC _) = False

-- may we introspect the value's structure?
observable U = False
observable (S _ _) = False
observable (TC _) = False
observable _ = True

-- parse text from a sequence of numbers
valToText :: V c -> Maybe Text
valToText (N r) | (3 == r) = Just T.empty
valToText (P (N r) b) | validChar r =
    (((toEnum . fromInteger . numerator) r) `T.cons`) <$> 
    valToText b
valToText _ = Nothing

textToVal :: Text -> V c
textToVal t =
    case T.uncons t of
        Nothing -> N 3
        Just (c, t') -> 
            let r = (fromIntegral . fromEnum) c in
            P (N r) (textToVal t')

validChar :: Rational -> Bool
validChar r = 
    let n = numerator r in
    let d = denominator r in
    (1 == d) && (0 <= n) && (n <= 0x10ffff)

instance Show (ABC c) where 
    show = T.unpack . showABC

instance Show Op where 
    show = T.unpack . showOp

instance Show (V c) where 
    -- show = T.unpack . abcLit
    show (N r) | (1 == denominator r) = show (numerator r)
    show (N r) = show (numerator r) ++ "/" ++ show (denominator r)
    show p@(P a b) = 
        case valToText p of
            Just txt -> (T.unpack . txtLit) txt
            Nothing -> "(" ++ show a ++ "*" ++ show b ++ ")"
    show (L a) = "(" ++ show a ++ "+_)"
    show (R b) = "(_+" ++ show b ++ ")"
    show U     = "u"
    show (S t v) = show v ++ "{:" ++ T.unpack t ++ "}"
    show (TC _) = "{TAILCALL THUNK (SHOULD NOT BE SEEN!)}"
    show (B kf abc) = "[" ++ show abc ++ "]" ++ rel ++ aff
        where rel = if may_drop kf then "" else "k"
              aff = if may_copy kf then "" else "f"
              
