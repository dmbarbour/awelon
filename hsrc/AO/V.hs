{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- dynamic AO/ABC values
module AO.V
    ( V(..), prod, Op(..), ABC(..), KF(..), kf0
    , copyable, droppable, observable
    -- , ToABCV(..), FromABCV(..), toABCVL, fromABCVL
    , valToText, textToVal
    , abcQuote, abcLit
    , quoteNum, quoteNat
    , opCodeList, inlineOpCodeList
    ) where

import Control.Applicative
import Data.Function (on)
import Data.Ratio
--import Data.ByteString (ByteString)
--import qualified Data.ByteString as B
--import qualified Data.Word as W
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.List as L

data V c -- ABC's structural types in context c
    = L !(V c)    -- sum left
    | R !(V c)    -- sum right
    | N {-# UNPACK #-} !Rational -- number
    | P !KF !(V c) !(V c) -- product; tracks copy/drop allowance
    | B {-# UNPACK #-} !KF {-# UNPACK #-} !(ABC c) -- block
    | U -- unit
    | S !Text !(V c)  -- sealed value (via sealer capability)
    deriving (Eq)

-- track affine and relevant properties
-- (these are tracked for blocks and products)
data KF = KF { may_copy :: Bool, may_drop :: Bool }
    deriving (Eq)
kf0 :: KF
kf0 = KF True True

-- a block operates in a monadic context c
data ABC c = ABC
    { abc_code :: (S.Seq Op) -- code for show, structural equality
    , abc_comp :: !(V c -> c (V c)) -- compiled form
    }
instance Eq (ABC c) where (==) = (==) `on` abc_code 

-- Ops no longer carry values directly. This is the bare
-- minimum for ABC. A compiler may target any category.
data Op
    = Op {-# UNPACK #-} !Char     -- a normal operator
    | TL !Text -- text literal
    | BL !(S.Seq Op) -- block literal
    | Invoke !Text -- {invocation}
    | AMBC ![S.Seq Op]  -- AMBC extension, set of options
    deriving (Eq) -- structural equality

-- ABC CODES
opCodeList, inlineOpCodeList :: [Char]
opCodeList = " \n0123456789#" ++ inlineOpCodeList
inlineOpCodeList = "lrwzvcLRWZVC%^$'okf+*-/Q?DFMKPSBN>"

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
abcQuote (B kf abc) = (addk . addf) bb  where
    bb = (S.singleton . BL . abc_code) abc
    addk = if (not . may_drop) kf then (S.|> (Op 'k')) else id
    addf = if (not . may_copy) kf then (S.|> (Op 'f')) else id
abcQuote (L v) = abcQuote v S.|> (Op 'V')
abcQuote (R v) = abcQuote v S.>< S.fromList intro0 where
    intro0 = (Op 'V' : Op 'V' : Op 'R' : Op 'W' : Op 'L' : Op 'C' : [])
abcQuote (S tok v) = abcQuote v S.|> Invoke ('$' `T.cons` tok)
abcQuote v@(P _ a b) = 
    case valToText v of
        Just txt -> S.singleton (TL txt)
        Nothing -> 
            abcQuote a S.>< 
            (abcQuote b S.|> Op 'w' S.|> Op 'l')

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

-- smart constructor for products (lazily tracks copyable and droppable)
prod :: V c -> V c -> V c
prod a b = P kf a b where
    kf = KF { may_copy = (copyable a && copyable b)
            , may_drop = (droppable a && droppable b)
            }

droppable, copyable, observable :: V c -> Bool

-- may we drop this value?
droppable (B kf _) = may_drop kf
droppable (P kf _ _) = may_drop kf
droppable (L v) = droppable v
droppable (R v) = droppable v
droppable (N _) = True
droppable (S _ v) = droppable v
droppable U = True

-- may we copy this value?
copyable (B kf _) = may_copy kf
copyable (P kf _ _) = may_copy kf
copyable (L v) = copyable v
copyable (R v) = copyable v
copyable (N _) = True
copyable (S _ v) = copyable v
copyable U = True

-- may we introspect the value's structure?
observable U = False
observable (S _ _) = False
observable _ = True

-- parse text from a sequence of numbers
valToText :: V c -> Maybe Text
valToText (N r) | (3 == r) = Just T.empty
valToText (P kf (N r) b) | nonLinear kf && validChar r  =
    (((toEnum . fromInteger . numerator) r) `T.cons`) <$> 
    valToText b
valToText _ = Nothing

textToVal :: Text -> V c
textToVal t =
    case T.uncons t of
        Nothing -> N 3
        Just (c, t') -> 
            let r = (fromIntegral . fromEnum) c in
            P kf0 (N r) (textToVal t')

nonLinear :: KF -> Bool
nonLinear kf = may_copy kf && may_drop kf

validChar :: Rational -> Bool
validChar r = 
    let n = numerator r in
    let d = denominator r in
    (1 == d) && (0 <= n) && (n <= 0x10ffff)



{-
--
-- HASKELL / ABC INTEGRATION
--
-- (a) conversion functions for values (ToABCV, FromABCV)
-- (b) need a useful invoker for bootstrap purposes
--
class ToABCV v where toABCV :: v -> V c
instance ToABCV (Ratio Integer) where toABCV = N 
instance ToABCV Integer where toABCV = N . fromInteger
instance (ToABCV a, ToABCV b) => ToABCV (Either a b) where 
    toABCV = either (L . toABCV) (R . toABCV)
instance (ToABCV a) => ToABCV (Maybe a) where
    toABCV = maybe (L U) (R . toABCV)
instance (ToABCV a) => ToABCV [a] where
    toABCV = toABCVL 0
instance (ToABCV a, ToABCV b) => ToABCV (a,b) where
    toABCV (a,b) = P (toABCV a) (toABCV b)
instance ToABCV Text where toABCV = toABCVL 3 . T.unpack
instance ToABCV Char where toABCV = N . fromIntegral . fromEnum
instance ToABCV ByteString where toABCV = toABCVL 8 . B.unpack
instance ToABCV W.Word8 where toABCV = N . fromIntegral
instance ToABCV () where toABCV () = U

-- generate list terminated by number
toABCVL :: (ToABCV a) => Rational -> [a] -> V c
toABCVL vf [] = (N vf)
toABCVL vf (v:vs) = toABCV v `P` toABCVL vf vs

-- read list terminated by specific number
fromABCVL :: (FromABCV a) => Rational -> V c -> Maybe [a]
fromABCVL rF (N rF') | rF == rF' = Just []
fromABCVL rF (P a la) = (:) <$> fromABCV a <*> fromABCVL rF la
fromABCVL _ _ = Nothing

class FromABCV v where fromABCV :: V c -> Maybe v
instance FromABCV Integer where
    fromABCV (N r) | (1 == denominator r) = Just (numerator r)
    fromABCV _ = Nothing
instance FromABCV (Ratio Integer) where
    fromABCV (N r) = Just r
    fromABCV _ = Nothing
instance (FromABCV a, FromABCV b) => FromABCV (Either a b) where
    fromABCV (L a) = Left <$> fromABCV a 
    fromABCV (R b) = Right <$> fromABCV b
    fromABCV _ = Nothing
instance (FromABCV a) => FromABCV (Maybe a) where
    fromABCV (L U) = Just Nothing
    fromABCV (R a) = Just <$> fromABCV a
    fromABCV _ = Nothing
instance (FromABCV a) => FromABCV [a] where
    fromABCV = fromABCVL 0
instance (FromABCV a, FromABCV b) => FromABCV (a,b) where
    fromABCV (P a b) = (,) <$> fromABCV a <*> fromABCV b
    fromABCV _ = Nothing
instance FromABCV Text where 
    fromABCV l = T.pack <$> fromABCVL 3 l
instance FromABCV Char where
    fromABCV (N x) =
        if (1 /= denominator x) then Nothing else
        let n = numerator x in
        if (n < 0 || n > 0x10ffff) then Nothing else
        Just ((toEnum . fromIntegral) n)
    fromABCV _ = Nothing
instance FromABCV ByteString where 
    fromABCV l = B.pack <$> fromABCVL 8 l
instance FromABCV W.Word8 where
    fromABCV (N x) =
        if (1 /= denominator x) then Nothing else
        let n = numerator x in
        if (n < 0 || n > 255) then Nothing else
        Just (fromIntegral n)
    fromABCV _ = Nothing
instance FromABCV () where
    fromABCV U = Just ()
    fromABCV _ = Nothing
-}

instance Show (ABC c) where 
    show = T.unpack . showABC

instance Show Op where 
    show = T.unpack . showOp

instance Show (V c) where 
    -- show = T.unpack . abcLit
    show (N r) | (1 == denominator r) = show (numerator r)
    show (N r) = show (numerator r) ++ "/" ++ show (denominator r)
    show p@(P _ a b) = 
        case valToText p of
            Just txt -> (T.unpack . txtLit) txt
            Nothing -> "(" ++ show a ++ "*" ++ show b ++ ")"
    show (L a) = "(" ++ show a ++ "+_)"
    show (R b) = "(_+" ++ show b ++ ")"
    show U     = "u"
    show (S t v) = show v ++ "{$" ++ T.unpack t ++ "}"
    show (B kf abc) = "[" ++ show abc ++ "]" ++ rel ++ aff
        where rel = if may_copy kf then "" else "k"
              aff = if may_drop kf then "" else "f"

