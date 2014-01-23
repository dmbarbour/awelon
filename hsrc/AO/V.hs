{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- dynamic AO/ABC values
module AO.V
    ( V(..), Block(..), block, Op(..), ABC(..), inABC
    , ToABCV(..), toABCVL, FromABCV(..), fromABCVL
    , valToText, textToVal, abcLit, quoteNum
    ) where

import Control.Applicative
import Data.Ratio
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Word as W

data V -- ABC's structural types
    = L V        -- sum left
    | R V        -- sum right
    | N Rational -- number
    | P V V      -- product
    | B Block    -- block
    | U          -- unit
    | S Text V   -- sealed value (via sealer capability)
    deriving (Eq)
data Block = Block 
    { b_aff  :: Bool
    , b_rel  :: Bool
    , b_code :: ABC
    } deriving (Eq)
data Op
    = Op Char     -- a normal operator
    | Qu V        -- quoted values and literals (e → v*e)
    | Invoke Text -- {invocation}
    | AMBC [ABC]  -- AMBC extension, set of options
    deriving (Eq) -- structural equality
newtype ABC = ABC [Op] deriving (Eq)

inABC :: ABC -> [Op]
inABC (ABC ops) = ops

block :: ABC -> Block 
block abc = Block { b_aff = False, b_rel = False, b_code = abc }

-- showABC :: ABC -> Text
showABC :: ABC -> Text
showABC (ABC code) = T.concat $ map showOp code

showOp :: Op -> Text
showOp (Op c) = T.singleton c
showOp (Qu v) = abcLit v
showOp (Invoke text) = '{' `T.cons` text `T.snoc` '}'
showOp (AMBC opts) = 
    let opTexts = map showABC opts in
    let sepOpts = T.intercalate (T.singleton '|') opTexts in
    '(' `T.cons` sepOpts `T.snoc` ')'

-- show a value as ABC code
--   value v converted to ABC of type e → v*e
abcLit :: V -> Text
abcLit (N r) = T.pack $ quoteNum r
abcLit U = T.pack "vvrwlc" -- intro1, not most efficient
abcLit (B b) = 
    let wk = if (b_rel b) then (`T.snoc` 'k') else id in
    let wf = if (b_aff b) then (`T.snoc` 'f') else id in
    let ctxt = showABC (b_code b) in
    wf $ wk $ '[' `T.cons` ctxt `T.snoc` ']'
abcLit (L v) = abcLit v `T.snoc` 'V'
abcLit (R v) = abcLit v `T.append` intro0 where
    intro0 = T.pack "VVRWLC"
abcLit (S t v) = abcLit v `T.append` sealer where
    sealer = '{' `T.cons` '$' `T.cons` t `T.snoc` '}'
abcLit v@(P a b) =
    case valToText v of
        Just txt -> txtLit txt
        Nothing -> 
            abcLit a `T.append` abcLit b 
            `T.snoc` 'w' `T.snoc` 'l'

txtLit :: Text -> Text
txtLit = wrap . escapeLines . toLines where
    toLines = T.splitOn (T.singleton '\n')
    escapeLines = T.intercalate (T.pack "\n ")
    wrap = (T.cons '"') . (`T.snoc` '~') . (`T.snoc` '\n')

-- given a number, find code that generates it as a literal
quoteNum :: Rational -> [Char]
quoteNum r =
    if (r < 0) then quoteNum (negate r) ++ "-" else
    let num = numerator r in
    let den = denominator r in
    if (1 == den) then (quoteNat [] num) else
    if (1 == num) then (quoteNat "/" den) else
    quoteNat (quoteNat "/*" den) num 

-- build from right to left (positive integer required)
quoteNat :: [Char] -> Integer -> [Char]
quoteNat code n =
    if (0 == n) then ('#' : code) else
    let (q,r) = n `divMod` 10 in
    quoteNat (iop r : code) q

iop :: Integer -> Char 
iop n | (0 <= n && n <= 9) = (toEnum (48 + fromIntegral n)) 
      | otherwise = error "iop expects argument between 0 and 9"

--
-- HASKELL / ABC INTEGRATION
--
-- (a) conversion functions for values (ToABCV, FromABCV)
-- (b) need a useful invoker for bootstrap purposes
--
class ToABCV v where toABCV :: v -> V
instance ToABCV V where toABCV = id
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
instance ToABCV Block where toABCV = B

-- generate list terminated by number
toABCVL :: (ToABCV a) => Rational -> [a] -> V
toABCVL vf [] = (N vf)
toABCVL vf (v:vs) = toABCV v `P` toABCVL vf vs

-- read list terminated by specific number
fromABCVL :: (FromABCV a) => Rational -> V -> Maybe [a]
fromABCVL rF (N rF') | rF == rF' = Just []
fromABCVL rF (P a la) = (:) <$> fromABCV a <*> fromABCVL rF la
fromABCVL _ _ = Nothing

class FromABCV v where fromABCV :: V -> Maybe v
instance FromABCV V where  fromABCV = Just
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
instance FromABCV Block where
    fromABCV (B b) = Just b
    fromABCV _ = Nothing

instance Show ABC where 
    show = T.unpack . showABC

instance Show Op where 
    show op = show (ABC [op])

instance Show V where 
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
    show (S t v) = show v ++ "{$" ++ T.unpack t ++ "}"
    show (B b) = "[" ++ show (b_code b) ++ "]" ++ rel ++ aff
        where rel = if b_rel b then "k" else ""
              aff = if b_aff b then "f" else ""


valToText :: V -> Maybe Text
valToText = fromABCV

textToVal :: Text -> V
textToVal = toABCV
