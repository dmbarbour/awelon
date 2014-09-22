{-# LANGUAGE ViewPatterns #-}

-- | ABC has a standard compression algorithm for ABC resources. It
-- is also used for cipher texts (with value sealing) and many ABC
-- streams. The format is an adaptation of optimal LZSS parsing. It
-- is modified to mitigate worst-case expansion of non-compressible
-- data. While plain old ABC code should compress very nicely, the
-- embedded binaries, cipher texts, and resource IDs will not.
--
-- Compression involves three simple, independent, streamable passes: 
--   base16 compression (see ABC.Base16)
--   phrase-optimal LZSS compression (offset 1..2047, size 3..34)
--   literal aggregation (8 phrase groups, each of 8 phrases)
--
-- This compression can be streamed, and uses a predictable amount
-- of memory to encode or decode. The worst case expansion is 2.4%
-- for large embedded binaries (relative to a raw binary encoding).
-- Pure ABC should compress very nicely.
--
-- Addending zeroes to the compressed binary will not affect its
-- meaning. This is useful if compression is applied before block
-- encryption - add as many trailing zeroes as you need.
--
-- Large scale compression in Awelon systems is achieved by reuse of 
-- ABC resources (cf. ABC.Resource), and eventually by ABCD. 
--
module ABC.Compress
    ( compress
    , decompress
    ) where

import Control.Exception (assert)
import Data.Word
import qualified Data.List as L
import qualified ABC.Base16 as B16

compress :: [Word8] -> [Word8]
compress = pg_encode . pg_aggr . lzss_compress . B16.compress

decompress :: [Word8] -> [Word8]
decompress = B16.decompress . lzss_decompress . pg_deaggr . pg_decode

-- singular LZSS phrases (literal or match)
data LZSS_Phrase
    = Lit {-# UNPACK #-} !Word8
    | Match {-# UNPACK #-} !Offset {-# UNPACK #-} !Size
    deriving Show
type Offset = Word16 -- 1..2047 (or 0 for end of input)
type Size = Word8 -- 3..34

-- aggregated groups of phrases
data PG
    = LG [Word8]        -- literal group
    | MG [LZSS_Phrase]  -- mixed group (at least one match)
    deriving Show

-- encode
pg_encode :: [PG] -> [Word8]
pg_encode [] = []
pg_encode pgs = pg_encode8 pg8 ++ pg_encode pgs' where
    (pg8,pgs') = L.splitAt 8 pgs

-- encode eight or fewer phrases
pg_encode8 :: [PG] -> [Word8]
pg_encode8 pgs = pg_flags pgs : L.concat (fmap pg_encode1 pgs)

-- compute PG flags
pg_flags :: [PG] -> Word8
pg_flags = toFlagsByte . fmap pg_flag1 where
    pg_flag1 (LG _) = True
    pg_flag1 (MG _) = False

-- encode a single phrase group. Literals are encoded without any 
-- additional flags, which reduces the overhead down to 1:64. 
pg_encode1 :: PG -> [Word8]
pg_encode1 (LG bytes) = bytes
pg_encode1 (MG lzps)  = mg_encode8 lzps

mg_encode8 :: [LZSS_Phrase] -> [Word8]
mg_encode8 mgs = mg_flags mgs : L.concat (fmap mg_encode1 mgs)

mg_flags :: [LZSS_Phrase] -> Word8
mg_flags = toFlagsByte . fmap mg_flag1 where
    mg_flag1 (Lit _) = True
    mg_flag1 (Match _ _) = False

mg_encode1 :: LZSS_Phrase -> [Word8]
mg_encode1 (Lit w) = [w]
mg_encode1 (Match offset size) | (validMatch offset size) =
    let n = (toInteger offset * 32) + (toInteger size - 3) in
    let (upper,lower) = n `divMod` 256 in
    [fromIntegral upper, fromIntegral lower]
mg_encode1 v = error ("invalid LZSS phrase: " ++ show v)

-- a valid match must fit the 11+5 bit encoding. A zero offset
-- match may signal that the encoding is finished.
validMatch :: Offset -> Size -> Bool
validMatch offset size = validOffset && validSize where
    validOffset = (1 <= offset) && (offset <= 2047) -- 11 bits; 0 invalid
    validSize = (3 <= size) && (size <= 34) -- 5 bits; add 3

pg_decode :: [Word8] -> [PG]
pg_decode [] = []
pg_decode (fByte:bs) =
    let (pg8,bs') = pg_decode_f [] (toFlagBits fByte) bs in
    pg8 ++ pg_decode bs'

-- accumulate phrase groups according to a list of PG flag bits
pg_decode_f :: [PG] -> [Bool] -> [Word8] -> ([PG],[Word8])
pg_decode_f r [] bs = (L.reverse r, bs)
pg_decode_f r (True:fs') bs = pg_decode_f (LG w8 : r) fs' bs' where
    w8  = L.take 8 $ bs ++ L.repeat 0
    bs' = L.drop 8 bs 
pg_decode_f r (False:fs') bs = pg_decode_f (MG lzps : r) fs' bs' where
    (lzps,bs') = mg_decode8 bs

mg_decode8 :: [Word8] -> ([LZSS_Phrase],[Word8])
mg_decode8 [] = ([],[]) -- end of input
mg_decode8 (fByte:bs) = mg_decode_f [] (toFlagBits fByte) bs

-- accumulate LZSS phrases according to a list of LZSS flag bits
mg_decode_f :: [LZSS_Phrase] -> [Bool] -> [Word8] -> ([LZSS_Phrase],[Word8])
mg_decode_f r [] bs = (L.reverse r, bs)
mg_decode_f r (True:fs') (w:bs') = mg_decode_f (Lit w : r) fs' bs' 
mg_decode_f r (False:fs') (w1:w2:bs') = mg_decode_f (match w1 w2 : r) fs' bs'
mg_decode_f r fs bs = mg_decode_f r fs (bs ++ [0]) -- may extend data with zeroes

match :: Word8 -> Word8 -> LZSS_Phrase
match 0 0 = Match 0 0 -- special case for decode (to fill end of input)
match w1 w2 = 
    let w16 = (toInteger w1 * 256) + toInteger w2 in
    let (offset,sizeBits) = w16 `divMod` 32 in
    let size = sizeBits + 3 in
    assert (0 /= offset) $ 
    Match (fromIntegral offset) (fromIntegral size)

-- a flags byte is ordered such that the lowest bit corresponds
-- to the first element, and the highest bit corresponds to the
-- last element. Bits are encoded from booleans, 1 is true, 0 is
-- false. If there are fewer than eight elements, we'll encode
-- the upper bits as 0, so it must be suitable as the default.
toFlagsByte :: [Bool] -> Word8
toFlagsByte = L.foldr addBit 0 . L.take 8 where
    addBit b s = (s * 2) + fb b
    fb False = 0
    fb True  = 1

toFlagBits :: Word8 -> [Bool]
toFlagBits = wb 8 where
    wb :: Int -> Word8 -> [Bool] 
    wb 0 _ = []
    wb n w = let (q,r) = w `divMod` 2 in (r /= 0) : wb (n - 1) q

-- test if we have eight literals in a row...
l8 :: [LZSS_Phrase] -> Maybe ([Word8],[LZSS_Phrase])
l8 ( (Lit w1) : (Lit w2) : (Lit w3) : (Lit w4) :
     (Lit w5) : (Lit w6) : (Lit w7) : (Lit w8) : lzps ) =
    Just ([w1,w2,w3,w4,w5,w6,w7,w8], lzps)
l8 _ = Nothing

-- group phrases eight at a time. 
pg_aggr :: [LZSS_Phrase] -> [PG]
pg_aggr [] = []
pg_aggr (l8 -> Just (ws,lzps')) = LG ws : pg_aggr lzps'
pg_aggr lzps = MG phrases : pg_aggr lzps' where
    (phrases,lzps') = L.splitAt 8 lzps

-- flatten the phrase groups back into LZSS
pg_deaggr :: [PG] -> [LZSS_Phrase]
pg_deaggr [] = []
pg_deaggr ((LG ws):pgs) = (fmap Lit ws) ++ pg_deaggr pgs
pg_deaggr ((MG ps):pgs) = ps ++ pg_deaggr pgs


-- For LZSS compression, I need a history




-- placeholder LZSS compression 
lzss_compress :: [Word8] -> [LZSS_Phrase]
lzss_compress = fmap Lit

-- placeholder LZSS decompression
lzss_decompress :: [LZSS_Phrase] -> [Word8]
lzss_decompress [] = []
lzss_decompress (Lit w : lzps) = w : lzss_decompress lzps
lzss_decompress (Match 0 _ : _ ) = [] -- end of input due to extra zeroes
lzss_decompress (Match _ _ : _ ) = error $ 
    "TODO: finish implementing LZSS compression and decompression"



