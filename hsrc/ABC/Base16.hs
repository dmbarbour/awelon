{-# LANGUAGE ViewPatterns, PatternGuards #-}
-- | Awelon Bytecode is pursuing a rather non-conventional approach
-- to efficient embedding of binary data for storage and transport.
--
-- The idea is that we do not need efficient representation at the
-- ABC layer. Indeed, we can naively encode binary data in base16.
-- In practice, we'll compress ABC resources and most ABC streams on
-- the network. It is not difficult to recognize a long sequence of
-- base16 and compress it to binary, i.e. a specialized compression
-- pass (applied prior to byte-level compression).
--
-- This module provides the alphabet, encoding and decoding binaries, 
-- and a compressor and decompressor. The alphabet in this case is
-- not conventional - `bdfghjkmnpqstxyz` - to mitigate interference with
-- compressing the rest of ABC, and to avoid spelling offensive words.
--
-- This enables efficient storage and transport of binary information
-- despite embedding in a UTF-8 textual medium. It far more efficent
-- than Base64, having less than 1% overhead for large binaries. The
-- break-even with Base64 is encoding 6 bytes. Further, base16, even
-- with the unusual alphabet, is relatively simple for conversions.
--
-- In the compressed form, we have:
--
--   * header byte 0xF8
--   * length byte L encoding 3..256 (as 0..253 + 3)
--   * followed by L raw bytes 
--
-- Decompressed form is a sequence of 6..512 base16 characters (always
-- an even number). 
--
-- In case we encounter 0xF8 in the raw stream, we'll escape it by 
-- following with 0xFE. However, this is just to ensure compression is
-- well defined for all byte streams. In practice, we're compressing a
-- UTF-8 stream, and UTF-8 does not use 0xC0, 0xC1, or 0xF5..0xFF. Thus
-- we should never encounter the 0xF8 in the original stream. 
-- 
module ABC.Base16 
    ( alphabet
    , encode
    , decode
    , compress
    , decompress
    ) where

import Control.Exception (assert)
import Control.Applicative
import Data.Word
import qualified Data.List as L

-- | The alphabet for embedding Base16 in ABC is not conventional.
-- Instead of 0-9 A-F, we use the lower case alphabet minus vowels
-- and most ABC data plumbing operators (vrwlc). This mitigates risk
-- of spelling offensive words, and limits interference with other
-- compression for the non-binary elements.
alphabet :: String
alphabet = "bdfghjkmnpqstxyz"

alph8 :: [Word8]
alph8 = fmap (fromIntegral . fromEnum) alphabet

-- given a character that should be in the alphabet, 
-- translate it to a number in range 0..15.
h2n :: Word8 -> Maybe Word8
h2n c = fromIntegral <$> L.elemIndex c alph8

-- | Given raw binary, translate into alphabet.
encode :: [Word8] -> [Word8]
encode (b:bs) = h1 : h2 : encode bs where
    (n1,n2) = b `divMod` 16
    h1 = alph8 !! fromIntegral n1
    h2 = alph8 !! fromIntegral n2
encode [] = []

-- | Given a binary consisting of ABC base16 alphabet
-- elements, translate to binary. The second bytestring
-- returns the first data that could not be decoded.
decode :: [Word8] -> ([Word8],[Word8])
decode = decode' []

decode' :: [Word8] -> [Word8] -> ([Word8],[Word8])
decode' dbs ((h2n -> Just h1) : (h2n -> Just h2) : bs) = decode' dbs' bs where
    dbs'    = newByte : dbs 
    newByte = h1*16 + h2
decode' dbs bs = (L.reverse dbs, bs)


-- | Compression always works. Given a binary that may contain some
-- *embedded* base16, it will compress those embedded base16 sequences.
--
-- Existing header bytes (0xF8), however, will be expanded to two bytes. 
-- (But this won't happen for compressing UTF-8 encoded text.)
compress :: [Word8] -> [Word8]
compress (0xF8 : bs) = (0xF8 : 0xFE : compress bs)
compress (tkhx -> (n,hbs,bs)) | (n >= 3) = 
    assert (n <= 256) $ (0xF8 : fromIntegral (n - 3) : hbs) ++ compress bs
compress (b:bs) = (b : compress bs)
compress [] = []

-- try to take up to 256 hexadecimal-encoded bytes from the stream
tkhx :: [Word8] -> (Int, [Word8], [Word8])
tkhx = tkhx' 0 []

tkhx' :: Int -> [Word8] -> [Word8] -> (Int, [Word8], [Word8])
tkhx' n hbs bs | (n >= 256) = (n, L.reverse hbs, bs) -- maximum extraction
tkhx' n hbs ((h2n -> Just h1) : (h2n -> Just h2) : bs') = tkhx' n' hbs' bs' where
    hbs' = hb:hbs
    hb = h1*16 + h2
    n' = n + 1
tkhx' n hbs bs = (n, L.reverse hbs, bs) -- could not extract a byte

-- | Decompress a byte stream, expanding embedded base16. 
--
-- Note: this implementation will succeed for all inputs, even the
-- invalid ones (e.g. 0xF8 0xFF, or not enough bytes in list). 
decompress :: [Word8] -> [Word8]
decompress (0xF8 : n : bs) | (n > 253) = (0xF8 : decompress bs)
                           | otherwise = 
    let (hbs,bs') = L.splitAt ((fromIntegral n)+3) bs in
    encode hbs ++ decompress bs'
decompress (b : bs) = b : decompress bs
decompress [] = []

