
-- | ABC has a non-conventional approach to separate compilation and
-- linking: an external ABC resource is named by secure hash of its
-- bytecode, i.e. `{#secureHashOfResource}`. The hash in question is
-- the SHA3-384, encoded via base64url. 
--
-- The bytecode `{#secureHashOfResource}` will apply the identified
-- ABC resource to the tacit value. Of course, this may require the
-- runtime to download the resource, and serves as an opportunity to
-- separately typecheck, compile, and cache the resource for reuse.
-- Careful use of secure hash resources can mitigate the otherwise 
-- exponential size of ABC streams, reducing memory and cache burden
-- and potentially saving bandwidth in a distributed stream. 
--
-- Anyhow, hashing ABC code comes up often enough, and in a standard
-- manner to support linking. This module provides the function for it.
module ABC.Hash (abcHash) where

import qualified Data.ByteString as B
import qualified Data.Byteable as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Crypto.Hash as CH
import qualified Data.ByteString.Base64.URL as B64
import ABC.Operators

-- | abcHash takes an ABC sequence and returns a base64url string...
-- which will have length 64 characters. 
abcHash :: [Op] -> String
abcHash = toBase64 . B.toBytes . sha3_384 . T.encodeUtf8 . T.pack . show

-- the type declaration selects the hash function (yuck)
sha3_384 :: B.ByteString -> CH.Digest CH.SHA3_384
sha3_384 = CH.hash

toBase64 :: B.ByteString -> String
toBase64 = fmap toChar . B.unpack . B64.encode where
    toChar = toEnum . fromIntegral

