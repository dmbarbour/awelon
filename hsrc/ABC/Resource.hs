{-# LANGUAGE ViewPatterns #-}

-- | ABC has a non-conventional approach to separate compilation and
-- linking: ABC resources are given deterministic, cryptographically
-- unique names using secure hash of the bytecode, and ABC can invoke
-- these resources by name to logically inline the bytecode. 
-- 
-- To support integration with content distribution networks, ABC 
-- resources are encrypted. The decryption key is the secure hash of
-- the bytecode. The full name includes both the decryption key and
-- the secure hash of the cipher text to quickly look up the resource.
-- 
-- The full ABC invocation looks like:
-- 
--      {#secureHashOfCiphertext:secureHashOfBytecode}
-- 
-- Pseudocode for resource construction:
-- 
--      given bytecode
--      encryptionKey = secureHashBC(bytecode)
--      cipherText = encrypt(compress(bytecode),encryptionKey)
--      lookupKey = secureHashCT(cipherText)
--      store(lookupKey,cipherText)
--      resourceId = lookupKey:encryptionKey
--      return resourceId
-- 
-- Pseudocode for resource acquisition:
-- 
--      given resourceId
--      extract lookupKey, encryptionKey from resourceId
--      cipherText = fetch(lookupKey)
--      validate(lookupKey == secureHashCT(cipherText))
--      bytecode = decompress(decrypt(cipherText),encryptionKey)
--      validate(encryptionKey == secureHashBC(bytecode))
--      validateABC(bytecode)
--      return bytecode
-- 
-- Separate compilation is opportunistic. We expect reuse of names,
-- so we can cache and compile the bytecode associated with a given
-- resourceId.
--
-- Not all details are settled. Some relatively stable decisions:
-- 
--      secureHash CT,BC: are independent halves of SHA3-384
--      base64url encoding of secure hashes in resource ID
--      AES encryption, CTR mode, nonce simple function of key
--      simple, deterministic, unambiguous compression algorithm
-- 
-- This module implements the standard resource model as far as it
-- has been implemented, stubbing the elements that haven't been 
-- decided yet.
-- 
module ABC.Resource 
    ( HashBC, HashCT
    , CipherText, ResourceToken
    , secureHashBC, secureHashCT
    , makeResource, loadResource
    , abcResourceToken
    ) where

import Data.Functor.Identity
import qualified Data.List as L
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as B
import qualified Data.Byteable as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Read as R
import qualified Crypto.Hash as CH
import qualified Data.ByteString.Base64.URL as B64
import ABC.Operators

-- | HashCT and HashBC are 24 octet bytestrings
type HashBC = ByteString
type HashCT = ByteString

-- | encrypted, compressed bytecode
type CipherText = ByteString

-- | A resource token is the text that goes between curly braces
-- for invocation in ABC. I.e. in this case, it has the form:
--      #secureHashOfCiphertext:secureHashOfBytecode
-- with the secure hashes encoded in base64url.
type ResourceToken = String


-- | generate secure hash for a series of operations.
-- (second half of SHA3-384 on UTF-8 encoding of ABC)
secureHashBC :: [Op] -> HashBC
secureHashBC = secureHashBC' . encodeABC

-- | encode ABC as a bytestring
encodeABC :: [Op] -> ByteString
encodeABC = T.encodeUtf8 . T.pack . show

-- | decode bytestring as ABC program
decodeABC :: ByteString -> Maybe [Op]
decodeABC bcBytes = 
    case T.decodeUtf8' bcBytes of
        Left _ -> Nothing
        Right txt -> 
            let s = T.unpack txt in
            case R.readList s of
                [(ops,"")] -> Just ops
                _ -> Nothing 

-- generate secure hash for the bytecode from bytestring
secureHashBC' :: ByteString -> HashBC
secureHashBC' = B.drop 24 . sha3_384

-- | secure hash for the ciphertext (used as lookup key)
-- (first half of SHA3-384)
secureHashCT :: CipherText -> HashCT
secureHashCT = B.take 24 . sha3_384 

-- | given a storage function and resource, create the ABC resource
-- and return deterministic, unique resource token. This token can
-- later be used with loadResource to obtain the bytecode.
makeResource :: (Monad m) => (HashCT -> CipherText -> m ()) -> [Op] -> m ResourceToken
makeResource fnStore bytecode =
    let bcBytes = encodeABC bytecode in
    let hashBC = secureHashBC' bcBytes in
    let cipherText = encrypt hashBC (compress bcBytes) in
    let hashCT = secureHashCT cipherText in
    fnStore hashCT cipherText >>
    let resourceId = "#" ++ toBase64 hashCT ++ ":" ++ toBase64 hashBC in
    return resourceId

-- | purely compute the resource token without storing the resource
abcResourceToken :: [Op] -> ResourceToken
abcResourceToken = runIdentity . makeResource nullStore where
    nullStore _hashCT _cipherText = return ()

-- | given a resource loading function, access an ABC resource via
-- token. In unlikely case of HashCT collisions, multiple candidate
-- cipher texts can be returned. The bytecode hash will provide an
-- additional uniqueness filter.
--
-- loadResource will validate that all hashes are matched and that
-- the bytecode can parse as ABC. It will fail if no valid resources
-- are loaded, or (very improbably!) in case of full 384-bit secure
-- hash collision. 
--
loadResource :: (Monad m) => (HashCT -> m [CipherText]) -> ResourceToken -> m [Op]
loadResource fnLoad tok@(splitToken -> Just (hashCT,hashBC)) =
    fnLoad hashCT >>= \ lLoadedTexts -> 
    let lMatchHashCT = L.filter ((== hashCT) . secureHashCT) lLoadedTexts in
    let lbcBytes = L.nub $ fmap (decompress . decrypt hashBC) lMatchHashCT in
    let lMatchHashBC = L.filter ((== hashBC) . secureHashBC') lbcBytes in
    let lOps = mapMaybe decodeABC lMatchHashBC in
    case lOps of
        [] -> fail ("ABC resource " ++ tok ++ " not found") -- e.g. network failure
        [ops] -> return ops
        _ -> fail ("ABC resource " ++ tok ++ " is ambiguous") -- secure hash collision!
loadResource _ tok = fail $ "invalid resource token: " ++ tok

-- extract information from a resource token 
splitToken :: ResourceToken -> Maybe (HashCT, HashBC)
splitToken ('#':rscid) =
    let (rct,crbc) = L.splitAt 32 rscid in
    case crbc of
        (':':rbc) ->
            case (fromBase64 rct, fromBase64 rbc) of
                (Just hct, Just hbc) ->
                    -- ensure 192 bits for each hash
                    let okSize = (24 == B.length hct) && (24 == B.length hbc) in
                    if okSize then Just (hct,hbc) else Nothing
                _ -> Nothing
        _ -> Nothing -- not a valid token
splitToken _ = Nothing
    
-- the type declaration selects the hash function (yuck)
sha3_384 :: ByteString -> ByteString
sha3_384 = B.toBytes . sha3_384'

sha3_384' :: ByteString -> CH.Digest CH.SHA3_384
sha3_384' = CH.hash

toBase64 :: ByteString -> String
toBase64 = fmap toChar . B.unpack . B64.encode where
    toChar = toEnum . fromIntegral

fromBase64 :: String -> Maybe ByteString
fromBase64 = e2mb . B64.decode . T.encodeUtf8 . T.pack where
    e2mb = either (const Nothing) Just


-- todo: implement encryption
encrypt :: HashBC -> ByteString ->  CipherText
encrypt _key = id

-- todo: implement decryption
decrypt :: HashBC -> CipherText -> ByteString
decrypt _key = id

-- todo: implement compression
compress :: ByteString -> ByteString
compress = id

-- todo: implement decompression
decompress :: ByteString -> ByteString
decompress = id
