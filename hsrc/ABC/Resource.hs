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
    ( CipherText, ResourceToken
    , SaveMethod, LoadMethod
    , makeResource, loadResource
    , abcResourceToken
    -- miscellaneous
    , HashCT, HashBC
    , secureHashCT, secureHashBC, secureHashBC'
    , SecureHash, secureHash
    , encodeABC, decodeABC
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

-- | HashCT and HashBC are 192 bit (24 octet) strings
--
-- Concretely:
--   HashCT is first  192 bits of SHA3-384 of the ciphertext.
--   HashBC is second 192 bits of SHA3-384 of the bytecode (as UTF-8)
--
-- Independent halves of SHA3-384 ensures independence of hash values
-- without relying on quality of encryption or compression. The total
-- 384 bits from HashCT and HashBC contribute to the unique identity
-- of a ResourceToken.
-- 
type HashBC = ByteString
type HashCT = ByteString

-- | SecureHash is simply the full SHA3-384 value (48 octets)
type SecureHash = ByteString

-- | Cipher text in this case is compressed and encrypted bytecode.
-- 
-- At the moment, compression and encryption algorithms is not yet 
-- decided, and this is simply identical to the bytecode. But that
-- won't always hold.
type CipherText = ByteString

-- | A resource token is the text that goes between curly braces
-- for invocation in ABC. I.e. in this case, it has the form:
--      #secureHashOfCiphertext:secureHashOfBytecode
-- with the secure hashes encoded in base64url.
type ResourceToken = String

-- | Store a ciphertext somewhere, to be later looked up by secure
-- hash. The full secure hash is given here just to avoid local
-- redundant computation. In some situations, it may be appropriate
-- to validate the secure hash for security or integrity reasons.
type SaveMethod m = SecureHash -> CipherText -> m ()

-- | Load a ciphertext given HashCT, i.e. the first half of the full
-- SHA3-384 SecureHash of the ciphertext. In the unlikely case of a
-- 192-bit hash collision, or for some simple storage models, we may
-- return multiple candidate texts. 
type LoadMethod m = HashCT -> m [CipherText]

-- | generate secure hash for a series of operations.
-- (second half of SHA3-384 on UTF-8 encoding of ABC)
secureHashBC :: [Op] -> HashBC
secureHashBC = secureHashBC' . encodeABC

-- | encode ABC as a UTF-8 bytestring
encodeABC :: [Op] -> ByteString
encodeABC = T.encodeUtf8 . T.pack . show

-- | decode bytestring as ABC program (or fail returning Nothing)
decodeABC :: ByteString -> Maybe [Op]
decodeABC bcBytes = 
    case T.decodeUtf8' bcBytes of
        Left _ -> Nothing
        Right txt -> 
            let s = T.unpack txt in
            case R.reads s of
                [(ops,"")] -> Just ops
                _rl -> Nothing

-- | generate secure hash for the bytecode from bytestring
-- eqv. to `drop 24 . secureHash`
secureHashBC' :: ByteString -> HashBC
secureHashBC' = B.drop 24 . secureHash

-- | secure hash for the ciphertext (used as lookup key)
-- first half of SHA3-384, eqv. to `take 24 . secureHash`
secureHashCT :: CipherText -> HashCT
secureHashCT = B.take 24 . secureHash

-- | generate a complete secure hash (SHA3-384, as 48 octets)
secureHash :: ByteString -> SecureHash
secureHash = sha3_384

-- | given a storage method and an ABC subprogram, create the ABC 
-- resource and return a deterministic, unique resource token. 
--
makeResource :: (Monad m) => (SaveMethod m) -> [Op] -> m ResourceToken
makeResource fnSave bytecode = saveCode >> return rscTok where
    bcBytes = encodeABC bytecode
    hashBC = secureHashBC' bcBytes
    cipherText = encrypt hashBC (compress bcBytes)
    fullHashCT = secureHash cipherText
    saveCode = fnSave fullHashCT cipherText
    hashCT = B.take 24 fullHashCT
    rscTok = "#" ++ toBase64 hashCT ++ ":" ++ toBase64 hashBC

-- | purely compute the resource token without storing the resource
abcResourceToken :: [Op] -> ResourceToken
abcResourceToken = runIdentity . makeResource nullStore where
    nullStore _hashCT _cipherText = return ()

-- | given a load method and a full resource token, obtain a single
-- resource that should be cryptographically unique. Or `fail` if 
-- no such resource can be loaded. Also performs a minimal, shallow
-- validation that the hashes match and ABC parses.
loadResource :: (Monad m) => (LoadMethod m) -> ResourceToken -> m [Op]
loadResource fnLoad tok@(splitToken -> Just (hashCT,hashBC)) =
    fnLoad hashCT >>= \ lCandidate -> 
    let lMatchHashCT = L.filter ((== hashCT) . secureHashCT) lCandidate in
    let lbcBytes = L.nub $ mapMaybe (decompress . decrypt hashBC) lMatchHashCT in
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
encrypt :: HashBC -> ByteString -> CipherText
encrypt _key = id

-- todo: implement decryption
decrypt :: HashBC -> CipherText -> ByteString
decrypt _key = id

-- todo: implement compression
compress :: ByteString -> ByteString
compress = id

-- todo: implement decompression
decompress :: ByteString -> Maybe ByteString
decompress = Just
