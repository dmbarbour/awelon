
-- Easy precompilation, using filesystem as the resource database. 
-- 
-- Only saves the cipher texts. Thus, should behave similarly to
-- downloading resources from the network.
--
module Precompile
    ( precompile -- save resources to filesystem
    , getRscDir, hctFile -- where to find the resources
    ) where

import Control.Applicative 
import Control.Monad
import qualified Control.Exception as Err
import Control.Concurrent

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString as B

import qualified System.IO as Sys
import qualified System.Environment as Env
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS

import qualified Codec.Binary.Base32 as B32

import AO.Dict
import AO.Precompile

-- (idempotent) get directory for ciphertext resources
getRscDir :: IO FS.FilePath
getRscDir = 
    getAO_TEMP >>= \ aoTmp ->
    let rscTmpDir = aoTmp FS.</> FS.fromText (T.pack "rsc") >>
    FS.createDirectory True rscTmpDir >>
    return rscTmpDir

-- (idempotent) obtain (and create) the AO_TEMP directory
-- may raise an IOError based on permissions or similar
getAO_TEMP :: IO FS.FilePath
getAO_TEMP = 
    (maybe "aotmp" id <$> tryJust (Env.getEnv "AO_TEMP")) >>= \ d0 ->
    let fp0 = FS.fromText (T.pack d0) in
    FS.createTree fp0 >>
    FS.canonicalizePath fp0

try :: IO a -> IO (Either Err.SomeException a)
try = Err.try -- type forced

tryJust :: IO a -> IO (Maybe a)
tryJust op = either (const Nothing) (Just) <$> try op

-- precompile a dictionary and emit resources to filesystem
precompile :: AODict md -> IO (AODict md)
precompile d0 =
    let (df,(_,secd)) = preCompileDict d0 in
    getRscDir >>= \ rscDir ->
    mapM_ (saveSecD rscDir) (M.toList secd) >>
    return df

hctFile :: HashCT -> FS.FilePath
hctFile = FS.fromText . T.pack . B32.encode . B.unpack 

-- save ABC resources to external storage (in filesystem)
saveSecD :: FS.FilePath -> (HashCT,[CipherText]) -> IO ()
saveSecD _ _ [] = return ()
saveSecD rscDir hct (ct:cts) =
    let fullPath = rscDir FS.</> hctFile hct in
    FS.writeFile fullPath ct >>
    unless (null cts) (putErrLn (ambMsg hct))

ambMsg :: HashCT -> String
ambMsg hct = 
    "ambiguous resource (192-bit hash collision!) at " ++
    FS.encodeString (hctFile hct)

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr
