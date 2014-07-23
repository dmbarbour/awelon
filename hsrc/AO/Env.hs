
-- | environment variables, paths and directories
module AO.Env
    ( getAO_TEMP
    , getAO_PATH
    , getRscDir
    , getJitDir
    ) where

import Control.Applicative
import Data.Either
import qualified Data.Text as T
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified System.Environment as Env
import qualified System.IO.Error as Err

getEnvDefault :: String -> String -> IO String
getEnvDefault var defaultVal = withDefault <$> getVar where
    withDefault = either (const defaultVal) id 
    getVar = Err.tryIOError (Env.getEnv var)

-- (idempotent) obtain (and create) the AO_TEMP directory
-- may raise an IOError based on permissions or similar
getAO_TEMP :: IO FS.FilePath
getAO_TEMP = 
    getEnvDefault "AO_TEMP" "aotmp" >>= \ d0 ->
    let fp0 = FS.fromText (T.pack d0) in
    FS.createTree fp0 >>
    canonicalizeDirPath fp0

-- (idempotent) obtain (and create) the ABC ciphertext resource 
-- storage directory. This will serve as the primary location for
-- resources until we upgrade to a proper database. 
getRscDir :: IO FS.FilePath
getRscDir =  
    getAO_TEMP >>= \ aoTmp ->
    let rscDir = aoTmp FS.</> FS.fromText (T.pack "rsc") in
    FS.createDirectory True rscDir >>
    return rscDir

-- | (idempotent) obtain or create the JIT directory
getJitDir :: IO FS.FilePath
getJitDir =
    getAO_TEMP >>= \ aoTmp ->
    let jitDir = aoTmp FS.</> FS.fromText (T.pack "jit") in
    FS.createDirectory True jitDir >>
    return jitDir

-- | obtain valid filesystem paths from AO_PATH environment variable
-- (defaults empty; does not use current path)
getAO_PATH :: IO [FS.FilePath]
getAO_PATH = 
    getEnvDefault "AO_PATH" "" >>= \ sps ->
    let paths = FS.splitSearchPathString sps in
    let canonize = Err.tryIOError . canonicalizeDirPath in
    rights <$> mapM canonize paths

-- canonizalize + assert isDirectory (may fail with IOError)
canonicalizeDirPath :: FS.FilePath -> IO FS.FilePath 
canonicalizeDirPath fp = 
    FS.canonicalizePath fp >>= \ cfp ->
    FS.isDirectory cfp >>= \ bDir ->
    if bDir then return cfp else -- success case
    let emsg = show cfp ++ " is not a directory." in
    let etype = Err.doesNotExistErrorType in
    Err.ioError $ Err.mkIOError etype emsg Nothing (Just (show fp))
    
