{-# LANGUAGE CPP #-}

-- | This module is responsible for loading '.ao' files from the
-- filesystem, reading AO_PATH, and generally doing file IO. 
module AO.LoadAO 
    ( getAO_PATH, getAO_DICT, importText, importDictFile, importDictFiles
    ) where

import Control.Applicative
import Control.Monad

import Data.Either 
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified System.Environment as Env
import qualified System.IO.Error as Err
import qualified System.IO as Sys

import AO.AOTypes
import AO.ParseAO

-- OS-dependent AO_PATH separator
isPathSep :: Char -> Bool
#if defined(WinPathFmt)
isPathSep = (== ';') -- flag defined if os(windows) in cabal file
#else
isPathSep = (== ':') -- suitable for most *nix systems and Mac
#endif

-- obtain unique, canonicalized AO_PATH(s)
-- (uses working directory if no AO_PATH is defined)
getAO_PATH :: IO [FS.FilePath]
getAO_PATH = 
    Err.tryIOError (Env.getEnv "AO_PATH") >>= \ aop ->
    case aop of
        Left _ -> (:[]) <$> FS.getWorkingDirectory
        Right str ->
            let paths = splitPath str in
            mapM (Err.tryIOError . FS.canonicalizePath) paths >>=
            filterM FS.isDirectory . rights

splitPath :: String -> [FS.FilePath]
splitPath = map FS.fromText . T.split isPathSep . T.pack 

getAO_DICT :: IO Import
getAO_DICT =
    Err.tryIOError (Env.getEnv "AO_DICT") >>= \ aod ->
    case aod of
        Left _ -> return (T.pack "lang")
        Right str -> return (T.pack str)

reportError :: String -> IO ()
reportError = Sys.hPutStrLn Sys.stderr

-- load text for a given import.
-- (will print error then return empty text if import not found.)
importText :: Import -> IO Text
importText imp =
    getAO_PATH >>= \ fdirs ->
    let fn = FS.fromText imp FS.<.> T.pack "ao" in
    let fpaths = map (FS.</> fn) fdirs in
    firstM (map FS.readTextFile fpaths) >>= \ mbT ->
    case mbT of
        Just txt -> return txt
        Nothing -> 
            let eMsg = "unable find `" ++ T.unpack imp 
                       ++ "` for import"
            in
            reportError eMsg >> 
            return T.empty

-- return first success from list of actions)
firstM :: [IO a] -> IO (Maybe a)
firstM [] = return Nothing
firstM (op:ops) = 
    Err.tryIOError op >>= 
    either (const (firstM ops)) (return . Just)

-- import then parse.
importDictFile :: Import -> IO DictFile
importDictFile imp = readDictFileText imp <$> importText imp

-- deep load a dictionary from an initial import
--   loads each import exactly once (no cycles or redundancies)
--   order in list is such that rightmost words should override leftmost
importDictFiles :: Import -> IO [(Import,DictFile)]
importDictFiles = importDictFiles' [] . (:[])

importDictFiles' :: [(Import,DictFile)] -> [Import] -> IO [(Import,DictFile)]
importDictFiles' done [] = return done
importDictFiles' done (imp:imps) =
    let alreadyImported = L.elem imp (map fst done) in
    if alreadyImported then importDictFiles' done imps else
    importDictFile imp >>= \ df ->
    let imps' = L.reverse (df_imports df) ++ imps in
    importDictFiles' ((imp,df):done) imps'

