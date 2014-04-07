{-# LANGUAGE ViewPatterns #-}
-- | This module models the side-effects used by AOExec.
--
-- AOExec adapts RDP's resource model to the extent feasible for an
-- imperative interpretation of AO. The role of side-effects is to
-- observe or influence external resources. Conceptually, resources
-- already exist, have stable identity, and wait to be manipulated.
-- 
-- * Some resources are 'abundant', i.e. there are as many unique 
--   resources as unique identifiers. Abundant resources include
--   local state, channels, broadcast topics, and so on. Developers
--   can use these resources as the application needs. 
--
-- * Affine and linear types model exclusive access to resources. So
--   developers can reason that access to a resource is exclusive in
--   a program, at least within a specific time-step. 
--
-- Where feasible, state resources in AOExec default to persistent.
--
-- AO doesn't permit infinite loops. Long-running behaviors instead
-- are modeled by manipulating some programmable resources. Details
-- haven't been hammered out, yet.
--
-- AOExec enforces a managed model of time. Writes in one step are 
-- generally not visible until the next step. This helps enforce AO's 
-- policy that every subprogram must terminate. It also simplifies 
-- reasoning about partial failure and concurrency. The initial 
-- AOExec word occurs as a single step, so cannot observe its own
-- effects.
module AOPower
    ( newDefaultContext
    , executivePowers
    , processContext
    , CX (..)
    ) where

import Control.Arrow (first)
import Control.Applicative
-- import Control.Monad 
-- import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
-- import qualified System.Environment as Sys
import qualified System.IO as Sys
import AOPrelude
import qualified AODict -- to support introspection

data CX = CX
    { cx_stepct :: Integer -- for debugging, mostly
    , cx_dict   :: M.Map Text Program
    , cx_powers :: M.Map Text (CX -> V -> IO V)
    }

baseDict :: M.Map Text Program
baseDict = M.fromList $ fmap (first T.pack) $ AODict.allWords

newDefaultContext :: IO CX
newDefaultContext = return $ CX 
    { cx_stepct = 0 
    , cx_dict = baseDict
    , cx_powers = rootPowers
    }

vtxt :: V -> Maybe Text
vtxt = fmap T.pack . vToText

executivePowers :: CX -> Block
executivePowers cx = pb where
    pb = Block True True ((=<<) runCmd) where
    runCmd (P (vtxt -> Just cmd) args) =
        case M.lookup cmd (cx_powers cx) of
            Nothing -> fail $ "unrecognized power: " ++ show cmd
            Just action -> (P (B pb)) <$> action cx args
    runCmd v = fail $ "unrecognized command: " ++ show v

processContext :: CX -> IO ()
processContext _cx = return ()

rootPowers :: M.Map Text (CX -> V -> IO V)
rootPowers = M.fromList $ fmap (first T.pack) $
    [ ("dict.word", getDictWord)
    , ("dict.wordList", getDictWordList)
    , ("debugOut", const debugPrint)
    -- OS environment variables (read only and trivial)
    -- , ("OS.getEnv", const getOSEnv)
    -- , ("OS.getEnvList", const getOSEnvList)
    -- partitioned subprograms? need sub-context, too, to fork/join...
    -- secure random sources
    -- filesystem readers and writers
    -- clock models? realtime delayed actions?
    -- unique values?
    ]

debugPrint :: V -> IO V
debugPrint v = Sys.hPutStrLn Sys.stderr (show v) >> return v

-- obtain a word from the dictionary
getDictWord :: CX -> V -> IO V
getDictWord cx (vtxt -> Just word) =
    case M.lookup word (cx_dict cx) of
        Just prog -> return (R (B (Block False False prog)))
        Nothing -> return (L U)
getDictWord _cx v = fail $ "dict.word expects text argument; received " ++ show v

getDictWordList :: CX -> V -> IO V
getDictWordList cx U = return $ (listToV . fmap dictEntV . M.toList . cx_dict) cx
getDictWordList _cx v = fail $ "dict.wordList expects unit argument; received " ++ show v

listToV :: [V] -> V
listToV [] = L U
listToV (v:vs) = R (P v (listToV vs))

dictEntV :: (Text, Program) -> V
dictEntV (txt,prog) = P (textToV $ T.unpack txt) (B (Block False False prog))


