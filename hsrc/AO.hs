
-- | This module can read basic AO dictionary files, perform a few
-- simple validations, and compile words into ABC code. 
module AO
    ( parseDictFile
    , loadDictFile
    , loadDictionary
    ) where

import Filesystem
import qualified Text.Parsec as P


parseDictFile :: (P.Stream s m Char) => P.ParsecT s u m DictF
parseDef :: (P.Stream 
