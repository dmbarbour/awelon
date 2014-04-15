
-- Exposes definition for Dict between the modules that need it
-- (for safe casting, mostly)
module AO.InnerDict 
    ( AODict(..)
    ) where

import Control.Arrow (second)
import qualified Data.Map as M
import AO.Code

-- | An AO dictionary is simply a map from words to code plus any
-- metadata for each word. 
--
-- However, a dictionary has a few special restrictions: 
--
-- * no cyclic definitions
-- * no incompletely defined words
--
-- In this AO library, these attributes are enforced by a smart
-- constructors, 'buildAODict' or 'cleanAODict'. Functions taking
-- an `AODict` type may then depend on a clean (but not necessarily
-- type-safe) dictionary. 
--
newtype AODict meta = AODict (M.Map Word (AO_Code, meta))

-- for debugging support, just counts the words.
instance Show (AODict meta) where
    showsPrec _ (AODict d) = 
        showString "AO dictionary with " . 
        shows (M.size d) . showString " words."

-- users are free to manipulate metadata 
-- without disturbing a clean AODict.
instance Functor AODict where 
    fmap fn (AODict d) = AODict (fmap (second fn) d)

