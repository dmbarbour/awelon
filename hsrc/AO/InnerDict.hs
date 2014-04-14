
-- Exposes definition for Dict between the modules that need it
-- (for safe casting, mostly)
module AO.InnerDict 
    ( AODict(..)
    , AODictT(..)
    ) where

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
-- constructor, 'buildAODict'. Functions taking an `AODict` type
-- may then depend on a clean (but not necessarily type-safe) 
-- dictionary. 
--
newtype AODict meta = AODict (M.Map Word (AO_Code, meta))

-- | An AO 'typed' dictionary is both clean and typesafe. It can be
-- cast to a merely clean dictionary. Construction of a typed 
-- dictionary also tends to add metadata for type information.
newtype AODictT meta = AODictT (M.Map Word (AO_Code, meta))


