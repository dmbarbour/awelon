
-- | simple definition for a dictionary, plus some pure utilities to
-- manipulate it. A dictionary typically has a definition plus metadata
-- for each word. 
module AO.Dictionary
    ( AODef(..)
    ) where

import AO.Code

data AODef = AODef 
    { aod_word   :: Word    -- the word being defined
    , aod_code   :: AO_Code -- its definition 
    } 
