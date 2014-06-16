
-- | Compile a graphical IR to Haskell text to support JIT.
--
-- This is an *imperative* interpretation of the ABC graph.
--
module GraphToHS 
    ( abc2hs
    ) where

import ABC.Operators
import ABCGraph

abc2hs :: [Op] -> Either String String
abc2hs = either Left mkGraph . abc2graph where
    mkGraph (wI,ns,wO) = 

graph2hs wI ns wO

-- for now, let's assume the initial node list is provided 
-- in a topological order. I can try to enforce this later, 
-- it isn't especially essential at the moment.
graph2hs :: Wire -> [Node] -> Wire -> Either String String
graph2hs




