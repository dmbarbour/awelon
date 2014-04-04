
-- | This module models the side-effects used by AOExec.
--
-- AOExec adapts RDP's resource model to the extent feasible for an
-- imperative interpretation of AO. The role of side-effects is to
-- observe or influence external resources. Those resources already
-- exist, have stable identity, and often wait to be manipulated.
-- Some resources are 'abundant' - as many resources available as
-- unique identifiers. Some resources are 'linear' - i.e. requiring
-- exclusive control (at least initially), which is enforcable by 
-- AO's substructural types.
--
-- Long-running behaviors are expressed by interacting with programmable
-- resources, i.e. programming their future behavior.
--
-- AOExec enforces a managed model of time. Writes in one step are 
-- generally not visible until the next step. This helps enforce AO's 
-- policy that every subprograms must terminate. It also simplifies 
-- reasoning about partial failure and concurrency. The initial 
-- AOExec word occurs as a single step, so cannot observe its own
-- effects.
module AOPower
    ( newDefaultContext
    , executivePowers
    , processContext
    ) where

import Control.Monad 
import AOPrelude
import AODict (allWords) -- to support introspection

data CX = CX

newDefaultContext :: IO CX
newDefaultContext = return CX

executivePowers :: CX -> Block
executivePowers cx = Block True True ((=<<) pbTodo) where
    pbTodo v = fail $ "TODO: handle " ++ show v

processContext :: CX -> IO ()
processContext _cx = return ()

