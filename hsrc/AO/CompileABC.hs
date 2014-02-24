-- | I would like to 'compile' ABC code, mostly to eliminate the
-- data plumbing at runtime, and also to perform some partial eval
-- of numbers and decisions.
--
-- Profiling of runABC has indicated that the 'lrwz' operations are
-- dominant on the performance costs, and that constructing numbers
-- is also very high. If most of these costs can be removed from a
-- runtime loop, the performance for running Haskell should be much
-- better - by perhaps an order of magnitude. In addition, we may be
-- able to achieve better memory locality, or even some parallelization.
--
-- Naive attempts to compile ABC have fallen flat. I could pursue a 
-- register-based compilation, but it isn't convenient to express that
-- in Haskell. I'll probably need to try a graph rewriting compiler, 
-- or even model a proper G-machine. 
module AO.CompileABC
    ( 
    ) where
