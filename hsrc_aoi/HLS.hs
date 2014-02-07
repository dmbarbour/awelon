
-- | HLS describes state with history and a half-life. The decay is
-- pseudo-random on a flat buffer. Developers can set two parameters:
--
--   halflife - determines how much state is kept
--   join function - determines how old states collapse
--
-- An alternative implementation might use multiple ring buffers for
-- state, dropping the occasional state as it moves from buffer to
-- buffer. However, this is not used at the moment.
module HLS
    ( HLS
    , hls_full_init
    , hls_init
    , hls_put, hls_get, hls_modify
    , hls_getHist
    ) where

import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified System.Random as R

-- states with exponential decay of history
data HLS s = HLS 
    { hls_halflife :: {-# UNPACK #-} !Double 
    , hls_rgen     :: !R.StdGen
    , hls_state    :: !s
    , hls_hist     :: !(S.Seq s)
    , hls_join     :: !(s -> s -> s)
    }

-- | Initialize with halflife and join function.
--
-- The join function is applied whenever two states are collapsed;
-- the newer state is joined with the older state, somewhere deep
-- in the history. 
hls_full_init :: Double -> (s -> s -> s) -> s -> HLS s
hls_full_init hl jf s0 = HLS
    { hls_halflife = hl
    , hls_rgen = R.mkStdGen 60091
    , hls_state = s0
    , hls_hist = S.empty
    , hls_join = jf
    }

-- | default half-life (e) and default join-function (const)
hls_init :: s -> HLS s
hls_init = hls_full_init 2.71828 const 

-- | obtain incomplete history (including current state)
hls_getHist :: HLS s -> [s]
hls_getHist hls = S.toList (hls_state hls S.<| hls_hist hls)

-- | put a value into the state
hls_put :: s -> HLS s -> HLS s
hls_put s hls =
    let (r,g') = R.random (hls_rgen hls) in
    let n = negate $ round $ log (1.0 - r) * hls_halflife hls in
    let h' = dropHLS (hls_join hls) n (hls_state hls S.<| hls_hist hls) in
    h' `seq` s `seq`
    hls { hls_rgen = g', hls_state = s, hls_hist = h' }

-- | get current value from the state
hls_get :: HLS s -> s
hls_get = hls_state

hls_modify :: (s -> s) -> HLS s -> HLS s
hls_modify f hls = hls_put (f (hls_get hls)) hls

-- dropHLS will join two items in a list at the given position
-- This operation is strict in the join function because it is
-- important to avoid a memory leak. 
dropHLS :: (s -> s -> s) -> Int -> S.Seq s -> S.Seq s
dropHLS jf n hist =
    let (before,after) = S.splitAt n hist in
    case (S.viewr before, S.viewl after) of
        (S.EmptyR, _) -> after
        (_, S.EmptyL) -> before
        (hNew S.:> sNew, sOld S.:< hOld) ->
            let sJ = jf sNew sOld in
            sJ `seq` ((hNew S.|> sJ) S.>< hOld)
