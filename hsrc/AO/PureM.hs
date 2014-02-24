
-- | PureM is a partial evaluation monad.
-- It performs pure computations where feasible.
module AO.PureM (PureM(..), runPureM, runPureA) where

import Control.Applicative
import Control.Monad.Trans.Class

data PureM m a = Pure a | PureM (m a)

instance (Functor f) => Functor (PureM f) where
    fmap fn (Pure a) = Pure (fn a)
    fmap fn (PureM fa) = PureM (fmap fn fa)
instance (Applicative a) => Applicative (PureM a) where
    pure = Pure
    (<*>) (Pure fn) ma = fmap fn ma
    (<*>) (PureM mfn) (Pure a) = (PureM (mfn <*> pure a))
    (<*>) (PureM mfn) (PureM ma) = PureM (mfn <*> ma)
    (<*)  fa (Pure _) = fa
    (<*)  fa fb = (const <$> fa <*> fb)
    (*>)  (Pure _) fb = fb
    (*>)  fa fb = (flip const <$> fa <*> fb)
instance (Monad m) => Monad (PureM m) where
    return = Pure
    (>>=) (Pure a) f = f a
    (>>=) (PureM op) f = PureM $ 
        op >>= \ result ->
        case f result of
            (Pure b) -> return b
            (PureM mb) -> mb
instance MonadTrans PureM where lift = PureM

runPureA :: (Applicative m) => PureM m a -> m a
runPureA (Pure a) = pure a
runPureA (PureM ma) = ma

runPureM :: (Monad m) => PureM m a -> m a
runPureM (Pure a) = return a
runPureM (PureM ma) = ma
