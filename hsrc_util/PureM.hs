
-- | PureM is a partial evaluation monad.
-- It performs pure computations where feasible.
module PureM (PureM(..), runPureM, runPureA) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data PureM m a = Pure a | PureM (m a)

instance (Functor f) => Functor (PureM f) where
    fmap fn (Pure a) = Pure (fn a)
    fmap fn (PureM fa) = PureM (fmap fn fa)
instance (Applicative a) => Applicative (PureM a) where
    pure = Pure
    (<*>) (Pure fn) ma = fmap fn ma
    (<*>) (PureM mfn) (Pure a) = (PureM (mfn <*> pure a))
    (<*>) (PureM mfn) (PureM ma) = PureM (mfn <*> ma)
instance (Monad m) => Monad (PureM m) where
    return = Pure
    (>>=) (Pure a) f = f a
    (>>=) (PureM op) f = PureM (op >>= runPureM . f)
    fail = PureM . fail
instance MonadTrans PureM where lift = PureM
instance (MonadIO m) => MonadIO (PureM m) where liftIO = PureM . liftIO

runPureA :: (Applicative m) => PureM m a -> m a
runPureA (Pure a) = pure a
runPureA (PureM ma) = ma

runPureM :: (Monad m) => PureM m a -> m a
runPureM (Pure a) = return a
runPureM (PureM ma) = ma
