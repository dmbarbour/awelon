
-- A pool of worker threads. Effectively a semaphore. But in this
-- case, new threads will spin up when necessary and self-destruct
-- when they run out of work. (Threads are cheap in Haskell.) The
-- main reason to limit concurrent work is to control resources,
-- e.g. number of open file descriptors and the amount of memory in
-- use but inaccessible due to only partial completion.
--
-- Intended for short-lived work, e.g. to read or write one file.
-- The IO operations should have their own way of calling home when
-- a result is needed. Workers will silently kill exceptions, but
-- the IO ops should catch them first (now asserted for debugging).
--
-- The worker pool is 'fair' in the sense that work starts in the same
-- order it is added to the pool. But work might not finish in that
-- order; the whole point is asynchronous operation!
--
module WorkerPool
    ( newWorkerPool
    ) where

import Data.IORef
import Control.Monad (join, void, liftM)
import Control.Exception (assert, try, SomeException)
import Control.Concurrent (forkIO)
import qualified Data.Sequence as S

type WQ = S.Seq (IO ()) 
type WPD = Either Int WQ
type WPool = IORef WPD 

-- VALID WORKER STATE:
--  either `Left n` with n > 0 (excess workers)
--     or  `Right ops` (waiting on a worker)

newWorkerPool :: Int -> IO (IO () -> IO ())
newWorkerPool n = assert (n > 0) $ liftM addWork $ newIORef (Left n)

addWork :: WPool -> IO () -> IO ()
addWork wp op = join $ atomicModifyIORef wp addw where
    addw (Left 1) = (Right S.empty, forkWorker wp op)
    addw (Left n) = assert (n > 1) $ (Left (n - 1), forkWorker wp op)
    addw (Right ops) = (Right (ops S.|> op), return ())

forkWorker :: WPool -> IO () -> IO ()
forkWorker wp op = void $ forkIO $ workerLoop wp op

workerLoop :: WPool -> IO () -> IO ()
workerLoop wp op = (try op >>= assertNoE) >> doMoreWork wp

assertNoE :: Either SomeException a -> IO ()
assertNoE (Left _) = assert False $ return ()
assertNoE _ = return ()
    
doMoreWork :: WPool -> IO ()
doMoreWork wp = join $ atomicModifyIORef wp takew where
    takew (Left n) = assert (n > 0) $ (Left (n + 1), return ())
    takew (Right wq) = case S.viewl wq of
        S.EmptyL -> (Left 1, return ())
        (op S.:< ops) -> (Right ops, workerLoop wp op)

