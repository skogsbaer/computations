{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- | Dispatches changes from one source to multiple listeners.
module Control.Computations.Utils.Dispatcher (
  Dispatcher,
  Listener,
  initDispatcher,
  closeDispatcher,
  withDispatcher,
  mkListener,
  waitListener,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Tuple
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Test.Framework

data Dispatcher a = Dispatcher
  { d_thread :: Async ()
  , d_var :: TVar (Option (Int :!: a))
  }

initDispatcher :: STM a -> IO (Dispatcher a)
initDispatcher wait = do
  v <- newTVarIO None
  t <- async (waitLoop 0 v)
  pure (Dispatcher t v)
 where
  waitLoop !i v = do
    x <- atomically wait
    atomically $ writeTVar v (Some (i :!: x))
    waitLoop (i + 1) v

closeDispatcher :: Dispatcher a -> IO ()
closeDispatcher d = cancel (d_thread d)

withDispatcher :: STM a -> (Dispatcher a -> IO b) -> IO b
withDispatcher wait = bracket (initDispatcher wait) closeDispatcher

data Listener a = Listener
  { l_prev :: TVar Int
  , l_var :: TVar (Option (Int :!: a))
  }

mkListener :: Dispatcher a -> IO (Listener a)
mkListener d = do
  cur <- readTVarIO (d_var d)
  start <- newTVarIO $ (option 0 fst' cur) - 1
  pure (Listener start (d_var d))

waitListener :: Listener a -> STM a
waitListener l = do
  prev <- readTVar (l_prev l)
  x <- readTVar (l_var l)
  case x of
    None -> retry
    Some (cur :!: y) ->
      if prev == cur
        then retry
        else do
          writeTVar (l_prev l) cur
          pure y

test_basics :: IO ()
test_basics = do
  changesVar <- newTVarIO None
  let emitChange i = atomically $ writeTVar changesVar (Some i)
  withDispatcher (wait changesVar) $ \disp -> do
    l1 <- mkListener disp
    v1 <- newTVarIO []
    withAsync (listen v1 l1) $ \_ -> do
      emitChange 1
      l2 <- mkListener disp
      v2 <- newTVarIO []
      withAsync (listen v2 l2) $ \_ -> do
        sleep
        emitChange 2
        sleep
        emitChange 3
        sleep
        list1 <- readTVarIO v1
        assertEqual [3, 2, 1] list1
        list2 <- readTVarIO v2
        assertEqual [3, 2, 1] list2
 where
  sleep = c_sleep realClock (milliseconds 5)
  listen :: TVar [Int] -> Listener Int -> IO ()
  listen v l = do
    x <- atomically $ waitListener l
    atomically $ modifyTVar' v $ \list -> x : list
    listen v l
  wait :: TVar (Option Int) -> STM Int
  wait v = do
    x <- readTVar v
    case x of
      None -> retry
      Some y -> do
        writeTVar v None
        pure y
