module Control.Computations.Utils.ConcUtils (
  ignoreThreadKilled,
  ignoreThreadKilled',
  timeout,
  timeoutFail,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Exception
import Control.Monad.Catch
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class
import qualified System.Timeout as Sys

ignoreThreadKilled :: (MonadIO m, MonadCatch m) => m () -> m ()
ignoreThreadKilled = ignoreThreadKilled' ()

ignoreThreadKilled' :: (MonadIO m, MonadCatch m) => a -> m a -> m a
ignoreThreadKilled' onKilled action =
  Catch.catchJust predicate action handler
 where
  handler msg =
    do
      tid <- liftIO myThreadId
      liftIO $ logDebug (msg tid)
      return onKilled
  predicate exc
    | Just ThreadKilled <- fromException exc =
        Just $ \tid -> "Ignoring ThreadKilled exception for " ++ (show tid)
    | Just Async.AsyncCancelled <- fromException exc =
        Just $ \tid -> "Ignoring AsyncCanceled exception for " ++ (show tid)
    | otherwise = Nothing

timeout :: TimeoutFun a
timeout t = Sys.timeout (asMicroseconds t)

timeoutFail :: String -> TimeSpan -> IO a -> IO a
timeoutFail name ts action =
  do
    res <- timeout ts action
    case res of
      Nothing -> fail (name ++ " timed out after " ++ show ts)
      Just v -> pure v
