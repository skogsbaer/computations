module Control.Computations.Demos.Hospital.Server (serverMain) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils

----------------------------------------
-- EXTERNAL
----------------------------------------

import qualified Data.Text.Encoding as T
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Handler.Warp (run)

app :: Application
app = eventSourceAppIO nextEvent
 where
  nextEvent = do
    c_sleep realClock (seconds 2)
    t <- c_currentTime realClock
    let s = formatUTCTime t
    pure $
      ServerEvent
        { eventName = Just (T.encodeUtf8Builder "time")
        , eventId = Just (T.encodeUtf8Builder "time")
        , eventData = [T.encodeUtf8Builder s]
        }

serverMain :: IO ()
serverMain = do
  logNote "Listening on http://localhost:8080/"
  run 8080 app
