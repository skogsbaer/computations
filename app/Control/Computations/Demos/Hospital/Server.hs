{- |

doc/DOC_ID/VERSION --> load document with specific version
-}
module Control.Computations.Demos.Hospital.Server (serverMain) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.FileStore.Watcher
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Handler.Warp (run)
import Safe

eventSrcApp :: FileStoreWatcher -> Application
eventSrcApp fsw = eventSourceAppIO nextEvent
 where
  nextEvent = do
    changes <- atomically (waitForFileStoreChanges fsw)
    let jsonBuilder = J.fromEncoding (J.toEncoding (fsc_added changes))
        nameBuilder = T.encodeUtf8Builder "fileStoreChanges"
    pure $
      ServerEvent
        { eventName = Just nameBuilder
        , eventId = Just nameBuilder
        , eventData = [jsonBuilder]
        }

readVersion :: T.Text -> Maybe Version
readVersion t = fmap mkVersion $ readMay (T.unpack t)

invalidRequest :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
invalidRequest req respond = do
  logInfo ("Invalid request: " ++ show req)
  respond $ responseLBS status400 [] BSL.empty

serveDoc :: FilePath -> Application
serveDoc outDir req respond =
  case pathInfo req of
    "doc" : docId : (readVersion -> Just version) : _ -> do
      respond $
        responseFile
          status200
          [("Content-Type", "application/json")]
          (objFile outDir (mkDocId docId) version)
          Nothing
    _ -> invalidRequest req respond

app :: Application -> Application -> Application
app watchApp serveApp req respond =
  case pathInfo req of
    "doc" : _ -> serveApp req respond
    "watch" : _ -> watchApp req respond
    _ -> invalidRequest req respond

serverMain :: FilePath -> IO ()
serverMain outDir = do
  logNote "Listening on http://localhost:8080/"
  withFileStoreWatcher (FileStoreWatcherCfg outDir (milliseconds 500)) $ \watcher ->
    run 8080 (app (eventSrcApp watcher) (serveDoc outDir))
