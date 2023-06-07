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
import Control.Computations.Utils.Types
import Control.Computations.Utils.Dispatcher

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.Maybe
import Control.Concurrent.STM
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.EventSource.EventStream
import Network.Wai.Handler.Warp (run)
import Safe
import System.FilePath
import qualified Data.List as L

_myEventSourceAppIO :: IO ServerEvent -> Application
_myEventSourceAppIO src _ sendResponse =
    sendResponse $ responseStream
        status200
        [(hContentType, "text/event-stream")]
        send
  where
    send sendChunk flush = do
      se <- src
      case eventToBuilder se of
        Nothing -> do
          logWarn ("Could not convert event to builder")
          pure ()
        Just b  -> do
          logInfo ("Sending new event: " ++ show b)
          sendChunk b
          flush
          send sendChunk flush

eventSrcApp :: Dispatcher FileStoreChanges -> Application
eventSrcApp disp req respond = do
  idVar <- newTVarIO (0::Int)
  l <- mkListener disp
  eventSourceAppIO (nextEvent idVar l) req respond
 where
  nextEvent idVar l = do
    changes <- atomically (waitListener l)
    id <- atomically $ stateTVar idVar $ \x -> (x, x+1)
    logInfo ("Pushing new event: " ++ show changes)
    let jsonBuilder = J.fromEncoding (J.toEncoding (fsc_added changes))
        nameBuilder = T.encodeUtf8Builder "fileStoreChanges"
        idBuilder = T.encodeUtf8Builder ("fileStoreChanges_" <> showText id)
    pure $
      ServerEvent
        { eventName = Just nameBuilder
        , eventId = Just idBuilder
        , eventData = [jsonBuilder]
        }

readVersion :: T.Text -> Maybe Version
readVersion t = fmap mkVersion $ readMay (T.unpack t)

invalidRequest :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
invalidRequest req respond = do
  logInfo ("Invalid request: " ++ show req)
  respond $ responseLBS status400 [] BSL.empty

notFound :: [T.Text] -> (Response -> IO ResponseReceived) -> IO ResponseReceived
notFound path respond = do
  logInfo ("Not found: " ++ show (T.intercalate "/" path))
  respond $ responseLBS status404 [] BSL.empty

serveDoc :: FilePath -> FilePath -> Application
serveDoc outDir webappDir req respond =
  case pathInfo req of
    "doc" : docId : (readVersion -> Just version) : [] -> do
      sendFile (Just "application/json")
        (objFile outDir (mkDocId docId) version)
    "doc" : _ -> invalidRequest req respond
    "static" : (T.unpack -> name) : [] -> do
      sendFile Nothing (webappDir </> name)
    [] ->
      sendFile Nothing (webappDir </> "index.html")
    p -> notFound p respond
  where
    sendFile mcType path = do
      let cType = fromMaybe (contentTypeFromFilename path) mcType
      respond $
        responseFile
          status200
          [(hContentType, cType)]
          path
          Nothing

contentTypeFromFilename :: String -> BS.ByteString
contentTypeFromFilename fname =
  case L.lookup (takeExtension fname) list of
    Just x -> x
    Nothing -> def
  where
    list = [(".css", "text/css"),
             (".gif", "image/gif"),
             (".htm", "text/html"),
             (".html", "text/html"),
             (".jpe", "image/jpeg"),
             (".jpeg", "image/jpeg"),
             (".jpg", "image/jpeg"),
             (".js", "application/x-javascript"),
             (".png", "image/png")
             ]
    def = "application/octet-stream"

app :: Application -> Application -> Application
app watchApp serveApp req respond = do
  logDebug ("New request: " ++ show req)
  case pathInfo req of
    "watch" : _ -> watchApp req respond
    _ -> serveApp req respond


serverMain :: FilePath -> FilePath -> IO ()
serverMain outDir webappDir = do
  logNote "Listening on http://localhost:8080/"
  withFileStoreWatcher (FileStoreWatcherCfg outDir (milliseconds 500)) $ \watcher ->
    withDispatcher (waitForFileStoreChanges watcher) $ \dispatcher ->
      run 8080 (app (eventSrcApp dispatcher) (serveDoc outDir webappDir))
