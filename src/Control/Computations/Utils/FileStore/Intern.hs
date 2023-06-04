module Control.Computations.Utils.FileStore.Intern (
  FileStoreRow (..),
  fileStoreDbFile,
  mostRecentRowStmt,
  getMostRecentRow,
  parseFileStoreRow,
  outputFile,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.FileStore.Types
import Control.Computations.Utils.Logging
import qualified Control.Computations.Utils.SqliteUtils as Sqlite
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Int
import qualified Data.Text as T
import qualified Database.SQLite3 as SqliteLib
import System.FilePath

fileStoreDbFile :: FilePath -> FilePath
fileStoreDbFile rootDir = rootDir </> "index.sqlite"

mostRecentRowStmt :: Sqlite.Database -> IO Sqlite.Statement
mostRecentRowStmt db =
  SqliteLib.prepare db $
    "SELECT key, docId, version, hash FROM file_store_objs"
      <> " WHERE docId = :docId"
      <> " AND version IN (SELECT MAX(version) FROM file_store_objs WHERE docID = :docId)"

data FileStoreRow = FileStoreRow
  { fsr_key :: Int64
  , fsr_id :: DocId
  , fsr_version :: Version
  , fsr_hash :: Option T.Text
  }

getMostRecentRow :: Sqlite.Statement -> DocId -> IO (Maybe FileStoreRow)
getMostRecentRow stmt did = do
  logDebug ("Getting most recent row from file store for " ++ show did)
  rows <- Sqlite.query stmt [(":docId", SqliteLib.SQLText (unDocId did))]
  case rows of
    [] -> pure Nothing
    [row] -> do
      x <- parseFileStoreRow row
      pure (Just x)
    _ -> fail $ "Invalid result for getMostRecentRow query: " ++ show rows

parseFileStoreRow :: MonadFail m => Sqlite.SQLRow -> m FileStoreRow
parseFileStoreRow row = do
  case row of
    [ ("key", SqliteLib.SQLInteger key)
      , ("docId", SqliteLib.SQLText docId)
      , ("version", SqliteLib.SQLInteger v)
      , ("hash", sqlHash)
      ] -> do
        h <- case sqlHash of
          SqliteLib.SQLText t -> pure (Some t)
          SqliteLib.SQLNull -> pure None
          _ -> fail errMsg
        pure $
          FileStoreRow
            { fsr_key = key
            , fsr_id = DocId docId
            , fsr_version = mkVersion v
            , fsr_hash = h
            }
    _ -> fail errMsg
 where
  errMsg = "Row is not a valid file store row: " ++ show row

outputFile :: FilePath -> DocId -> Version -> FilePath
outputFile rootDir did v = rootDir </> (T.unpack (unDocId did) ++ "_" ++ show (unVersion v))
