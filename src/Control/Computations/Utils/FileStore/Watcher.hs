{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.FileStore.Watcher (
  FileStoreWatcherCfg (..),
  FileStoreWatcher,
  FileStoreChanges (..),
  withFileStoreWatcher,
  waitForFileStoreChanges,
  loadDoc,
  objFile,
  module Control.Computations.Utils.FileStore.Types,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.FileStore.Intern
import Control.Computations.Utils.FileStore.Types
import Control.Computations.Utils.Logging
import qualified Control.Computations.Utils.SqliteUtils as Sqlite
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int
import qualified Data.Text as T
import qualified Database.SQLite3 as SqliteLib
import Test.Framework

data FileStoreWatcherCfg = FileStoreWatcherCfg
  { fswc_rootDir :: FilePath
  , fswc_pollInterval :: TimeSpan
  }
  deriving (Show)

data FileStoreWatcher = FileStoreWatcher
  { fsw_rootDir :: FilePath
  , fsw_thread :: Async ()
  , fsw_db :: Sqlite.Database
  , fsw_newRowsStmt :: Sqlite.Statement
  , fsw_mostRecentStmt :: SqliteLib.Statement
  , fsw_changesVar :: TVar FileStoreChanges
  }

data FileStoreChanges = FileStoreChanges
  { fsc_added :: HashMap DocId Version
  , fsc_deleted :: HashSet DocId
  }
  deriving (Eq, Show)

emptyFileStoreChanges :: FileStoreChanges
emptyFileStoreChanges = FileStoreChanges HashMap.empty HashSet.empty

isEmptyFileStoreChanges :: FileStoreChanges -> Bool
isEmptyFileStoreChanges c =
  HashMap.null (fsc_added c) && HashSet.null (fsc_deleted c)

initFileStoreWatcher :: FileStoreWatcherCfg -> IO FileStoreWatcher
initFileStoreWatcher cfg = do
  logDebug ("Initializing file store watcher with config " ++ show cfg)
  db <-
    SqliteLib.open2
      (T.pack (fileStoreDbFile (fswc_rootDir cfg)))
      [SqliteLib.SQLOpenReadOnly, SqliteLib.SQLOpenFullMutex]
      SqliteLib.SQLVFSDefault
  changesVar <- newTVarIO emptyFileStoreChanges
  let newRowsQuery =
        "SELECT key, docId, version, hash FROM file_store_objs"
          <> " WHERE key > :last "
          <> " ORDER BY KEY ASC"
  stmt <- SqliteLib.prepare db newRowsQuery
  lastStmt <- SqliteLib.prepare db "SELECT MAX(key) FROM file_store_objs"
  rows <- Sqlite.retryIfBusy "FileStoreWatcher.init" $ Sqlite.query lastStmt []
  last <-
    case rows of
      [] -> pure (unVersion zeroVersion)
      [[(_, Sqlite.SQLNull)]] -> pure (unVersion zeroVersion)
      [[(_, Sqlite.SQLInteger k)]] -> pure k
      _ -> fail ("Invalid result for lastStmt: " ++ show rows)
  thread <- async (watcher cfg last stmt changesVar)
  mostRecentStmt <- mostRecentRowStmt db
  pure (FileStoreWatcher (fswc_rootDir cfg) thread db stmt mostRecentStmt changesVar)

closeFileStoreWatcher :: FileStoreWatcher -> IO ()
closeFileStoreWatcher fsw = do
  logDebug ("Closing file store watcher")
  cancel (fsw_thread fsw)
  SqliteLib.finalize (fsw_newRowsStmt fsw)
  Sqlite.closeSqliteDb (fsw_db fsw)

withFileStoreWatcher :: FileStoreWatcherCfg -> (FileStoreWatcher -> IO a) -> IO a
withFileStoreWatcher cfg action =
  bracket (initFileStoreWatcher cfg) closeFileStoreWatcher action

watcher :: FileStoreWatcherCfg -> Int64 -> Sqlite.Statement -> TVar FileStoreChanges -> IO ()
watcher cfg last stmt changesVar =
  do
    let loop last =
          do
            logTrace ("FileStoreWatcher.watcher, last=" ++ show last)
            newLast <- performStep last
            c_sleep realClock (fswc_pollInterval cfg)
            loop newLast
    loop last
 where
  performStep :: Int64 -> IO Int64
  performStep last = do
    rows' <-
      Sqlite.retryIfBusy "FileStoreWatcher.query" $
        Sqlite.query stmt [(":last", Sqlite.SQLInteger last)]
          `catch` ( \(e :: SqliteLib.SQLError) -> do
                      if SqliteLib.sqlError e == SqliteLib.ErrorBusy
                        then logDebug ("sqlite watcher: " ++ show e)
                        else logWarn ("sqlite watcher: " ++ show e)
                      pure []
                  )
    rows <- mapM parseFileStoreRow rows'
    atomically $ do
      oldChanges <- readTVar changesVar
      newChanges <- foldM handleRow oldChanges rows
      unless (newChanges == oldChanges) $
        logDebugSTM ("New file store changes: " ++ show newChanges)
      writeTVar changesVar newChanges
    case reverse rows of
      [] -> pure last
      lastRow : _ -> pure (fsr_key lastRow)
  handleRow
    :: FileStoreChanges
    -> FileStoreRow
    -> STM FileStoreChanges
  handleRow changes row = do
    case fsr_hash row of
      None ->
        pure $
          FileStoreChanges
            { fsc_added = HashMap.delete (fsr_id row) (fsc_added changes)
            , fsc_deleted = HashSet.insert (fsr_id row) (fsc_deleted changes)
            }
      Some _ ->
        pure $
          FileStoreChanges
            { fsc_added = HashMap.insert (fsr_id row) (fsr_version row) (fsc_added changes)
            , fsc_deleted = HashSet.delete (fsr_id row) (fsc_deleted changes)
            }

loadDoc :: FileStoreWatcher -> DocId -> IO (Fail (Version, BS.ByteString))
loadDoc fsw did = do
  logDebug ("Loading document " ++ show did)
  mRow <- getMostRecentRow (fsw_mostRecentStmt fsw) did
  case mRow of
    Nothing -> do
      let msg = "Could not load content of " ++ show did ++ ": no row in DB"
      logWarn msg
      pure (Fail msg)
    Just (fsr_hash -> None) -> do
      let msg = "Could not load content of " ++ show did ++ ": document has been deleted"
      logWarn msg
      pure (Fail msg)
    Just (fsr_version -> version) -> do
      bs <- BS.readFile (outputFile (fsw_rootDir fsw) did version)
      pure (Ok (version, bs))

waitForFileStoreChanges :: FileStoreWatcher -> STM FileStoreChanges
waitForFileStoreChanges fsw = do
  changes <- readTVar (fsw_changesVar fsw)
  when (isEmptyFileStoreChanges changes) retry
  writeTVar (fsw_changesVar fsw) emptyFileStoreChanges
  pure changes

objFile :: FilePath -> DocId -> Version -> FilePath
objFile = outputFile
