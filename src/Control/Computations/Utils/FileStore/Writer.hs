{-

This is a gargabe collected store for documents with an eventlog for
addition and deletion of documents.  The eventlog is append-only but
events may be garbage collected. Garbage collection is done after some
delay (e.g. 2 days), but this has not been implemented.

Each event has the form

  (key, docId, version, hash)

- key: increasing
- docId: the ID of the document
- version: its version,
- hash: the hash of the document's content

The content of the document is stored outside of the eventlog.

NOTE: this code has not been taken from the production code but it has
been reimplemented. In the production code, there are two variants of the
file store: the implementation of the first variant is using postgresql,
the other a custom eventlog on disk.

Implementation notes:

There is a database table with columns:

- key (int, autoincrement, not null)
- docId (string, ID of the document, not null)
- version (int, not null)
- hash (word128, hex-encoding as string, null)

If hash is null, then the document has been deleted.

The content is stored on the filesystem, the filename is $docId_$version.

On write of a document with ID i with content b:

- Compute hash h of i and b.
- If DB does not contain i:
  - Write content with version 1 to disk
  - Add row (i, 1, h) to DB
- If DB contains i with version v:
  - If hash stored for i is equal to b: done
  - Otherwise:
    - Write content with version v+1 to disk
    - Add row (i, v+1, h)

On delete of document with ID i:

- If the current version of the document is v: add row (i, v+1, NULL) to DB
-}
module Control.Computations.Utils.FileStore.Writer (
  FileStore,
  FileStoreCfg (..),
  withFileStore,
  storeDoc,
  deleteDocs,
  listAllDocs,
  module Control.Computations.Utils.FileStore.Types,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.FileStore.Intern
import Control.Computations.Utils.FileStore.Types
import Control.Computations.Utils.Hash
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging
import qualified Control.Computations.Utils.SqliteUtils as Sqlite
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Database.SQLite3 as SqliteLib

data FileStoreCfg = FileStoreCfg
  { fsc_rootDir :: FilePath
  }
  deriving (Show)

data FileStore = FileStore
  { fs_rootDir :: FilePath
  , fs_db :: SqliteLib.Database
  , fs_queryAllStmt :: SqliteLib.Statement
  , fs_mostRecentStmt :: SqliteLib.Statement
  , fs_addStmt :: SqliteLib.Statement
  }

initFileStore :: FileStoreCfg -> IO FileStore
initFileStore cfg = do
  logDebug ("Initialing file store with config " ++ show cfg)
  db <- Sqlite.initSqliteDb (T.pack (fileStoreDbFile (fsc_rootDir cfg)))
  Sqlite.exec db $
    "CREATE TABLE IF NOT EXISTS file_store_objs ("
      <> "  key INTEGER PRIMARY KEY AUTOINCREMENT," -- first value will be 1
      <> "  docId TEXT NOT NULL,"
      <> "  version INTEGER NOT NULL,"
      <> "  hash TEXT NULL"
      <> ");"
      <> "CREATE UNIQUE INDEX docId_version ON file_store_objs(docId, version);"
      <> "CREATE INDEX docId ON file_store_objs(docId);"
  queryAllStmt <-
    SqliteLib.prepare db $
      "SELECT DISTINCT docId FROM file_store_objs AS outer"
        <> " WHERE hash is NOT NULL"
        <> " AND version IN (SELECT MAX(inner.version) FROM file_store_objs AS inner"
        <> "                 WHERE inner.docID = outer.docId)"
  mostRecentStmt <- mostRecentRowStmt db
  addStmt <-
    SqliteLib.prepare db $
      "INSERT INTO file_store_objs (docId, version, hash) VALUES"
        <> " (:docId, :version, :hash)"
  pure $
    FileStore
      { fs_rootDir = fsc_rootDir cfg
      , fs_db = db
      , fs_queryAllStmt = queryAllStmt
      , fs_mostRecentStmt = mostRecentStmt
      , fs_addStmt = addStmt
      }

closeFileStore :: FileStore -> IO ()
closeFileStore fs = do
  logDebug ("Closing file store at " ++ show (fs_rootDir fs))
  SqliteLib.finalize (fs_queryAllStmt fs)
  SqliteLib.finalize (fs_mostRecentStmt fs)
  SqliteLib.finalize (fs_addStmt fs)
  Sqlite.closeSqliteDb (fs_db fs)

withFileStore :: FileStoreCfg -> (FileStore -> IO a) -> IO a
withFileStore cfg action =
  bracket (initFileStore cfg) closeFileStore action

addRow :: FileStore -> DocId -> Version -> Maybe T.Text -> IO ()
addRow fs did v mh = do
  Sqlite.execStmt
    (fs_addStmt fs)
    [ (":docId", SqliteLib.SQLText (unDocId did))
    , (":version", SqliteLib.SQLInteger (unVersion v))
    , (":hash", sqlHash)
    ]
 where
  sqlHash =
    case mh of
      Nothing -> SqliteLib.SQLNull
      Just t -> SqliteLib.SQLText t

storeDoc :: FileStore -> DocId -> BS.ByteString -> IO Version
storeDoc fs did bs = do
  logDebug ("Storing document " ++ show did)
  mRow <- getMostRecentRow (fs_mostRecentStmt fs) did
  let h = hashToHexText (largeHash128 bs)
  case mRow of
    Just row ->
      if Some h == fsr_hash row
        then do
          logInfo
            ( "Not storing content for "
                ++ show did
                ++ ": old version "
                ++ show (fsr_version row)
                ++ " is identical (hash: "
                ++ T.unpack h
                ++ ")"
            )
          pure (fsr_version row)
        else reallyStore (Just row) h
    _ -> reallyStore Nothing h
 where
  reallyStore :: Maybe FileStoreRow -> T.Text -> IO Version
  reallyStore mRow h = do
    let newVersion =
          case mRow of
            Nothing -> firstVersion
            Just row -> incVersion (fsr_version row)
        objId = ObjId did newVersion
    logInfo
      ( "Storing content for "
          ++ show objId
          ++ " (hash: "
          ++ T.unpack h
          ++ ")"
      )
    writeFileAtomically (outputFile (fs_rootDir fs) did newVersion) bs
    addRow fs did newVersion (Just h)
    pure newVersion

deleteDocs :: FileStore -> [DocId] -> IO ()
deleteDocs fs docs = do
  logDebug ("Deleting documents " ++ show docs)
  Sqlite.withTransaction (fs_db fs) $ forM_ docs delDoc
 where
  delDoc :: DocId -> IO ()
  delDoc did = do
    mRow <- getMostRecentRow (fs_mostRecentStmt fs) did
    case mRow of
      Nothing -> logWarn ("Document " ++ show did ++ " not in DB, nothing to delete")
      Just row ->
        addRow fs did (incVersion (fsr_version row)) Nothing

listAllDocs :: FileStore -> IO (HashSet DocId)
listAllDocs fs = do
  logDebug ("Listing all documents")
  rows <- Sqlite.query (fs_queryAllStmt fs) []
  list <- forM rows $ \case
    [(_, SqliteLib.SQLText docId)] -> pure (DocId docId)
    _ -> fail ("FileStore: invalid row for queryAll result")
  pure (HashSet.fromList list)
