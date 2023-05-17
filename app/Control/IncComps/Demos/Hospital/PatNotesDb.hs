{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Control.IncComps.Demos.Hospital.PatNotesDb (
  setupPatNotesDb,
  patNotesSqliteSrcCfg,
  patNotes,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine
import Control.IncComps.Demos.Hospital.PatTypes
import Control.IncComps.FlowImpls.SqliteSrc
import qualified Control.IncComps.Utils.SqliteUtils as Sqlite
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.TimeUtils
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Int
import Data.LargeHashable
import qualified Data.Text as T

setupPatNotesDb :: FilePath -> IO ()
setupPatNotesDb path = do
  Sqlite.withSqliteDb (T.pack path) $ \db -> do
    Sqlite.exec db $
      "CREATE TABLE IF NOT EXISTS pat_notes ("
        <> "  key INTEGER PRIMARY KEY AUTOINCREMENT"
        <> "  pat_id TEXT NOT NULL,"
        <> "  time TEXT NOT NULL,"
        <> "  text TEXT NOT NULL"
        <> ");"

patNotesSqliteSrcCfg :: CompSrcInstanceId -> FilePath -> TimeSpan -> SqliteSrcCfg
patNotesSqliteSrcCfg ident path ts =
  SqliteSrcCfg
    { ssc_ident = ident
    , ssc_fileName = path
    , ssc_tableName = "pat_notes"
    , ssc_increasingCol = "key"
    , ssc_filterCol = "pat_id"
    , ssc_pollInterval = ts
    }

rowToPatNote :: MonadFail m => Sqlite.SQLRow -> m PatNote
rowToPatNote row = do
  patId <-
    Sqlite.getColumnValue row "pat_id" >>= \case
      Sqlite.SQLText t -> pure t
      x -> fail ("Invalid value in pat_id column of pat_notes DB: " ++ show x)
  time <-
    Sqlite.getColumnValue row "time" >>= \case
      Sqlite.SQLText t ->
        case parseUTCTime (T.unpack t) of
          Just x -> pure x
          Nothing ->
            fail ("Invalid value in time column of pat_notes DB: " ++ show t)
      x -> fail ("Invalid value in time column of pat_notes DB: " ++ show x)
  note <-
    Sqlite.getColumnValue row "text" >>= \case
      Sqlite.SQLText t -> pure t
      x -> fail ("Invalid value in text column of pat_notes DB: " ++ show x)
  pure (PatNote (PatId patId) time note)

patNotes :: TypedCompSrcId SqliteSrc -> PatId -> CompM (HashSet PatNote)
patNotes srcId (PatId patId) = do
  res <- compSrcReq srcId (AllData (RowFilter (Sqlite.SQLText patId)))
  l <- mapM rowToPatNote res
  pure (HashSet.fromList l)
