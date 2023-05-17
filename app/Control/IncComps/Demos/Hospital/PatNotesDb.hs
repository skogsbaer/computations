{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Control.IncComps.Demos.Hospital.PatNotesDb (
  setupPatNotesDb,
  withPatNotesDb,
  patNotesSqliteSrcCfg,
  patNotes,
  insertPatNote
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

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T

setupPatNotesDb :: FilePath -> IO ()
setupPatNotesDb path = withPatNotesDb path (\_ -> pure ())

withPatNotesDb :: FilePath -> (Sqlite.Database -> IO a) -> IO a
withPatNotesDb path action =
  Sqlite.withSqliteDb (T.pack path) $ \db -> do
    Sqlite.exec db $
      "CREATE TABLE IF NOT EXISTS pat_notes ("
        <> "  key INTEGER PRIMARY KEY AUTOINCREMENT,"
        <> "  pat_id TEXT NOT NULL,"
        <> "  time TEXT NOT NULL,"
        <> "  text TEXT NOT NULL"
        <> ");"
    action db

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

insertPatNote :: Sqlite.Database -> PatNote -> IO ()
insertPatNote db note = do
  let sql = "INSERT INTO pat_notes (pat_id, time, text) VALUES (:id, :time, :text)"
  Sqlite.withStatement db sql $ \stmt ->
    Sqlite.insert stmt
      [(":id", Sqlite.SQLText (unPatId (pn_patId note))),
       (":time", Sqlite.SQLText (formatUTCTime (pn_time note))),
       (":text", Sqlite.SQLText (pn_text note))]
