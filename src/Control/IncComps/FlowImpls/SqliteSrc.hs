{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | `CompSrc` for append-only tables of an sqlite database. Each table
 must define a column so that the value of the column increases with
 every new row appended to the table (typically a timestampo).
 Update operations are not supported.
 For simplicity, a single `SqliteSrc` only supports one table.
-}
module Control.IncComps.FlowImpls.SqliteSrc (
  SqliteSrcCfg (..),
  SqliteSrcReq (..),
  SqliteSrc,
  SqliteDep,
  RowFilter (..),
  SQLData,
  initSqliteSrc,
  closeSqliteSrc,
  withSqliteSrc,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.Utils.Clock
import Control.IncComps.Utils.IOUtils
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.SqliteUtils
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Int
import Data.LargeHashable
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Database.SQLite3 as Sqlite
import GHC.Generics (Generic)
import System.FilePath
import Test.Framework

data SqliteSrcCfg = SqliteSrcCfg
  { ssc_ident :: CompSrcInstanceId
  , ssc_fileName :: FilePath
  , ssc_tableName :: TableName
  , ssc_increasingCol :: ColumnName
  -- ^ Name of the column used as increasing value for the NewRowsSince request.
  , ssc_filterCol :: ColumnName
  -- ^ Name of the column being used as filter for the AllData request.
  , ssc_pollInterval :: TimeSpan
  -- We use polling because the Haskell binding does not support sqlite3_update_hook
  }
  deriving (Show)

data SqliteSrcReq a where
  NewRowsSince :: Option SQLData -> SqliteSrcReq [SQLRow]
  AllData :: RowFilter -> SqliteSrcReq [SQLRow]

data RowFilter = RowFilter
  { rf_columnValue :: SQLData
  }
  deriving (Eq, Show, Generic)

data SqliteDepKey
  = SqliteDepKeyNewRowsSince
  | SqliteDepKeyAllData RowFilter
  deriving (Eq, Show, Generic)

instance Hashable RowFilter
instance LargeHashable RowFilter
instance Hashable SqliteDepKey
instance LargeHashable SqliteDepKey

-- The version of the dependency is the largest value of the increasing column.
type SqliteDep = Dep SqliteDepKey SQLData

data SqliteSrc = SqliteSrc
  { ss_cfg :: SqliteSrcCfg
  , ss_database :: Sqlite.Database
  , ss_newRowsStmt :: Sqlite.Statement
  , ss_newRowsNoLastStmt :: Sqlite.Statement
  , ss_allDataStmt :: Sqlite.Statement
  , ss_watcherStmt :: Sqlite.Statement
  , ss_watcherStmtNoLast :: Sqlite.Statement
  , ss_changesVar :: TVar (HashMap SqliteDepKey SQLData)
  , ss_thread :: Async ()
  }

initSqliteSrc :: SqliteSrcCfg -> IO SqliteSrc
initSqliteSrc cfg =
  do
    logDebug ("Initializing sqlite src with config " ++ show cfg)
    db <-
      Sqlite.open2
        (T.pack (ssc_fileName cfg))
        [Sqlite.SQLOpenReadOnly, Sqlite.SQLOpenFullMutex]
        Sqlite.SQLVFSDefault
    let orderBy = " ORDER BY " <> ssc_increasingCol cfg <> " ASC"
        newRowsQuery =
          "SELECT * FROM "
            <> ssc_tableName cfg
            <> " WHERE "
            <> ssc_increasingCol cfg
            <> " > :last"
            <> orderBy
        newRowsNoLastQuery =
          "SELECT * FROM " <> ssc_tableName cfg <> orderBy
        allDataQuery =
          "SELECT * FROM "
            <> ssc_tableName cfg
            <> " WHERE "
            <> ssc_filterCol cfg
            <> " = :filter"
            <> orderBy
        watcherBaseQuery =
          "SELECT "
            <> ssc_increasingCol cfg
            <> ", "
            <> ssc_filterCol cfg
            <> " FROM "
            <> ssc_tableName cfg
        watcherQueryNoLast = watcherBaseQuery <> orderBy
        watcherQuery = watcherBaseQuery <> " WHERE " <> ssc_increasingCol cfg <> " > :last" <> orderBy
    newRowsStmt <- Sqlite.prepare db newRowsQuery
    newRowsNoLastStmt <- Sqlite.prepare db newRowsNoLastQuery
    allDataStmt <- Sqlite.prepare db allDataQuery
    watcherStmt <- Sqlite.prepare db watcherQuery
    watcherStmtNoLast <- Sqlite.prepare db watcherQueryNoLast
    changesVar <- newTVarIO HashMap.empty
    thread <- async (watcher cfg watcherStmt watcherStmtNoLast changesVar)
    pure $
      SqliteSrc
        { ss_cfg = cfg
        , ss_database = db
        , ss_newRowsStmt = newRowsStmt
        , ss_newRowsNoLastStmt = newRowsNoLastStmt
        , ss_allDataStmt = allDataStmt
        , ss_watcherStmt = watcherStmt
        , ss_watcherStmtNoLast = watcherStmtNoLast
        , ss_changesVar = changesVar
        , ss_thread = thread
        }

closeSqliteSrc :: SqliteSrc -> IO ()
closeSqliteSrc src =
  do
    logDebug ("Closing sqlite src " ++ show (compSrcInstanceId src))
    cancel (ss_thread src)
    Sqlite.finalize (ss_newRowsStmt src)
    Sqlite.finalize (ss_newRowsNoLastStmt src)
    Sqlite.finalize (ss_allDataStmt src)
    Sqlite.finalize (ss_watcherStmt src)
    Sqlite.finalize (ss_watcherStmtNoLast src)
    closeSqliteDb (ss_database src)

withSqliteSrc :: SqliteSrcCfg -> (SqliteSrc -> IO a) -> IO a
withSqliteSrc cfg action =
  bracket
    (initSqliteSrc cfg)
    closeSqliteSrc
    action

instance CompSrc SqliteSrc where
  type CompSrcReq SqliteSrc = SqliteSrcReq
  type CompSrcKey SqliteSrc = SqliteDepKey
  type CompSrcVer SqliteSrc = SQLData
  compSrcInstanceId src = ssc_ident (ss_cfg src)
  compSrcExecute = executeImpl
  compSrcUnregister = unregisterImpl
  compSrcWaitChanges = waitChangesImpl

watcher :: SqliteSrcCfg -> Sqlite.Statement -> Sqlite.Statement -> TVar (HashMap SqliteDepKey SQLData) -> IO ()
watcher cfg stmt stmtNoLast changesVar =
  do
    let loop last =
          do
            logTrace ("Sqlite.watcher, last=" ++ show last)
            newLast <- performStep last
            c_sleep realClock (ssc_pollInterval cfg)
            loop newLast
    loop Nothing
 where
  handleRow
    :: HashMap SqliteDepKey SQLData
    -> SQLRow
    -> IO (HashMap SqliteDepKey SQLData)
  handleRow m row =
    do
      incColVal <- getColumnValue row (ssc_increasingCol cfg)
      filterColVal <- getColumnValue row (ssc_filterCol cfg)
      let key1 = SqliteDepKeyNewRowsSince
          key2 = SqliteDepKeyAllData (RowFilter filterColVal)
      pure (HashMap.insertWith max key1 incColVal (HashMap.insertWith max key2 incColVal m))
  performStep mLast =
    do
      l <-
        case mLast of
          Nothing -> retryIfBusy "watcher.query" $ query stmtNoLast []
          Just last -> retryIfBusy "watcher.query" $ query stmt [(":last", last)]
          `catch` ( \(e :: Sqlite.SQLError) -> do
                      if Sqlite.sqlError e == Sqlite.ErrorBusy
                        then logDebug ("sqlite watcher: " ++ show e)
                        else logWarn ("sqlite watcher: " ++ show e)
                      pure []
                  )
      depMap <- foldM handleRow HashMap.empty l
      unless (HashMap.null depMap) $
        logDebug ("New dependencies for " ++ show cfg ++ ": " ++ show depMap)
      atomically $ modifyTVar' changesVar (HashMap.unionWith max depMap)
      pure $ (HashMap.lookup SqliteDepKeyNewRowsSince depMap) `mplus` mLast

executeImpl :: SqliteSrc -> SqliteSrcReq a -> IO (HashSet SqliteDep, Fail a)
executeImpl src req =
  case req of
    NewRowsSince last -> execNewRowsSince last
    AllData rf -> execAllData rf
 where
  exec :: Sqlite.Statement -> [(T.Text, SQLData)] -> SqliteDepKey -> IO (HashSet SqliteDep, Fail [SQLRow])
  exec stmt bindings depKey =
    do
      rows <- query stmt bindings
      case reverse rows of
        [] -> pure (HashSet.empty, Ok [])
        last : _ ->
          do
            let col = ssc_increasingCol (ss_cfg src)
            case L.lookup col last of
              Just lastData ->
                pure (HashSet.singleton (Dep depKey lastData), Ok rows)
              Nothing ->
                do
                  let msg = "SqliteSrc: result request does not contain column " ++ show col
                  logWarn msg
                  pure (HashSet.empty, Fail msg)
      `catch` \(e :: IOException) ->
        do
          let msg = "Error interactive with sqlite: " ++ show e
          logWarn msg
          pure (HashSet.empty, Fail msg)
  execNewRowsSince :: Option SQLData -> IO (HashSet SqliteDep, Fail [SQLRow])
  execNewRowsSince mLast =
    case mLast of
      None -> exec (ss_newRowsNoLastStmt src) [] SqliteDepKeyNewRowsSince
      Some last -> exec (ss_newRowsStmt src) [(":last", last)] SqliteDepKeyNewRowsSince
  execAllData :: RowFilter -> IO (HashSet SqliteDep, Fail [SQLRow])
  execAllData rf = do
    exec (ss_allDataStmt src) [(":filter", rf_columnValue rf)] (SqliteDepKeyAllData rf)

unregisterImpl :: SqliteSrc -> HashSet SqliteDepKey -> IO ()
unregisterImpl _ _ = pure ()

waitChangesImpl :: SqliteSrc -> STM (HashSet SqliteDep)
waitChangesImpl src =
  do
    depMap <- readTVar (ss_changesVar src)
    writeTVar (ss_changesVar src) HashMap.empty
    let srcIdent = show (compSrcInstanceId src)
    if HashMap.null depMap
      then do
        logTraceSTM ("no changes in sqlite " ++ srcIdent)
        retry
      else do
        logDebugSTM
          ( show (HashMap.size depMap)
              ++ " changes in sqlite src "
              ++ srcIdent
          )
        pure $ HashSet.fromList (map (\(k, v) -> Dep k v) (HashMap.toList depMap))

--
-- Tests
--

test_basics :: IO ()
test_basics = loop 1 -- run basicTest several times because it's not deterministic
 where
  numRuns = 100
  loop :: Int -> IO ()
  loop n = do
    logNote ("Test run " ++ show n)
    basicTest
    when (n <= numRuns) $ loop (n + 1)

basicTest :: IO ()
basicTest =
  withSysTempDir $ \rootDir ->
    do
      let dbPath = T.pack (rootDir </> "test.sqlite")
      logInfo ("Starting sqlite test, dbPath=" ++ show dbPath)
      withSqliteDb dbPath $ \db ->
        do
          Sqlite.exec db $
            "CREATE TABLE test_table ("
              <> "  id TEXT NOT NULL,"
              <> "  data INTEGER,"
              <> "  time INTEGER PRIMARY KEY AUTOINCREMENT"
              <> ");"
          let cfg =
                SqliteSrcCfg
                  { ssc_ident = "testSqliteSrc"
                  , ssc_fileName = T.unpack dbPath
                  , ssc_tableName = "test_table"
                  , ssc_increasingCol = "time"
                  , ssc_filterCol = "id"
                  , ssc_pollInterval = pollInterval
                  }
          logInfo ("DB created successfully, cfg=" ++ show cfg)
          withSqliteSrc cfg $ \src ->
            do
              logInfo "Testing empty"
              let emptyRes = (HashSet.empty, Ok [])
              res1 <- executeImpl src (NewRowsSince None)
              assertEqual emptyRes res1
              res2 <- executeImpl src (AllData (mkRowFilter "foo"))
              assertEqual emptyRes res2
              logInfo "Testing insert 1"
              let list1 = [("foo", 1), ("bar", 3), ("foo", 2)]
              insert db list1
              Just tNewRows1 <- subAssert $ checkExec src (NewRowsSince None) SqliteDepKeyNewRowsSince list1
              Just tFoo1 <-
                subAssert $
                  checkExec
                    src
                    (AllData (mkRowFilter "foo"))
                    (SqliteDepKeyAllData (mkRowFilter "foo"))
                    [("foo", 1), ("foo", 2)]
              Just tBar1 <-
                subAssert $
                  checkExec
                    src
                    (AllData (mkRowFilter "bar"))
                    (SqliteDepKeyAllData (mkRowFilter "bar"))
                    [("bar", 3)]
              assertEqual tNewRows1 tFoo1
              assertBool (tBar1 < tFoo1)
              logInfo ("Checking changes 1a")
              waitForChanges
              changes1 <- atomically $ waitChangesImpl src
              assertEqual
                ( HashSet.fromList
                    [ Dep SqliteDepKeyNewRowsSince tNewRows1
                    , Dep (SqliteDepKeyAllData (mkRowFilter "foo")) tFoo1
                    , Dep (SqliteDepKeyAllData (mkRowFilter "bar")) tBar1
                    ]
                )
                changes1
              logInfo ("Checking changes 1b")
              waitForChanges
              changes2 <- atomically $ waitChangesImpl src `orElse` pure HashSet.empty
              assertEqualVerbose ("changes2=" ++ show changes2) 0 (HashSet.size changes2)
              logInfo "Testing insert 2"
              let list2 = [("bar", 4)]
              insert db list2
              Just tNewRows2 <- subAssert $ checkExec src (NewRowsSince (Some tNewRows1)) SqliteDepKeyNewRowsSince list2
              Just tFoo2 <-
                subAssert $
                  checkExec
                    src
                    (AllData (mkRowFilter "foo"))
                    (SqliteDepKeyAllData (mkRowFilter "foo"))
                    [("foo", 1), ("foo", 2)]
              assertEqual tFoo1 tFoo2
              assertBool (tFoo1 < tNewRows2)
              Just tBar2 <-
                subAssert $
                  checkExec
                    src
                    (AllData (mkRowFilter "bar"))
                    (SqliteDepKeyAllData (mkRowFilter "bar"))
                    [("bar", 3), ("bar", 4)]
              assertEqual tNewRows2 tBar2
              logInfo ("Checking changes 2")
              waitForChanges
              changes3 <- atomically $ waitChangesImpl src
              assertEqual
                ( HashSet.fromList
                    [ Dep SqliteDepKeyNewRowsSince tNewRows2
                    , Dep (SqliteDepKeyAllData (mkRowFilter "bar")) tBar2
                    ]
                )
                changes3
              logInfo "At end of test"
 where
  pollInterval = milliseconds 10
  waitForChanges = c_sleep realClock (2 * pollInterval)
  mkRowFilter :: T.Text -> RowFilter
  mkRowFilter t = RowFilter (Sqlite.SQLText t)
  insert :: Sqlite.Database -> [(T.Text, Int64)] -> IO ()
  insert db list = do
    forM_ list $ \(id, n) ->
      do
        retryIfBusy "test.insert" $
          Sqlite.exec
            db
            ( T.pack
                ( "INSERT INTO test_table (id, data) VALUES ("
                    ++ show id
                    ++ ", "
                    ++ show n
                    ++ ");"
                )
            )
  checkExec :: SqliteSrc -> SqliteSrcReq [SQLRow] -> SqliteDepKey -> [(T.Text, Int64)] -> IO (Maybe SQLData)
  checkExec src req depKey expectedData = do
    (deps, Ok res) <- executeImpl src req
    res' <-
      forM res $ \row -> do
        Sqlite.SQLText idVal <- getColumnValue row "id"
        Sqlite.SQLInteger dataVal <- getColumnValue row "data"
        pure (idVal, dataVal)
    assertEqual expectedData res'
    hiTime <-
      case reverse res of
        [] -> pure Nothing
        lastRow : _ -> do
          timeVal <- getColumnValue lastRow "time"
          pure (Just timeVal)
    assertEqual (map (Dep depKey) (maybeToList hiTime)) (HashSet.toList deps)
    pure hiTime
