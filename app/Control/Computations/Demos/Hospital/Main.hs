{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Control.Computations.Demos.Hospital.Main (
  HospitalPipelineOptions (..),
  hospitalPipeline,
  HospitalSimulationOptions (..),
  hospitalSimulation,
  HospitalVisiblePatsOptions (..),
  visiblePats,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.Demos.Hospital.CompDefs
import Control.Computations.Demos.Hospital.Config
import Control.Computations.Demos.Hospital.PatDb
import Control.Computations.Demos.Hospital.PatNotesDb
import Control.Computations.Demos.Hospital.PatTypes
import Control.Computations.Demos.Hospital.Simulation
import Control.Computations.Utils.Logging
import qualified Control.Computations.Utils.SqliteUtils as Sqlite
import Control.Computations.Utils.TimeUtils
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad.Extra
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time.Clock
import System.Directory
import System.FilePath

data HospitalPipelineOptions = HospitalPipelineOptions
  { hpo_configDir :: FilePath
  , hpo_rootDir :: FilePath
  }

hospitalPipeline :: HospitalPipelineOptions -> IO ()
hospitalPipeline opts = do
  paths <- setup (hpo_rootDir opts) (hpo_configDir opts)
  runVar <- newTVarIO None
  compDriver runVar (withCompFlows paths) (defineComps TimeInterval10s) ()

data HospitalSimulationOptions = HospitalSimulationOptions
  { hso_rootDir :: FilePath
  }

hospitalSimulation :: HospitalSimulationOptions -> IO ()
hospitalSimulation opts = do
  (_outDir, patDb, patNotesDb) <- setupRoot (hso_rootDir opts)
  simMain patDb patNotesDb defaultSimArgs

setup :: FilePath -> FilePath -> IO HospitalPaths
setup rootDir cfgDir = do
  unlessM (doesDirectoryExist cfgDir) $
    fail ("Configuration directory " ++ cfgDir ++ " does not exists")
  (outDir, patDb, patNotesDb) <- setupRoot rootDir
  pure
    HospitalPaths
      { hp_outDir = outDir
      , hp_patDb = patDb
      , hp_patNotesDb = patNotesDb
      , hp_configDir = cfgDir
      }

setupRoot :: FilePath -> IO (FilePath, FilePath, FilePath)
setupRoot root = do
  let outDir = root </> "output"
      patDb = root </> "pats.sqlite"
      patNotesDb = root </> "pat_notes.sqlite"
  createDirectoryIfMissing True outDir
  setupPatDb patDb
  setupPatNotesDb patNotesDb
  pure (outDir, patDb, patNotesDb)

data HospitalVisiblePatsOptions = HospitalVisiblePatsOptions
  { hvo_rootDir :: FilePath
  , hvo_configDir :: FilePath
  , hvo_time :: T.Text
  }

visiblePats :: HospitalVisiblePatsOptions -> IO ()
visiblePats opts = do
  paths <- setup (hvo_rootDir opts) (hvo_configDir opts)
  cfg <- parseConfigFile (hp_configDir paths </> "demo.cfg")
  time <- parseUTCTime (T.unpack (hvo_time opts))
  withPatDb (hp_patDb paths) $ \db ->
    Sqlite.withStatement db "SELECT * FROM pat_msgs ORDER BY key ASC" $ \stmt -> do
      rows <- Sqlite.query stmt []
      patMap <- foldM (handleRow cfg time) HashMap.empty rows
      logNote (show (HashMap.size patMap) ++ " patients visible at " ++ show time)
      let pats = L.sortOn p_name (HashMap.elems patMap)
      forM_ pats print
 where
  handleRow :: Config -> UTCTime -> HashMap PatId Pat -> Sqlite.SQLRow -> IO (HashMap PatId Pat)
  handleRow cfg time patMap row = do
    pat <- pm_pat <$> rowToPatMsg row
    let admission =
          (p_admissionDateTime pat) `addTimeSpan` (negate (c_visibleBeforeAdmission cfg))
        discharge =
          fmap (\t -> t `addTimeSpan` (c_visibleAfterDischarge cfg)) (p_dischargeDateTime pat)
        isIn =
          ( admission <= time
              && case discharge of
                None -> True
                Some t -> time <= t
          )
    logDebug (show pat ++ ": " ++ if isIn then "IN" else "OUT")
    if isIn
      then pure $ HashMap.insert (p_patId pat) pat patMap
      else pure $ HashMap.delete (p_patId pat) patMap
