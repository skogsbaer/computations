{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Control.IncComps.Demos.Hospital.Main (
  HospitalPipelineOptions(..), hospitalPipeline,
  HospitalSimulationOptions(..), hospitalSimulation
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine
import Control.IncComps.Demos.Hospital.CompDefs
import Control.IncComps.Utils.TimeUtils
import Control.IncComps.Utils.Types
import Control.IncComps.Demos.Hospital.PatDb
import Control.IncComps.Demos.Hospital.PatNotesDb
import Control.IncComps.Demos.Hospital.Simulation

----------------------------------------
-- EXTERNAL
----------------------------------------
import Control.Concurrent.STM
import System.Directory
import Control.Monad.Extra
import System.FilePath

data HospitalPipelineOptions = HospitalPipelineOptions
  { hpo_configDir :: FilePath
  , hpo_rootDir :: FilePath
  }

hospitalPipeline :: HospitalPipelineOptions -> IO ()
hospitalPipeline opts = do
  paths <- setup opts
  runVar <- newTVarIO None
  compDriver runVar (withCompFlows paths) (defineComps TimeInterval10s) ()

data HospitalSimulationOptions = HospitalSimulationOptions
  { hso_rootDir :: FilePath
  }

hospitalSimulation :: HospitalSimulationOptions -> IO ()
hospitalSimulation opts = do
  (_outDir, patDb, patNotesDb) <- setupRoot (hso_rootDir opts)
  simMain patDb patNotesDb defaultSimArgs

setup :: HospitalPipelineOptions -> IO HospitalPaths
setup opts = do
  let cfgDir = hpo_configDir opts
  unlessM (doesDirectoryExist cfgDir) $
    fail ("Configuration directory " ++ cfgDir ++ " does not exists")
  (outDir, patDb, patNotesDb) <- setupRoot (hpo_rootDir opts)
  pure HospitalPaths
    { hp_outDir = outDir
    , hp_patDb = patDb
    , hp_patNotesDb = patNotesDb
    , hp_configDir = cfgDir
    }

setupRoot :: FilePath -> IO (FilePath, FilePath, FilePath)
setupRoot root = do
  let outDir = root </> "data"
      patDb = root </> "pats.sqlite"
      patNotesDb = root </> "pat_notes.sqlite"
  createDirectoryIfMissing True outDir
  setupPatDb patDb
  setupPatNotesDb patNotesDb
  pure (outDir, patDb, patNotesDb)
