{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Control.IncComps.Demos.Main (main) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Demos.DirSync.Main
import Control.IncComps.Demos.Hospital.Main
import Control.IncComps.Demos.Tests
import Control.IncComps.Utils.Logging

----------------------------------------

----------------------------------------
-- EXTERNAL
----------------------------------------
import Options.Applicative

data Options = Options
  { opt_logLevel :: LogLevel
  , opt_command :: Command
  }

data Command
  = DirSync DirSyncOptions
  | HospitalPipeline HospitalPipelineOptions
  | HospitalSimulation HospitalSimulationOptions
  | HospitalVisiblePats HospitalVisiblePatsOptions
  | RunTests TestOptions

data DirSyncOptions = DirSyncOptions
  { dso_sourceDir :: FilePath
  , dso_targetDir :: FilePath
  }

data TestOptions = TestOptions -- empty for now

optionsParser :: Parser Options
optionsParser = do
  opt_logLevel <-
    option
      (eitherReader parseLogLevel)
      ( long "log-level"
          <> metavar "LEVEL"
          <> showDefault
          <> value NOTE
          <> help ("Log level, possible values: " ++ allLogLevels)
      )
  opt_command <-
    hsubparser
      ( command
          "sync"
          ( DirSync
              <$> info
                syncCommand
                (progDesc "Demo syncing a directory to some other directory")
          )
          <> command
            "test"
            ( RunTests
                <$> info testCommand (progDesc "Run the tests")
            )
          <> command
            "hospital-pipeline"
            ( HospitalPipeline
                <$> info hospitalPipelineCommand (progDesc "Run hospital pipeline")
            )
          <> command
            "hospital-simulation"
            ( HospitalSimulation
                <$> info hospitalSimulationCommand (progDesc "Run hospital simulation")
            )
          <> command
            "hospital-visible-pats"
            ( HospitalVisiblePats
                <$> info hospitalVisiblePatsCommand (progDesc "Display patients visible at a certain time")
            )
      )
  pure Options{..}
 where
  syncCommand :: Parser DirSyncOptions
  syncCommand = do
    dso_sourceDir <- dirOpt "source" "Source directory"
    dso_targetDir <- dirOpt "target" "Target directory"
    pure DirSyncOptions{..}
  hospitalPipelineCommand :: Parser HospitalPipelineOptions
  hospitalPipelineCommand = do
    hpo_rootDir <-
      dirOpt "root" $
        "Root directory with subdirectory for JSON output. "
          ++ "Also holds sqlite databases for patient data and patient notes."
    hpo_configDir <- dirOpt "config" "Directory with configuration files"
    pure (HospitalPipelineOptions{..})
  hospitalSimulationCommand :: Parser HospitalSimulationOptions
  hospitalSimulationCommand = do
    hso_rootDir <-
      dirOpt
        "root"
        "Root directory with sqlite databases for patient data and patient notes."
    pure (HospitalSimulationOptions{..})
  hospitalVisiblePatsCommand :: Parser HospitalVisiblePatsOptions
  hospitalVisiblePatsCommand = do
    hvo_rootDir <-
      dirOpt "root" $
        "Root directory with subdirectory for JSON output. "
          ++ "Also holds sqlite databases for patient data and patient notes."
    hvo_configDir <- dirOpt "config" "Directory with configuration files"
    hvo_time <-
      strOption
        ( long "time"
            <> metavar "TIME_STRING"
            <> help "Time in UTC at which visible patients should be computed"
        )
    pure (HospitalVisiblePatsOptions{..})
  testCommand :: Parser TestOptions
  testCommand = pure TestOptions

dirOpt :: String -> String -> Parser FilePath
dirOpt name helpText =
  strOption
    ( long name
        <> metavar "DIR"
        <> help helpText
    )

main :: IO ()
main =
  do
    opts <- execParser cmdlineParser
    setupLogging (opt_logLevel opts)
    case opt_command opts of
      DirSync syncOpts -> syncDirs (dso_sourceDir syncOpts) (dso_targetDir syncOpts)
      RunTests _ -> testMain
      HospitalPipeline opts -> hospitalPipeline opts
      HospitalSimulation opts -> hospitalSimulation opts
      HospitalVisiblePats opts -> visiblePats opts
 where
  cmdlineParser =
    info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "Demos for incremental computations"
      )
