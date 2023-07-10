{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Computations.Demos.Main (main) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Demos.DirSync.Main
import Control.Computations.Demos.Hospital.Main
import Control.Computations.Demos.Simple.Main
import Control.Computations.Demos.Tests
import Control.Computations.Utils.Logging

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
  | Simple SimpleOptions
  | HospitalPipeline HospitalPipelineOptions
  | HospitalSimulation HospitalSimulationOptions
  | HospitalVisiblePats HospitalVisiblePatsOptions
  | HospitalServer HospitalServerOptions
  | RunTests TestOptions

data DirSyncOptions = DirSyncOptions
  { dso_sourceDir :: FilePath
  , dso_targetDir :: FilePath
  }

data SimpleOptions = SimpleOptions --empty for now

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
          "simple"
          ( Simple
              <$> info
                simpleCommand
                (progDesc "Simple demo counting lines in files")
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
          <> command
            "hospital-server"
            ( HospitalServer
                <$> info hospitalServerCommand (progDesc "Start web server for hospital demo")
            )
      )
  pure Options{..}
 where
  syncCommand :: Parser DirSyncOptions
  syncCommand = do
    dso_sourceDir <- dirOpt "source" "Source directory"
    dso_targetDir <- dirOpt "target" "Target directory"
    pure DirSyncOptions{..}
  simpleCommand :: Parser SimpleOptions
  simpleCommand = pure SimpleOptions
  hospitalPipelineCommand :: Parser HospitalPipelineOptions
  hospitalPipelineCommand = do
    hpo_rootDir <-
      dirOpt "root" $
        "Root directory with subdirectory for output. "
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
        "Root directory with subdirectory for output. "
          ++ "Also holds sqlite databases for patient data and patient notes."
    hvo_configDir <- dirOpt "config" "Directory with configuration files"
    hvo_time <-
      strOption
        ( long "time"
            <> metavar "TIME_STRING"
            <> help "Time in UTC at which visible patients should be computed"
        )
    pure (HospitalVisiblePatsOptions{..})
  hospitalServerCommand :: Parser HospitalServerOptions
  hospitalServerCommand = do
    hso_outDir <- dirOpt "out" "Output directory"
    hso_webappDir <- dirOpt "web" "Web application directory (usually webapp)"
    pure (HospitalServerOptions{..})
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
      Simple _ -> simpleMain
      RunTests _ -> testMain
      HospitalPipeline opts -> hospitalPipeline opts
      HospitalSimulation opts -> hospitalSimulation opts
      HospitalVisiblePats opts -> visiblePats opts
      HospitalServer opts -> hospitalServer opts
 where
  cmdlineParser =
    info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "Demos for incremental computations"
      )
