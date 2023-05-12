module Control.IncComps.Demos.Main (main) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Demos.DirSync.Main
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

data DirSyncOptions = DirSyncOptions
  { dso_sourceDir :: FilePath
  , dso_targetDir :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      (eitherReader parseLogLevel)
      ( long "log-level"
          <> metavar "LEVEL"
          <> showDefault
          <> value NOTE
          <> help ("Log level, possible values: " ++ allLogLevels)
      )
    <*> hsubparser
      ( command "sync" (DirSync <$> info syncCommand (progDesc "Demo syncing a directory to some other directory"))
      -- <> command "commit" (info commitCommand ( progDesc "Record changes to the repository" ))
      )
 where
  syncCommand :: Parser DirSyncOptions
  syncCommand =
    DirSyncOptions
      <$> strOption
        ( long "source"
            <> metavar "DIR"
            <> help "Source directory"
        )
      <*> strOption
        ( long "target"
            <> metavar "DIR"
            <> help "Target directory"
        )

main :: IO ()
main =
  do
    opts <- execParser cmdlineParser
    setupLogging (opt_logLevel opts)
    case opt_command opts of
      DirSync syncOpts -> syncDirs (dso_sourceDir syncOpts) (dso_targetDir syncOpts)
 where
  cmdlineParser =
    info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "Demos for incremental computations"
      )
