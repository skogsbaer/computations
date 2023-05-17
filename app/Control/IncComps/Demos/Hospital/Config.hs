module Control.IncComps.Demos.Hospital.Config (
  Config (..),
  defaultConfig,
  parseConfig,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Utils.Fail
import Control.IncComps.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.LargeHashable
import GHC.Generics (Generic)
import Safe

data Config = Config
  { c_recentTimeSpan :: TimeSpan
  -- ^ A patient is "recent" if he/she was admitted max c_recentTimeSpan time before now.
  , c_visibleAfterDischarge :: TimeSpan
  -- ^ A patient is visible if he/she is not dischared or the discharge time is max
  -- c_visibleAfterDischarge in the past. The maximum value for this setting is one day.
  , c_visibleBeforeAdmission :: TimeSpan
  -- ^ A patient is visible if the admission date is in the past or max
  -- c_visibleBeforeAdmission in the future.
  }
  deriving (Eq, Show, Generic)

instance LargeHashable Config

defaultConfig :: Config
defaultConfig =
  Config
    { c_recentTimeSpan = hours 1
    , c_visibleAfterDischarge = days 1
    , c_visibleBeforeAdmission = hours 1
    }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> do
      recent <- getTimeSpan v "recentTime"
      afterDischarge <- getTimeSpan v "afterDischarge"
      beforeAdmission <- getTimeSpan v "beforeAdmission"
      pure $ Config {
          c_recentTimeSpan =
             fromMaybe (c_recentTimeSpan defaultConfig) recent
        , c_visibleAfterDischarge =
            fromMaybe (c_visibleAfterDischarge defaultConfig) afterDischarge
        , c_visibleBeforeAdmission =
            fromMaybe (c_visibleBeforeAdmission defaultConfig) beforeAdmission
        }
      where
        getTimeSpan v k = do
          ms <- v .:? k
          case ms of
            Nothing -> pure Nothing
            Just s ->
              case readMay s of
                Just ts -> pure $ Just (ts :: TimeSpan)
                Nothing -> fail ("Invalid timespan: " ++ s)

parseConfig :: BS.ByteString -> Fail Config
parseConfig bs =
  case eitherDecode (BSL.fromStrict bs) of
    Right cfg -> Ok cfg
    Left err -> Fail ("Error parsing config: " ++ err)
