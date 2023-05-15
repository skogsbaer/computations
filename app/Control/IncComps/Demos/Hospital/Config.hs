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

import qualified Data.ByteString as BS
import Data.LargeHashable
import GHC.Generics (Generic)

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

parseConfig :: BS.ByteString -> Fail Config
parseConfig = undefined -- FIXME
