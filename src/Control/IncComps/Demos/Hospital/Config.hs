module Control.IncComps.Demos.Hospital.Config (

  Config(..),
  parseConfig

                                              ) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine
import Control.IncComps.FlowImpls.CompLogging
import Control.IncComps.FlowImpls.FileSink
import Control.IncComps.FlowImpls.FileSrc
import Control.IncComps.FlowImpls.IOSink
import Control.IncComps.Utils.Fail
import Control.IncComps.Utils.IOUtils
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.Types
import Control.IncComps.Demos.Hospital.PatTypes

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Proxy
import Data.Time.Clock
import System.Directory
import System.FilePath
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.LargeHashable

data Config = Config
  { c_recentTimeSpan :: TimeSpan
    -- ^ A patient is "recent" if he/she was admitted max c_recentTimeSpan time before now.
  , c_visibleAfterDischarge :: TimeSpan
    -- ^ A patient is visible if he/she is not dischared or the discharge time is max
    -- c_visibleAfterDischarge in the past.
  , c_visibleBeforeAdmission :: TimeSpan
    -- ^ A patient is visible if the admission date is in the past or max
    -- c_visibleBeforeAdmission in the future.
  }
  deriving (Eq, Show, Generic)

instance LargeHashable Config

parseConfig :: BS.ByteString -> Fail Config
parseConfig = undefined
