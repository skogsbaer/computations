module Control.IncComps.Demos.Hospital.PatTypes (
  PatId (..),
  Sex (..),
  Name (..),
  Pat (..),
  PatNote (..),
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.Aeson
import Data.Hashable
import Data.LargeHashable
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics (Generic)

newtype PatId = PatId T.Text
  deriving (Eq, Show, Hashable, LargeHashable)

instance ToJSON PatId where
  toJSON (PatId t) = toJSON t

instance FromJSON PatId where
  parseJSON v = PatId <$> parseJSON v

data Sex = SexMale | SexFemale | SexOther T.Text | SexUnknown
  deriving (Eq, Show, Generic)

instance Hashable Sex
instance LargeHashable Sex
instance ToJSON Sex
instance FromJSON Sex

data Name = Name
  { n_lastName :: T.Text
  , n_firstName :: T.Text
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Name
instance LargeHashable Name
instance ToJSON Name
instance FromJSON Name

data Pat = Pat
  { p_patId :: PatId
  , p_admissionDateTime :: UTCTime
  , p_dischargeDateTime :: Option UTCTime
  , p_birtdate :: Day
  , p_sex :: Sex
  , p_name :: Name
  , p_primaryDiagnose :: T.Text
  }
  deriving (Eq, Show, Generic)

instance Hashable Pat
instance LargeHashable Pat
instance ToJSON Pat
instance FromJSON Pat

data PatNote = PatNote
  { pn_patId :: PatId
  , pn_time :: UTCTime
  , pn_text :: T.Text
  }
  deriving (Eq, Show, Generic)

instance Hashable PatNote
instance LargeHashable PatNote
