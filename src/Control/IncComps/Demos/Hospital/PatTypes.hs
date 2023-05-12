module Control.IncComps.Demos.Hospital.PatTypes where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import qualified Data.Text as T
import Data.Time.Clock
import Data.Hashable
import Data.LargeHashable
import GHC.Generics (Generic)

newtype PatId = PatId T.Text
  deriving (Eq, Show, Hashable, LargeHashable)

data Sex = SexMale | SexFemale | SexOther | SexUnknown
  deriving (Eq, Show, Generic)

instance Hashable Sex
instance LargeHashable Sex

data Name = Name
  { n_firstName :: T.Text
  , n_lastName :: T.Text
  }
  deriving (Eq, Show, Generic)

instance Hashable Name
instance LargeHashable Name

data Pat = Pat
  { p_patId :: PatId
  , p_admissionDate :: UTCTime
  , p_dischargeDate :: Option UTCTime
  , p_birtdate :: UTCTime
  , p_sex :: Sex
  , p_name :: Name
  , p_primaryDiagnose :: T.Text
  }
  deriving (Eq, Show, Generic)

instance Hashable Pat
instance LargeHashable Pat

data PatNote = PatNote
  { pn_patId :: PatId
  , pn_time :: UTCTime
  , pn_text :: T.Text
  }
  deriving (Eq, Show, Generic)

instance Hashable PatNote
instance LargeHashable PatNote
