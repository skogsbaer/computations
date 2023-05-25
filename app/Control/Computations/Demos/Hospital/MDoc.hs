module Control.Computations.Demos.Hospital.MDoc (
  MDoc (..),
  MSection (..),
  MContent (..),
  MList (..),
  URL (..),
  MListItem (..),
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Computations.Utils.StrictList
import Data.Aeson
import Data.Hashable
import Data.LargeHashable
import qualified Data.Text as T
import GHC.Generics (Generic)

data MDoc = MDoc
  { m_title :: T.Text
  , m_sections :: SL MSection
  }
  deriving (Generic)

instance ToJSON MDoc

data MSection = MSection
  { ms_title :: Option T.Text
  , ms_content :: SL MContent
  }
  deriving (Generic)

instance ToJSON MSection

data MContent
  = MContentList MList
  | MContentText T.Text

instance ToJSON MContent where
  toJSON x =
    case x of
      MContentList l -> toJSON l
      MContentText t -> toJSON t

data MList = MList
  {ml_items :: SL MListItem}
  deriving (Generic)

instance ToJSON MList where
  toJSON (MList l) = toJSON l

newtype URL = URL T.Text
  deriving newtype (Eq, Show, Hashable, LargeHashable, ToJSON)

data MListItem = MListItem
  { ml_url :: Option URL
  , ml_text :: T.Text
  }
  deriving (Generic)

instance ToJSON MListItem
