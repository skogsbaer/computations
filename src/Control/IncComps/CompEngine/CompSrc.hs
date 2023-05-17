{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.IncComps.CompEngine.CompSrc (
  Dep (..),
  CompSrcDep,
  CompSrcDeps,
  CompSrc (..),
  CompSrcId,
  TypedCompSrcId (..),
  compSrcId,
  typedCompSrcId,
  instanceIdFromTypedCompSrcId,
  instTextFromTypedCompSrcId,
  CompSrcInstanceId (..),
  AnyCompSrcDep,
  SomeCompSrcDep (..),
  AnyCompSrcKey,
  SomeCompSrcKey (..),
  AnyCompSrcVer,
  SomeCompSrcVer (..),
  AnyCompSrc (..),
  wrapCompSrcDep,
) where

----------------------------------------
-- LOCAL
---------------------------------------

import Control.IncComps.CompEngine.CompFlow
import Control.IncComps.CompEngine.Utils.DepMap (IsDep (..), IsDepConstraints)
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
---------------------------------------
import Control.Concurrent.STM
import Data.Data
import Data.HashSet
import Data.Hashable
import Data.Kind
import qualified Data.LargeHashable as LH
import Data.String
import qualified Data.Text as T
import GHC.Generics (Generic)

data Dep a b = Dep
  { dep_key :: a
  , dep_ver :: b
  }
  deriving (Eq, Ord, Data, Typeable, Generic, Hashable)

instance (Show k, Show v) => Show (Dep k v) where
  showsPrec p (Dep k v) = showHelper2 p "Dep" k v

instance (IsDepConstraints a, IsDepConstraints b) => IsDep (Dep a b) where
  type DepKey (Dep a b) = a
  type DepVer (Dep a b) = b
  depKey = dep_key
  depVer = dep_ver

type CompSrcDep s = Dep (CompSrcKey s) (CompSrcVer s)
type CompSrcDeps s = HashSet (CompSrcDep s)

newtype CompSrcInstanceId = CompSrcInstanceId {unCompSrcInstanceId :: T.Text}
  deriving (Eq, Ord, Generic)

instance Show CompSrcInstanceId where
  showsPrec p (CompSrcInstanceId t) = showHelper1 p "CompSrcInstanceId" t

instance Hashable CompSrcInstanceId
instance LH.LargeHashable CompSrcInstanceId

instance IsString CompSrcInstanceId where
  fromString = CompSrcInstanceId . T.pack

class (Typeable s, IsCompFlowData (CompSrcKey s), IsCompFlowData (CompSrcVer s)) => CompSrc s where
  type CompSrcReq s :: Type -> Type
  type CompSrcKey s :: Type
  type CompSrcVer s :: Type
  compSrcInstanceId :: s -> CompSrcInstanceId
  compSrcExecute :: s -> CompSrcReq s a -> IO (CompSrcDeps s, Fail a)
  compSrcUnregister :: s -> HashSet (CompSrcKey s) -> IO ()
  compSrcWaitChanges :: s -> STM (CompSrcDeps s)

data CompSrcId = CompSrcId
  { csi_type :: TypeId
  , csi_instance :: CompSrcInstanceId
  }
  deriving (Eq, Generic)

instance Show CompSrcId where
  showsPrec p (CompSrcId (TypeId t) (CompSrcInstanceId i)) =
    showHelper2 p "CompSrcId" t i

instance Hashable CompSrcId
instance LH.LargeHashable CompSrcId

newtype TypedCompSrcId a = TypedCompSrcId {unTypedCompSrcId :: CompSrcId}
  deriving (Eq, Generic)

instance Show (TypedCompSrcId a) where
  showsPrec p (TypedCompSrcId (CompSrcId (TypeId t) (CompSrcInstanceId i))) =
    showHelper2 p "TypedCompSrcId" t i

instanceIdFromTypedCompSrcId :: TypedCompSrcId a -> CompSrcInstanceId
instanceIdFromTypedCompSrcId = csi_instance . unTypedCompSrcId

instTextFromTypedCompSrcId :: TypedCompSrcId a -> T.Text
instTextFromTypedCompSrcId = unCompSrcInstanceId . instanceIdFromTypedCompSrcId

compSrcId :: forall s. CompSrc s => s -> CompSrcId
compSrcId s = unTypedCompSrcId (typedCompSrcId (Proxy @s) (compSrcInstanceId s))

typedCompSrcId :: CompSrc s => Proxy s -> CompSrcInstanceId -> TypedCompSrcId s
typedCompSrcId p instId =
  let i = CompSrcId (identifyProxy p) instId
   in TypedCompSrcId i

newtype SomeCompSrcDep s = SomeCompSrcDep {unSomeCompSrcDep :: (CompSrcDep s)}
deriving newtype instance CompSrc s => Show (SomeCompSrcDep s)
deriving newtype instance CompSrc s => Eq (SomeCompSrcDep s)
deriving newtype instance CompSrc s => Hashable (SomeCompSrcDep s)

-- deriving newtype instance CompSrc s => LH.LargeHashable (SomeCompSrcDep s)
type AnyCompSrcDep = ForAnyCompFlow CompSrc CompSrcId SomeCompSrcDep

newtype SomeCompSrcKey s = SomeCompSrcKey {unSomeCompSrcKey :: (CompSrcKey s)}
deriving newtype instance CompSrc s => Show (SomeCompSrcKey s)
deriving newtype instance CompSrc s => Eq (SomeCompSrcKey s)
deriving newtype instance CompSrc s => Hashable (SomeCompSrcKey s)

-- deriving newtype instance CompSrc s => LH.LargeHashable (SomeCompSrcKey s)
type AnyCompSrcKey = ForAnyCompFlow CompSrc CompSrcId SomeCompSrcKey

newtype SomeCompSrcVer s = SomeCompSrcVer {unSomeCompSrcVer :: (CompSrcVer s)}
deriving newtype instance CompSrc s => Show (SomeCompSrcVer s)
deriving newtype instance CompSrc s => Eq (SomeCompSrcVer s)
deriving newtype instance CompSrc s => Hashable (SomeCompSrcVer s)

-- deriving newtype instance CompSrc s => LH.LargeHashable (SomeCompSrcVer s)
type AnyCompSrcVer = ForAnyCompFlow CompSrc CompSrcId SomeCompSrcVer

instance CompSrc s => IsDep (SomeCompSrcDep s) where
  type DepKey (SomeCompSrcDep s) = SomeCompSrcKey s
  type DepVer (SomeCompSrcDep s) = SomeCompSrcVer s
  depKey (SomeCompSrcDep d) = SomeCompSrcKey (dep_key d)
  depVer (SomeCompSrcDep d) = SomeCompSrcVer (dep_ver d)

instance IsDep AnyCompSrcDep where
  type DepKey AnyCompSrcDep = AnyCompSrcKey
  type DepVer AnyCompSrcDep = AnyCompSrcVer
  depKey (ForAnyCompFlow i p d) = ForAnyCompFlow i p (depKey d)
  depVer (ForAnyCompFlow i p d) = ForAnyCompFlow i p (depVer d)

wrapCompSrcDep :: forall s. CompSrc s => s -> CompSrcDep s -> AnyCompSrcDep
wrapCompSrcDep s d =
  ForAnyCompFlow (compSrcId s) (Proxy @s) (SomeCompSrcDep d)

data AnyCompSrc = forall s. CompSrc s => AnyCompSrc s
