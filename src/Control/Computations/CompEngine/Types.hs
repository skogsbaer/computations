{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Computations.CompEngine.Types (
  -- * Computations
  Comp (..),
  AnyComp (..),
  CompId,
  mkCompId,
  mkCompIdWithPriority,
  mkCompIdFromText,
  CompFun,
  CompFunX,
  CompEnv (..),
  CompMap,
  IsCompParam,
  IsCompResult,

  -- * Caching of compuation results
  CompCacheBehavior (..),
  CompCacheValue (..),
  ccv_largeHash,
  AnyCompCacheValue (..),
  anyCompCacheValueApply,
  castCompCacheValue,
  CompCacheMeta (..),
  ccv_cacheSize,

  -- * Monad for building computation bodies
  CompM (..),
  CompYield (..),
  CompResult (..),
  CompReq (..),
  CompFlowReq (..),
  CompCont,
  ContCompM,
  contToCompM,
  compMFinished,

  -- * Requests
  doAnyRequest,
  compSrcReq,
  compSrcReq',
  compSinkReq,
  compSinkReq',

  -- * Dependencies
  CompDep (..),
  CompDepKey (..),
  CompDepVer (..),
  CompEngDep (..),
  CompEngDepKey (..),
  CompEngDepVer (..),
  mkCompDep,
  DepSet,

  -- * Applications of computations. `CompAp` and `Cap` are

  -- both abbreviations for "computation application"
  CompAp,
  pattern CompAp,
  cap_comp,
  cap_hash,
  cap_param,
  AnyCompAp (..),
  showAnyCompApDetails,
  CapId (..),
  CompApResult (..),
  mkCapId,
  capCompId,
  capId,
  compCacheValue,
  compApResult,
  wrapCompAp,
  anyCompApPriority,
  anyCapId,
  mkCompAp,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Utils.DepMap (IsDep (..))
import Control.Computations.CompEngine.Utils.PriorityAgingQueue (PaqPriority (..))
import Control.Computations.Utils.DataSize
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Logging
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad.Reader
import Data.Function (on)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.LargeHashable as LH
import Data.Map.Strict (Map)
import Data.Ord
import Data.String
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics (Generic)

data Comp p a = Comp
  { comp_name :: CompId
  -- ^ unique computation name
  , comp_caching :: CompCacheBehavior a
  -- ^ caching/persistence helper functions
  , comp_fun :: CompFunX p a
  -- ^ actual computation implementation function
  , comp_compMap :: CompMap
  -- ^ all computation defined until now
  }

_DEFAULT_PRIORITY_ :: PaqPriority
_DEFAULT_PRIORITY_ = PaqRegular

data CompId = CompId
  { compId_priority :: PaqPriority
  , compId_name :: T.Text
  }
  deriving (Eq, Ord, Generic)

instance Show CompId where
  showsPrec p (CompId prio name) =
    case prio of
      PaqRegular -> showHelper1 p "CompId" name
      _ -> showHelper2 p "CompId" name prio

instance Hashable CompId

mkCompId :: String -> CompId
mkCompId = CompId _DEFAULT_PRIORITY_ . T.pack

mkCompIdWithPriority :: PaqPriority -> T.Text -> CompId
mkCompIdWithPriority prio = CompId prio

mkCompIdFromText :: T.Text -> CompId
mkCompIdFromText = CompId _DEFAULT_PRIORITY_

instance IsString CompId where
  fromString = mkCompId

type CompFun p a = p -> CompM a
type CompFunX p a = CompEnv p a -> CompM a

data CompEnv p r = CompEnv
  { ce_cachedResult :: IsCompResult r => CompM (Maybe r)
  , ce_param :: p
  , ce_comp :: Comp p r
  }

type CompMap = Map CompId AnyComp
data AnyComp
  = forall p a.
    (IsCompResult a, IsCompParam p) =>
    AnyComp (Comp p a)

type IsCompParam p = (Show p, Typeable p, LH.LargeHashable p)
type IsCompResult a = (Typeable a, Show a)

newtype CompCacheBehavior a = CompCacheBehavior
  { ccb_memcache :: a -> CompCacheValue a
  }

data CompCacheValue a = CompCacheValue
  { ccv_payload :: Option a
  , ccv_meta :: CompCacheMeta
  }

ccv_largeHash :: CompCacheValue a -> Hash128
ccv_largeHash = ccm_largeHash . ccv_meta

data AnyCompCacheValue = forall a. Typeable a => AnyCompCacheValue (CompCacheValue a)

instance Show AnyCompCacheValue where
  showsPrec p (AnyCompCacheValue v) = showsPrec p v

anyCompCacheValueApply :: (forall a. CompCacheValue a -> b) -> AnyCompCacheValue -> b
anyCompCacheValueApply f (AnyCompCacheValue ccv) = f ccv

castCompCacheValue
  :: forall a b m. (MonadFail m, Typeable b, Typeable a) => CompCacheValue b -> m (CompCacheValue a)
castCompCacheValue ccv =
  case cast ccv of
    Just ok -> pure ok
    Nothing -> fail ("Couldn't cast " ++ show ccv ++ " to " ++ show targetType)
 where
  targetType = typeRep (Proxy :: Proxy a)

data CompCacheMeta = CompCacheMeta
  { ccm_largeHash :: Hash128
  , ccm_logrepr :: T.Text
  , ccm_approxCachedSize :: Option DataSize -- FIXME: what is needed from the size stuff?
  , ccm_cachedSize :: Option Int
  -- \| Size of the cached value in arbitrary unit.
  -- For example number of patients, ...
  }
  deriving (Show, Eq, Typeable)

instance Show (CompCacheValue a) where
  showsPrec p ccv =
    showParen (p > 10) $
      showString "CompCacheValue { ccv_logrepr = "
        . showString (T.unpack (ccm_logrepr (ccv_meta ccv)))
        . showString "}"

instance Eq (CompCacheValue a) where
  (==) = (==) `on` (ccm_largeHash . ccv_meta)

ccv_cacheSize :: CompCacheValue a -> Option DataSize
ccv_cacheSize = ccm_approxCachedSize . ccv_meta

-- | Monad running the body of a computation
newtype CompM a = CompM
  { runCompM :: CompMap -> (DepSet, CompYield a)
  }
  deriving (Functor)

-- | Yield a possibly intermediate result of a computation.
data CompYield a where
  CompFinished :: CompResult a -> CompYield a
  CompSuspended :: CompReq r -> CompCont r a -> CompYield a

instance Functor CompYield where
  fmap f (CompFinished ev) = CompFinished (fmap f ev)
  fmap f (CompSuspended req g) = CompSuspended req (\x -> f :<$> g x)

-- | The final return value of a computation.
data CompResult a
  = CompResultOk a
  | CompResultFail String
  deriving (Show, Eq, Ord, Functor)

-- | Data type representation of an request suspending a computation.
data CompReq r where
  CompReqCombined :: CompReq a -> CompReq b -> CompReq (a, b)
  CompReqEval :: IsCompResult a => CompAp a -> CompReq (Maybe (CompApResult a))
  CompReqCache :: IsCompResult a => CompAp a -> CompReq (Maybe a)
  CompReqFlow :: CompFlowReq a -> CompReq a

-- | Requests of a computation
data CompFlowReq a where
  CompFlowReqSink :: CompSink s => TypedCompSinkId s -> CompSinkReq s a -> CompFlowReq (Fail a)
  CompFlowReqSrc :: CompSrc s => TypedCompSrcId s -> CompSrcReq s a -> CompFlowReq (Fail a)

-- | Smart representation of the continuation `r -> CompM a`.
type CompCont r a = r -> ContCompM a

{- | A datatype representation of a `CompM` computation.
 This is to avoid repeatedly traversing the tree.
 See "A Smart View on Datatypes", Jaskelioff/Rivas, ICFP'15
-}
data ContCompM a
  = ContCompM (CompM a)
  | forall b. ContCompM b :>>= (b -> CompM a)
  | forall b. (b -> a) :<$> (ContCompM b)

instance Functor ContCompM where
  fmap = (:<$>)

instance Applicative ContCompM where
  pure = ContCompM . pure
  f <*> x = ContCompM (contToCompM f <*> contToCompM x)

instance Monad ContCompM where
  x >>= f = x :>>= (contToCompM . f)

contToCompM :: ContCompM a -> CompM a
contToCompM m =
  case m of
    (ContCompM x) -> x
    (m :>>= f) -> toCompMBind m f
    (f :<$> m) -> toCompMFmap f m
 where
  toCompMBind :: ContCompM b -> (b -> CompM a) -> CompM a
  toCompMBind (m :>>= f) k = toCompMBind m (f >=> k)
  toCompMBind (ContCompM gen) k = gen >>= k
  toCompMBind (f :<$> x) k = toCompMBind x (k . f)
  toCompMFmap :: (a -> b) -> ContCompM a -> CompM b
  toCompMFmap f (m :>>= k) = toCompMBind m (k >=> return . f)
  toCompMFmap f (ContCompM gen) = f <$> gen
  toCompMFmap f (g :<$> x) = toCompMFmap (f . g) x

instance Applicative CompM where
  pure = compMFinished . CompResultOk
  (<*>) = compMAp

instance Monad CompM where
  (>>=) = compMBind

instance MonadFail CompM where
  fail = compMFinished . CompResultFail

compMAp
  :: CompM (a -> b)
  -> CompM a
  -> CompM b
compMAp mf mb =
  CompM $ \r ->
    let !(wF, resA) = runCompM mf r
        !(wB, resB) = runCompM mb r
        -- We always track both dependencies.
        -- This is not strictly identical to the Monadic implementation,
        -- but it is still correct.
        !w' = wF <> wB
        !res =
          case (resA, resB) of
            (CompFinished (CompResultOk f), CompFinished (CompResultOk b)) ->
              -- The base case.
              CompFinished (CompResultOk (f b))
            (CompFinished (CompResultFail e), _) ->
              -- Keep only the left error. Consistent with the monadic case
              CompFinished (CompResultFail e)
            (_, CompFinished (CompResultFail e)) ->
              CompFinished (CompResultFail e)
            (CompFinished (CompResultOk f), CompSuspended req g) ->
              -- Store the finished value into the continuation
              CompSuspended req (\x -> f :<$> g x)
            (CompSuspended req g, CompFinished (CompResultOk b)) ->
              -- Same as above
              CompSuspended req (\f -> ($ b) :<$> g f)
            (CompSuspended reqF contA, CompSuspended reqB contB) ->
              -- Both computations are suspended so we combine the suspends into one.
              -- This could be used to exploit parallelism.
              CompSuspended (CompReqCombined reqF reqB) (\(f, b) -> contA f <*> contB b)
     in (w', res)

compMBind
  :: CompM a
  -> (a -> CompM b)
  -> CompM b
compMBind m f =
  CompM $ \r ->
    let !(!w, res) = runCompM m r
     in case res of
          CompFinished (CompResultFail e) -> (w, CompFinished (CompResultFail e))
          CompFinished (CompResultOk a) ->
            let (w', res') = runCompM (f a) r
                !w'' = w <> w'
             in (w'', res')
          CompSuspended req g -> (w, CompSuspended req (\x -> g x :>>= f))

compMFinished :: CompResult a -> CompM a
compMFinished ev = compMYield (CompFinished ev)

compMYield :: CompYield a -> CompM a
compMYield ret = CompM $ \_ -> (HashSet.empty, ret)

doAnyRequest :: CompReq a -> CompM a
doAnyRequest req =
  compMYield $
    CompSuspended req (ContCompM . compMFinished . CompResultOk)

compSrcReq :: CompSrc s => TypedCompSrcId s -> CompSrcReq s a -> CompM a
compSrcReq s req =
  do
    res <- compSrcReq' s req
    failInM res

compSrcReq' :: CompSrc s => TypedCompSrcId s -> CompSrcReq s a -> CompM (Fail a)
compSrcReq' s req =
  doAnyRequest $ CompReqFlow (CompFlowReqSrc s req)

compSinkReq :: CompSink s => TypedCompSinkId s -> CompSinkReq s a -> CompM a
compSinkReq s req =
  do
    res <- compSinkReq' s req
    failInM res

compSinkReq' :: CompSink s => TypedCompSinkId s -> CompSinkReq s a -> CompM (Fail a)
compSinkReq' s req =
  doAnyRequest $ CompReqFlow (CompFlowReqSink s req)

-- | Dependency resulting from calling a computation.
newtype CompDep = CompDep {unCompDep :: Dep CompDepKey CompDepVer}
  deriving newtype (Eq, Show, Hashable)

newtype CompDepKey = CompDepKey {unCompDepKey :: AnyCompAp}
  deriving newtype (Eq, Ord, Show, Hashable)

newtype CompDepVer = CompDepVer {unCompDepVer :: Option Hash128}
  deriving newtype (Eq, Show, Hashable)

instance IsDep CompDep where
  type DepKey CompDep = CompDepKey
  type DepVer CompDep = CompDepVer
  depKey (CompDep d) = dep_key d
  depVer (CompDep d) = dep_ver d

{- | All dependencies that can occur in compuations: either by performing a request
 on a source or by calling a computation.
-}
data CompEngDep
  = CompEngDepSrc AnyCompSrcDep
  | CompEngDepComp CompDep
  deriving (Show, Eq, Typeable, Generic)

data CompEngDepKey
  = CompEngDepKeySrc AnyCompSrcKey
  | CompEngDepKeyComp CompDepKey
  deriving (Show, Eq, Typeable, Generic)

data CompEngDepVer
  = CompEngDepVerSrc AnyCompSrcVer
  | CompEngDepVerComp CompDepVer
  deriving (Show, Eq, Typeable, Generic)

instance Hashable CompEngDep
instance Hashable CompEngDepKey
instance Hashable CompEngDepVer

instance IsDep CompEngDep where
  type DepKey CompEngDep = CompEngDepKey
  type DepVer CompEngDep = CompEngDepVer
  depKey dep =
    case dep of
      CompEngDepSrc x -> CompEngDepKeySrc (depKey x)
      CompEngDepComp x -> CompEngDepKeyComp (depKey x)
  depVer ver =
    case ver of
      CompEngDepSrc x -> CompEngDepVerSrc (depVer x)
      CompEngDepComp x -> CompEngDepVerComp (depVer x)

mkCompDep :: CompDepKey -> CompDepVer -> CompEngDep
mkCompDep a b = CompEngDepComp (CompDep (Dep a b))

type DepSet = HashSet CompEngDep

data CompAp r = forall p.
  (IsCompParam p, IsCompResult r) =>
  CompApIntern
  { capI_hash :: Hash128
  -- ^ Hash of computation name and param
  , capI_comp :: Comp p r
  , capI_param :: p
  }
  deriving (Typeable)

{-# COMPLETE CompAp #-}
pattern CompAp
  :: forall r
   . ()
  => forall p
   . (IsCompParam p, IsCompResult r)
  => Hash128
  -> Comp p r
  -> p
  -> CompAp r
pattern CompAp{cap_hash, cap_comp, cap_param} <- CompApIntern cap_hash cap_comp cap_param

mkCompAp :: (IsCompParam p, IsCompResult r) => Comp p r -> p -> CompAp r
mkCompAp comp p = value
 where
  value =
    CompApIntern
      { capI_hash =
          largeHash128 (compId_name $ comp_name comp, p)
      , capI_comp = comp
      , capI_param = p
      }

showCompApDetails :: CompAp r -> String
showCompApDetails (CompApIntern h c p) =
  "CompAp " ++ show h ++ " (" ++ show (comp_name c) ++ ") (" ++ show p ++ ")"

instance Show (CompAp r) where
  show (CompApIntern _ c p) = T.unpack (compId_name (comp_name c)) ++ "(" ++ show p ++ ")"

instance Eq (CompAp r) where
  (==) gap1 gap2 = (==) (cap_hash gap1) (cap_hash gap2)

instance Ord (CompAp r) where
  compare = comparing cap_hash

instance Hashable (CompAp r) where
  hashWithSalt s = hashWithSalt s . cap_hash

data CapId = CapId
  { capId_compId :: CompId
  , capId_paramText :: T.Text
  }
  deriving (Eq, Ord, Generic)

instance Show CapId where
  showsPrec p (CapId compId param) =
    showParen (p >= 10) $
      showString "CapId "
        . showsPrec 11 compId
        . showString " "
        . showsPrec 11 param

instance Hashable CapId

mkCapId :: IsCompParam p => CompId -> p -> CapId
mkCapId compId p =
  CapId
    { capId_compId = compId
    , capId_paramText = paramText
    }
 where
  maxlen = 1600
  showlen = 160
  paramText
    | paramLen > maxlen =
        pureWarn
          ( "Constructed a CapId for "
              ++ show compId
              ++ " with a very large parameter "
              ++ "of length "
              ++ show paramLen
              ++ ": "
              ++ T.unpack (shorten showlen theParamText)
          )
          theParamText
    | otherwise =
        theParamText
  paramLen = T.length theParamText
  theParamText = showText p

capCompId :: CompAp r -> CompId
capCompId (CompApIntern _ comp _) = comp_name comp

capId :: CompAp r -> CapId
capId (CompApIntern _ comp param) = mkCapId (comp_name comp) param

compCacheValue :: CompAp a -> a -> CompCacheValue a
compCacheValue (CompApIntern _ comp _) = ccb_memcache (comp_caching comp)

-- | Value resulting from applying a computation.
data CompApResult a = CompApResult
  { cr_returnValue :: a
  -- ^ this is returned to the callee
  , cr_cacheValue :: CompCacheValue a
  -- ^ this is cached
  }
  deriving (Show, Eq)

compApResult :: CompAp a -> a -> CompApResult a
compApResult cap a = CompApResult a (compCacheValue cap a)

data AnyCompAp = forall r. IsCompResult r => AnyCompAp (CompAp r)
  deriving (Typeable)

instance Show AnyCompAp where
  show (AnyCompAp gap) = show gap

showAnyCompApDetails :: AnyCompAp -> String
showAnyCompApDetails (AnyCompAp (ca :: CompAp r)) =
  "AnyCompAp "
    ++ show (typeRep (Proxy @r))
    ++ " ("
    ++ showCompApDetails ca
    ++ ")"

instance Eq AnyCompAp where
  (AnyCompAp (l :: CompAp a)) == (AnyCompAp (r :: CompAp b)) =
    case eqT :: Maybe (a :~: b) of
      Nothing -> False
      Just Refl -> l == r

instance Ord AnyCompAp where
  (AnyCompAp (l :: CompAp a)) `compare` (AnyCompAp (r :: CompAp b)) =
    case eqT :: Maybe (a :~: b) of
      Nothing -> compare (typeRep l) (typeRep r)
      Just Refl -> compare l r

instance Hashable AnyCompAp where
  hashWithSalt s (AnyCompAp gap) = hashWithSalt s gap

wrapCompAp :: IsCompResult r => CompAp r -> AnyCompAp
wrapCompAp = AnyCompAp

anyCompApPriority :: AnyCompAp -> PaqPriority
anyCompApPriority (AnyCompAp cap) = compId_priority (capCompId cap)

anyCapId :: AnyCompAp -> CapId
anyCapId (AnyCompAp cap) = capId cap
