module Control.Computations.CompEngine.CacheBehaviors (
  memCaching,
  inMemoryShowCaching,
  hashCaching,
) where

----------------------------------------
-- LOCAL
---------------------------------------

import Control.Computations.CompEngine.Types
import Control.Computations.Utils.DataSize
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import qualified Data.LargeHashable as LH
import qualified Data.Text as T

logreprLength :: Int
logreprLength = 40

-- | Stores the value itself in the cache, uses `LargeHashable` to compute the hash.
memCaching :: (LH.LargeHashable a, Show a) => CompCacheBehavior a
memCaching = CompCacheBehavior f
 where
  f x =
    let s = show x
        n = length s
        meta =
          CompCacheMeta
            { ccm_logrepr = T.pack (take logreprLength s)
            , ccm_approxCachedSize = Some $ bytes n
            , ccm_largeHash = largeHash128 x
            , ccm_cachedSize = None
            }
     in CompCacheValue (Some x) meta

-- | Only stores the hash in the cache.
hashCaching :: (LH.LargeHashable a, Show a) => CompCacheBehavior a
hashCaching = CompCacheBehavior f
 where
  f x =
    let !h = largeHash128 x
        !l = T.pack (take logreprLength (show x))
        meta =
          CompCacheMeta
            { ccm_logrepr = l
            , ccm_approxCachedSize = Some $ bytes 16
            , ccm_largeHash = h
            , ccm_cachedSize = None
            }
     in CompCacheValue None meta

-- | Stores the value itself in the cache, uses `show` to compute the hash.
inMemoryShowCaching :: Show a => CompCacheBehavior a
inMemoryShowCaching = CompCacheBehavior f
 where
  f x =
    let s = show x
        n = length s
        meta =
          CompCacheMeta
            { ccm_logrepr = T.pack (take logreprLength s)
            , ccm_approxCachedSize = Some $ bytes n
            , ccm_largeHash = largeHash128 s
            , ccm_cachedSize = None
            }
     in CompCacheValue (Some x) meta
