-- | This module defines the computations as in the paper. The
-- real definitions are in
-- 'Control.Computations.Demos.Hospital.CompDefs'.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Control.Computations.Demos.Hospital.PaperCompDefs () where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.Demos.Hospital.Config
import Control.Computations.Demos.Hospital.PatDb
import Control.Computations.Demos.Hospital.PatTypes
import Control.Computations.FlowImpls.FileSrc
import Control.Computations.FlowImpls.TimeSrc
import Control.Computations.Utils.TimeUtils

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Time.Clock

getCfgCompDef :: CompDef () Config
getCfgCompDef =
 mkCompDef "getCfg" fullCaching $ \() -> do
  bs <- compSrcReq cfgSrcId (ReadFile "demo.cfg")
  parseCfg bs

activePatsCompDef :: CompDef () PatsAcc
activePatsCompDef =
 mkIncCompDef "activePats" (Nothing,HashMap.empty) f
 where f :: () -> PatsAcc -> CompM PatsAcc
       -- read more patients from eventlog
       -- update PatMap accordingly
       f = undefined

visiblePatsCompDef :: Comp () Config
   -> Comp () PatsAcc -> CompDef () PatMap
visiblePatsCompDef cfgC activePatsC =
 mkCompDef "visiblePats" fullCaching $ \() -> do
  cfg <- evalCompOrFail cfgC ()
  (_, m) <- evalCompOrFail activePatsC ()
  now <- compGetTime TimeInterval5min
  pure (HashMap.filter (pred now cfg) m)
 where pred :: UTCTime -> Config -> Pat -> Bool
       pred = undefined

defineComps :: CompDefM (Comp () ())
defineComps = do
  cfgC <- defineComp getCfgCompDef
  activePatsC <- defineComp activePatsCompDef
  visiblePatsC <- defineComp $
    visiblePatsCompDef cfgC activePatsC
  undefined

type PatMap = HashMap PatId Pat
type PatsAcc = (Maybe PatMsgKey, PatMap)

cfgSrcId :: TypedCompSrcId FileSrc
cfgSrcId = undefined

-- ReadFile :: FilePath -> CompSrcReq FileSrc ByteString
parseCfg :: MonadFail m => ByteString -> m Config
parseCfg _ = fail "error"
