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
import Control.Computations.Utils.Fail
import Control.Computations.Utils.TimeUtils

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import Data.Proxy
import Data.Time.Clock

type PatMap = HashMap PatId Pat
type PatSet = HashSet Pat

cfgFileSrcId :: TypedCompSrcId FileSrc
cfgFileSrcId = typedCompSrcId (Proxy @FileSrc) "cfgFileSrc"

getCfgCompDef :: CompDef () Config
getCfgCompDef =
  mkCompDef "getCfgComp" memCaching $ \() -> do
    bs <- compSrcReq cfgFileSrcId
            (ReadFile "demo.cfg")
    failInM $ parseConfig bs

type PatsAcc = (Maybe PatMsgKey, PatMap)

activePatsCompDef :: CompDef () PatsAcc
activePatsCompDef =
  mkIncCompDef "activePatsComp"
    (Nothing, HashMap.empty) fun
 where fun :: () -> PatsAcc -> CompM PatsAcc
       fun = undefined

visiblePatsCompDef ::
  Comp () Config
  -> Comp () PatsAcc
  -> CompDef () PatMap
visiblePatsCompDef cfgC activePatsC =
  mkCompDef "visiblePatsComp" memCaching $
    \() -> do
      (_, m) <-
        evalCompOrFail activePatsC ()
      now <- compGetTime TimeInterval5min
      cfg <- evalCompOrFail cfgC ()
      pure (HashMap.filter (isVisible now cfg) m)
 where
  isVisible :: UTCTime -> Config -> Pat -> Bool
  isVisible now cfg pat = undefined

defineComps :: CompDefM (Comp () ())
defineComps = do
  cfgC <- defineComp getCfgCompDef
  activePatsC <- defineComp activePatsCompDef
  visiblePatsC <- defineComp $
    visiblePatsCompDef cfgC activePatsC
  undefined
