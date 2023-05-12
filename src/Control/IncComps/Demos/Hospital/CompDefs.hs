module Control.IncComps.Demos.Hospital.CompDefs where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine
import Control.IncComps.FlowImpls.CompLogging
import Control.IncComps.FlowImpls.FileSink
import Control.IncComps.FlowImpls.FileSrc
import Control.IncComps.FlowImpls.TimeSrc
import Control.IncComps.FlowImpls.IOSink
import Control.IncComps.Utils.Fail
import Control.IncComps.Utils.IOUtils
import Control.IncComps.Utils.TimeUtils
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.Types
import Control.IncComps.Demos.Hospital.PatTypes
import Control.IncComps.Demos.Hospital.Config
import Control.IncComps.Demos.Hospital.PatEventSrc
import Control.IncComps.Utils.Tuple

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Proxy
import Data.Time.Clock
import System.Directory
import System.FilePath
import qualified Data.Text as T
import Data.List (foldl')

configFileSrcId :: TypedCompSrcId FileSrc
configFileSrcId = typedCompSrcId (Proxy @FileSrc) "configFileSrc"

readConfigFile :: FilePath -> CompM BS.ByteString
readConfigFile p = compSrcReq configFileSrcId (ReadFile p)

htmlFileSinkId :: TypedCompSinkId FileSink
htmlFileSinkId = typedCompSinkId (Proxy @FileSink) "htmlFileSink"

writeHtmlFile :: FilePath -> BS.ByteString -> CompM ()
writeHtmlFile p bs = compSinkReq htmlFileSinkId (WriteFile p bs)

patEventSrcId :: TypedCompSrcId PatEventSrc
patEventSrcId = typedCompSrcId (Proxy @PatEventSrc) "patEventSrc"

patMsgsSince :: Option MsgTime -> CompM [PatMsg]
patMsgsSince t = compSrcReq patEventSrcId (PatMsgsSince t)

type PatMap = HashMap PatId Pat
type PatSet = HashSet Pat
newtype URL = URL T.Text

getConfigCompDef :: CompDef () Config
getConfigCompDef = mkComp "getConfigComp" inMemoryLHCaching $ \() ->
  do bs <- readConfigFile "demo.cfg"
     failInM (parseConfig bs) -- FIXME: deal with parse errors (don't fail!)

maxTimeToKeepAfterDischarge :: TimeSpan
maxTimeToKeepAfterDischarge = days 1

-- | Computes the map of active patients.
--
-- Active patients are those that are potentially shown to the user.
-- A patient is active is he/she has no discharge date or the discharge date
-- is only maxTimeToKeepAfterDischarge in the past. The admission date
-- does not influence the active patients (see below).
--
-- To compute the active patients, we essentially fold over the PatMsg events.
-- This folding is done incrementally, whenever a new PatMsg arrives. We don't
-- ever look at old messages again. Consequently, the logic for marking a
-- a patient as active must not rely on the current time and must not use
-- variables that are configurable at runtime. Hence, the logic ignores the
-- admission time because judging whether it's close enough would require
-- access to the current time. Further, we use the hardcode value
-- maxTimeToKeepAfterDischarge when evaluating the discharge date.
--
-- The computation defined by visiblePatsCompDef further refines the set of
-- active patients to the set of visible patients. This computation respects
-- the configuration variables `visibleAfterDischarge` and `visibleBeforeAdmission`.
--
-- To allow faster updates for demo mode, both computations are configured
-- over the `TimeIntervalType` used to update the current time in regular intervals.
activePatsCompDef :: TimeIntervalType -> CompDef () (Option MsgTime :!: PatMap)
activePatsCompDef interval =
  mkIncComp "activePatsComp'" (None :!: HashMap.empty) $ \() acc@(mMsgTime :!: _) ->
  do msgs <- patMsgsSince mMsgTime
     let (newMsgTime :!: newMap) = updatePats acc msgs
     now <- compGetTime interval
     pure (newMsgTime :!: removeDischarged now maxTimeToKeepAfterDischarge newMap)
  where
    updatePats :: (Option MsgTime :!: PatMap) -> [PatMsg] -> (Option MsgTime :!: PatMap)
    updatePats acc msgs = foldl' (\(_ :!: m) msg -> (Some (pm_time msg) :!: updatePat m msg)) acc msgs
    updatePat m msg =
      let pat = pm_pat msg
      in HashMap.insertWith (\new _ -> new) (p_patId pat) pat m
    removeDischarged :: UTCTime -> TimeSpan -> PatMap -> PatMap
    removeDischarged now ts m =
      HashMap.filter (\p -> not (dischargeDateTooFarInThePast now ts p)) m

dischargeDateTooFarInThePast :: UTCTime -> TimeSpan -> Pat -> Bool
dischargeDateTooFarInThePast now ts pat =
  case p_dischargeDate pat of
    None -> False
    Some t -> now `diffTime` t > ts

admissionDateTooFarInTheFuture :: UTCTime -> TimeSpan -> Pat -> Bool
admissionDateTooFarInTheFuture now ts pat =
  p_admissionDate pat `diffTime` now > ts

visiblePatsCompDef ::
  TimeIntervalType -> Comp () Config -> Comp () (Option MsgTime, PatMap) -> CompDef () PatMap
visiblePatsCompDef interval configC activePatsC =
  mkComp "activePatsComp" inMemoryLHCaching $ \() ->
  do (_, patMap) <- evalCompOrFail activePatsC ()
     now <- compGetTime interval
     cfg <- evalCompOrFail configC ()
     pure (HashMap.filter (cond now cfg) patMap)
  where
    cond now cfg pat =
      not (dischargeDateTooFarInThePast now (c_visibleAfterDischarge cfg) pat) &&
      not (admissionDateTooFarInTheFuture now (c_visibleBeforeAdmission cfg) pat)

recentPatsCompDef :: TimeIntervalType -> Comp () PatMap -> Comp () Config -> CompDef () PatSet
recentPatsCompDef interval activePatsC configC =
  mkComp "recentPatsComp" onlyCacheLHCaching $ \() ->
  do cfg <- evalCompOrFail configC ()
     patMap <- evalCompOrFail activePatsC ()
     now <- compGetTime interval
     pure $ HashSet.fromList $ filter (isRecent now cfg) (HashMap.elems patMap)
  where
    isRecent now cfg pat =
      let delta = now `diffTime` (p_admissionDate pat)
      in isNone (p_dischargeDate pat) &&
         delta > 0 &&
         delta <= c_recentTimeSpan cfg

patNotesCompDef :: CompDef PatId (HashSet PatNote)
patNotesCompDef = undefined -- get from source

overviewCompDef :: Comp () PatMap -> Comp () PatSet -> Comp Pat URL -> CompDef () ()
overviewCompDef _activePats _recentPats = undefined

getPatCompDef :: Comp () PatMap -> CompDef PatId Pat
getPatCompDef = undefined

detailsCompDef :: Comp PatId Pat -> Comp PatId (HashSet PatNote) -> CompDef Pat URL
detailsCompDef = undefined
