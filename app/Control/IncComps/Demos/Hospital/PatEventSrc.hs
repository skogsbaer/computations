{-# LANGUAGE TypeFamilies #-}
module Control.IncComps.Demos.Hospital.PatEventSrc where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Utils.Types
import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.Demos.Hospital.PatTypes

----------------------------------------
-- EXTERNAL
----------------------------------------
import qualified Data.Text as T
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Control.Concurrent.STM
import Data.Hashable
import Data.LargeHashable

newtype MsgTime = MsgTime Int
  deriving (Show, Eq, Hashable, LargeHashable)

data PatMsg = PatMsg
  { pm_time :: MsgTime
  , pm_pat :: Pat
  }

data PatEventRequest a where
  PatMsgsSince :: Option MsgTime -> PatEventRequest [PatMsg]

data PatEventSrc = PatEventSrc

type PatEventDep = Dep () MsgTime

instance CompSrc PatEventSrc where
  type CompSrcReq PatEventSrc = PatEventRequest
  type CompSrcKey PatEventSrc = ()
  type CompSrcVer PatEventSrc = MsgTime
  compSrcInstanceId = undefined
  compSrcExecute = executeImpl
  compSrcUnregister = unregisterImpl
  compSrcWaitChanges = waitChangesImpl

executeImpl :: PatEventSrc -> PatEventRequest a -> IO (HashSet PatEventDep, Fail a)
executeImpl = undefined

unregisterImpl :: PatEventSrc -> HashSet () -> IO ()
unregisterImpl _ _ = pure ()

waitChangesImpl :: PatEventSrc -> STM (HashSet PatEventDep)
waitChangesImpl = undefined
