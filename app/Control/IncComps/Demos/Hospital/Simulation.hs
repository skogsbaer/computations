{-# LANGUAGE RecordWildCards            #-}

module Control.IncComps.Demos.Hospital.Simulation (
  SimCfg(..), defaultSimCfg, SimArgs(..), defaultSimArgs, runSim, simMain
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Demos.Hospital.PatDb
import Control.IncComps.Demos.Hospital.PatNotesDb
import Control.IncComps.Demos.Hospital.PatTypes
import qualified Control.IncComps.Demos.Hospital.FakeDiagnoses as Fake
import qualified Control.IncComps.Demos.Hospital.FakeWords as Fake
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.TimeUtils
import Control.IncComps.Utils.Tuple
import Control.IncComps.Utils.Types
import Control.IncComps.Utils.Clock
import Control.IncComps.Utils.Logging
import qualified Control.IncComps.Utils.SqliteUtils as Sqlite

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Test.QuickCheck (Arbitrary(arbitrary))
import Test.QuickCheck.Gen hiding (oneof, frequency)
import qualified Test.QuickCheck.Gen as Q
import Test.QuickCheck.Random
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Calendar
import System.Random
import Text.Printf
import Data.Char

data SimAction
  = SimPat Pat
  | SimPatNote PatNote
  deriving (Show)

type SimPats = HashMap PatId (Pat :!: HashSet PatNote)

data SimCfg = SimCfg
  { sc_minPats :: Int
  , sc_maxPats :: Int
  , sc_timeFactor :: Int -- time runs faster by this factor
  }

defaultSimCfg :: SimCfg
defaultSimCfg = SimCfg
  { sc_minPats = 10
  , sc_maxPats = 20
  , sc_timeFactor = 288 -- one day in 5 minutes
  }

data SimEnv = SimEnv
  { se_cfg :: SimCfg
  , se_time :: UTCTime
  , se_pats :: SimPats
  , se_nextPatId :: Integer
  }

freshPatId :: SimM PatId
freshPatId = do
  i <- asks se_nextPatId
  pure $ PatId $ T.pack $ printf "%06d" (100000 + i)

newtype SimM a = SimM (ReaderT SimEnv Gen a)
  deriving (Functor, Applicative, Monad, MonadReader SimEnv)

runSimM :: SimEnv -> SimM a -> Gen a
runSimM env (SimM r) = runReaderT r env

gen :: Gen a -> SimM a
gen = SimM . lift

getPat :: PatId -> SimM Pat
getPat patId = do
  pats <- asks se_pats
  case HashMap.lookup patId pats of
    Just (p :!: _) -> pure p
    Nothing -> error ("Unknown patient in simulation: " ++ show patId)

getTime :: SimM UTCTime
getTime = asks se_time

scaleTimeSpan :: TimeSpan -> Int -> TimeSpan
scaleTimeSpan ts factor =
  milliseconds (asMilliseconds ts `div` factor)

getTimeInFuture :: TimeSpan -> TimeSpan -> SimM UTCTime
getTimeInFuture start end = do
  t <- getTime
  offset <- seconds <$> gen (chooseInt (asSeconds start, asSeconds end))
  timeFactor <- asks (sc_timeFactor . se_cfg)
  let scaledOffset = scaleTimeSpan offset timeFactor
  pure (t `addTimeSpan` scaledOffset)

simArbitrary :: Arbitrary a => SimM a
simArbitrary = gen arbitrary

arbitraryDiagnose :: SimM T.Text
arbitraryDiagnose = gen (elements Fake.diagnoses)

arbitrarySentence :: SimM T.Text
arbitrarySentence = gen $ do
  n <- chooseInt (4, 8)
  words <- replicateM n (elements Fake.wordList)
  case words of
    (T.unpack -> (c:cs)) : rest ->
      pure $ T.unwords (T.pack (toUpper c : cs) : rest) <> "."
    _ -> error "arbitrarySentence: impossible"

arbitraryNoteText :: SimM T.Text
arbitraryNoteText = do
  n <- gen $ chooseInt (1, 5)
  sentences <- replicateM n arbitrarySentence
  pure (T.unwords sentences)

arbitraryBirthdate :: SimM Day
arbitraryBirthdate = do
  t <- getTime
  offset <- days <$> gen (chooseInt (1, (90 * 365)))
  pure $ utctDay $ t `addTimeSpan` (negate offset)

countPats :: SimM Int
countPats = do
  pats <- asks se_pats
  pure (HashMap.size pats)

getCfg :: SimM SimCfg
getCfg = asks se_cfg

currentPatIds :: SimM [PatId]
currentPatIds = do
  pats <- asks se_pats
  pure (HashMap.keys pats)

oneof :: [SimM a] -> SimM a
oneof l = do
  env <- ask
  let l' = map (runSimM env) l
  gen (Q.oneof l')

frequency :: [(Int, SimM a)] -> SimM a
frequency l = do
  env <- ask
  let l' = map (\(i, x) -> (i, runSimM env x)) l
  gen (Q.frequency l')

simStep :: SimM SimAction
simStep = do
  numPats <- countPats
  cfg <- getCfg
  case () of
    _ | numPats < sc_minPats cfg -> newPat
      | numPats > sc_minPats cfg -> delPat
      | otherwise -> frequency [(5, newNote), (1, changePat), (2, newPat), (2, delPat)]
  where
    delPat = do
      patIds <- currentPatIds
      toDel <- gen $ elements patIds
      logInfoM ("Deleting patient " ++ show toDel)
      pat <- getPat toDel
      now <- getTime
      pure (SimPat (pat { p_dischargeDateTime = Some now }))
    newPat = do
      p_patId <- freshPatId
      logInfoM ("Generating new patient " ++ show p_patId)
      p_sex <- simArbitrary
      p_name <- gen $ arbitraryName p_sex
      p_primaryDiagnose <- arbitraryDiagnose
      p_birthdate <- arbitraryBirthdate
      p_admissionDateTime <- getTimeInFuture (seconds 0) (hours 8)
      p_dischargeDateTime <-
        frequency
          [ (5, pure None)
          , (3, do
                  dischargeTime <- getTimeInFuture (days 2) (days 8)
                  pure (Some dischargeTime))
          ]
      pure $ SimPat $ Pat {..}
    changePat = do
      patIds <- currentPatIds
      toChange <- gen $ elements patIds
      logInfoM ("Changing patient " ++ show toChange)
      pat <- getPat toChange
      let changeDiagnose = do
            d <- arbitraryDiagnose
            pure $ SimPat $ pat { p_primaryDiagnose = d }
          changeDischargeDate = do
            t <- getTimeInFuture (days (-1)) (days 1)
            pure $ SimPat $ pat { p_dischargeDateTime = Some t }
      oneof [changeDiagnose, changeDischargeDate]
    newNote = do
      allPatIds <- currentPatIds
      patId <- gen $ elements allPatIds
      logInfoM ("Adding note to patient " ++ show patId)
      now <- getTime
      noteText <- arbitraryNoteText
      pure $ SimPatNote $ PatNote patId now noteText

data SimDbs = SimDbs
  { sd_patDb :: Sqlite.Database
  , sd_patNotesDb :: Sqlite.Database
  }

performAction :: SimDbs -> SimAction -> IO ()
performAction dbs action =
  case action of
    SimPat p -> insertPat (sd_patDb dbs) p
    SimPatNote note -> insertPatNote (sd_patNotesDb dbs) note

applyActionToPats :: SimAction -> SimPats -> SimPats
applyActionToPats action pats =
  case action of
    SimPat p ->
      HashMap.insertWith
        (\(newPat :!: _) (_ :!: notes) -> (newPat :!: notes))
        (p_patId p) (p :!: HashSet.empty) pats
    SimPatNote note ->
      case HashMap.lookup (pn_patId note) pats of
        Nothing -> error ("simulation: patient " ++ show (pn_patId note) ++
                          " not found when inserting patient note")
        Just (p :!: notes) ->
          HashMap.insert (pn_patId note) (p :!: HashSet.insert note notes) pats

data SimArgs = SimArgs
  { sa_cfg :: SimCfg
  , sa_stepDelta :: TimeSpan
  , sa_maxSteps :: Integer
  }

defaultSimArgs :: SimArgs
defaultSimArgs = SimArgs
  { sa_cfg = defaultSimCfg
  , sa_stepDelta = seconds 5
  , sa_maxSteps = 1000000
  }

runSim :: SimDbs -> SimArgs -> IO ()
runSim dbs args = do
  initGen <- newQCGen
  t <- getCurrentTime
  loop initGen t HashMap.empty True 1
  where
    loop :: QCGen -> UTCTime -> SimPats -> Bool -> Integer -> IO ()
    loop gen startTime !pats !inInitPhase !curStep = do
      logNote ("Step " ++ show curStep ++ " of simulation")
      t <- getSimTime startTime
      let env = SimEnv (sa_cfg args) t pats curStep
          r = runSimM env simStep
          (gen1, gen2) = split gen
          action = unGen r gen1 30
      logInfo ("Action: " ++ show action)
      performAction dbs action
      let newPats = applyActionToPats action pats
          newInInitPhase =
            inInitPhase && HashMap.size newPats < sc_minPats (sa_cfg args)
      unless inInitPhase $ c_sleep realClock (sa_stepDelta args)
      unless (curStep >= (sa_maxSteps args)) $
        loop gen2 startTime newPats newInInitPhase (curStep + 1)
    getSimTime startTime = do
      t <- getCurrentTime
      let delta = t `diffTime` startTime
          scaledDelta = scaleTimeSpan delta (sc_timeFactor (sa_cfg args))
      pure (startTime `addTimeSpan` scaledDelta)

simMain :: FilePath -> FilePath -> SimArgs -> IO ()
simMain patDbPath patNotesDbPath args = do
  withPatDb patDbPath $ \patDb ->
    withPatNotesDb patNotesDbPath $ \patNotesDb ->
      runSim (SimDbs patDb patNotesDb) args
