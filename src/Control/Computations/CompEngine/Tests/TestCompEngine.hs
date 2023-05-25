{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Tests.TestCompEngine (
  Id (..),
  NItem (..),
  CountRuns,
  SomeItem (..),
  getItem,
  writeItem,
  mkTextRes,
  runCompEngineTest,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
---------------------------------------

import Control.Computations.CompEngine.CacheBehaviors
import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompEval
import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Tests.TestDynamicChanges
import Control.Computations.CompEngine.Types
import Control.Computations.FlowImpls.HashMapFlow
import Control.Computations.Utils.Fail
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.LargeHashable
import Data.Serialize
import Data.Serialize.Text ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Typeable
import GHC.Generics (Generic)
import Test.Framework

newtype Id = Id T.Text
  deriving (Show, Read, Eq, Ord, Hashable, Typeable, LargeHashable, Generic)

instance Serialize Id

listId, forgetIdVal :: Id
listId = Id ""
forgetIdVal = Id "forgetId"

idToByteString :: Id -> B.ByteString
idToByteString (Id a) = T.encodeUtf8 a

byteStringToId :: B.ByteString -> Id
byteStringToId = Id . T.decodeUtf8

class (Eq a, Show a, Typeable a, Hashable a) => Item a where
  getId :: a -> Id
  serialize :: a -> B.ByteString
  deserialize :: Id -> B.ByteString -> Maybe a
  forgetId :: a -> a
  forgetId = id

data SomeItem
  = forall a. Item a => SomeItem a
  deriving (Typeable)

instance Eq SomeItem where
  (SomeItem a) == (SomeItem b) =
    case cast a of
      Nothing -> False
      Just b' -> b == b'

instance Hashable SomeItem where
  hashWithSalt s (SomeItem i) = hashWithSalt s i

instance Show SomeItem where
  show (SomeItem a) = show a

instance Item SomeItem where
  getId (SomeItem i) = getId i
  serialize (SomeItem i) = serialize i
  deserialize _i _b = fail "cant deserialize to unknown item type"
  forgetId (SomeItem i) = SomeItem (forgetId i)

data NItem = NItem
  { ni_id :: Id
  , ni_name :: T.Text
  }
  deriving (Show, Eq, Typeable)

instance Hashable NItem where
  hashWithSalt s (NItem (Id i) n) = hashWithSalt s i + hashWithSalt s n

newtype ListItem
  = ListItem [Id]
  deriving (Show, Eq, Typeable, Hashable)

data ResourceItem = ResourceItem
  { ri_id :: Id
  , ri_content :: B.ByteString
  , ri_version :: T.Text
  }
  deriving (Show, Eq, Typeable)

instance Hashable ResourceItem where
  hashWithSalt s (ResourceItem (Id i) c v) =
    hashWithSalt s i + hashWithSalt s c + hashWithSalt s v

instance Item ResourceItem where
  getId = ri_id
  serialize ri = runPut (put (ri_content ri, ri_version ri))
  deserialize i b =
    case runGet get b of
      Left s -> pureError ("error while reading resource item " ++ s) $! Nothing
      Right (q, qq) -> Just (ResourceItem i q qq)

type Version = T.Text
type Versioned a = a
versioned :: a -> Version -> Versioned a
versioned a _v = a

instance Item NItem where
  getId = ni_id
  serialize = T.encodeUtf8 . ni_name
  deserialize i = Just . NItem i . T.decodeUtf8
  forgetId i = i{ni_id = forgetIdVal}

instance Item ListItem where
  getId = const (Id "")
  serialize (ListItem l) = runPut (put l)
  deserialize i b =
    case i of
      Id "" ->
        case runGet get b of
          Left s -> pureError ("error while reading list item " ++ s) $! Nothing
          Right q -> Just (ListItem q)
      Id r -> fail $ "list items have no id (found id " ++ T.unpack r ++ ")"

inputHashMapSrcId :: TypedCompSrcId HashMapFlow
inputHashMapSrcId = typedCompSrcId (Proxy @HashMapFlow) "input"

outputHashMapSinkId :: TypedCompSinkId HashMapFlow
outputHashMapSinkId = typedCompSinkId (Proxy @HashMapFlow) "output"

getItem :: (Item a) => Id -> CompM (Maybe a)
getItem ident = do
  res <- compSrcReq inputHashMapSrcId (HashMapLookupReq (idToByteString ident))
  case res of
    Nothing -> pure Nothing
    Just bs ->
      case deserialize ident bs of
        Nothing -> fail ("Deserialation of item with ID " ++ show ident ++ " failed")
        Just x -> pure (Just x)

writeItem :: (Item a) => a -> CompM ()
writeItem it =
  compSinkReq
    outputHashMapSinkId
    (HashMapStoreReq (idToByteString (getId it) `B.append` serialize it) (serialize it))

newtype UserId
  = UserId T.Text
  deriving (Read, Show, Typeable)

wid1, wid2 :: Id
wid1 = Id "23"
wid2 = Id "42"

wardItem1_v1, wardItem1_v2 :: NItem
wardItem1_v1 = NItem wid1 "foo"
wardItem1_v2 = NItem wid1 "foobar"

wardItem2_v1 :: NItem
wardItem2_v1 = NItem wid2 "cat"

wardListItem_v1, wardListItem_v2, wardListItem_v3 :: ListItem
wardListItem_v1 = ListItem [wid1]
wardListItem_v2 = ListItem [wid1, wid2]
wardListItem_v3 = ListItem [wid2]

wardWit1_v1, wardWit1_v2 :: Versioned NItem
wardWit1_v1 = versioned wardItem1_v1 "v1"
wardWit1_v2 = versioned wardItem1_v2 "v2"

wardWit2_v1 :: Versioned NItem
wardWit2_v1 = versioned wardItem2_v1 ("v1")

wardListWit_v1, wardListWit_v2, wardListWit_v3 :: Versioned ListItem
wardListWit_v1 = versioned wardListItem_v1 ("v1")
wardListWit_v2 = versioned wardListItem_v2 ("v2")
wardListWit_v3 = versioned wardListItem_v3 ("v3")

wardGenDef :: CompDef Id ()
wardGenDef =
  mkCompDef "wardGen" inMemoryShowCaching $ \wid ->
    do
      Just item <- (pureInfo $ "Reading " ++ show wid) $! getItem wid
      writeItem (item :: NItem)

allWardsCompDef
  :: Comp Id ()
  -> CompDef () ()
allWardsCompDef wardGen =
  mkCompDef "allWards" inMemoryShowCaching $ \() ->
    do
      Just (ListItem wardList) <- getItem listId
      mapM_ (evalComp wardGen) wardList

toItemList :: HM.HashMap B.ByteString B.ByteString -> [SomeItem]
toItemList = map toItem . HM.toList
 where
  toItem (k, v) = SomeItem (NItem (byteStringToId k) (T.decodeUtf8 v))

type CountRuns = HM.HashMap AnyCompAp (Integer, Integer)

runCompEngineTest
  :: [SomeItem]
  -> [SomeItem]
  -> (CompDefM (Comp () ()))
  -> (([SomeItem], [SomeItem]) -> IO ())
  -> IO ()
runCompEngineTest initialItems nextItems compDefs doTest =
  do
    hmInput <- initHashMapFlow "input"
    hmOutput <- initHashMapFlow "output"
    mapM_ (insertItem hmInput) initialItems
    reg <- newCompFlowRegistry
    registerCompSrc reg hmInput
    registerCompSrc reg hmOutput
    registerCompSink reg hmInput
    registerCompSink reg hmOutput
    (_compMap, mainComp) <- failInM $ runCompDefM compDefs
    (stateIf, closeSif) <- initStateIf True
    let caps = [wrapCompAp (mkCompAp mainComp ())]
    let run = do
          logInfo "Starting CompEngine test"
          runCompEngineTest' caps nextItems stateIf reg hmInput
          logInfo "Finished CompEngine test"
          input <- toItemList `fmap` getHashMap hmInput
          output <- toItemList `fmap` getHashMap hmOutput
          doTest (input, forgetId `fmap` output)
    run `finally` closeSif
    return ()
 where
  insertItem hmInput item = (hmfInsert hmInput (idToByteString $ getId item) (serialize item))

runCompEngineTest'
  :: forall a
   . (Item a)
  => [AnyCompAp]
  -> [a]
  -> CompEngineStateIf IO
  -> CompFlowRegistry
  -> HashMapFlow
  -> IO ()
runCompEngineTest' caps nextItems stateIf reg hmInput =
  runCompEngine compEngineIfs caps runCompEngineIf nextItems
 where
  compEngineIfs =
    CompEngineIfs
      { ce_compFlowRegistry = reg
      , ce_stateIf = stateIf
      }
  runCompEngineIf =
    RunCompEngineIf
      { rcif_shouldStartWithRun = shouldStartNextRun
      , rcif_emptyChangesMode = DontBlock
      , rcif_getTime = getCurrentTime
      , rcif_maxLoopRunTime = minutes 1
      , rcif_maxRunIterations = CompRunUnlimitedIterations
      , -- dont run garbage collector (tests assume that)
        rcif_reportGarbage = \_ -> return ()
      }
  shouldStartNextRun i _ _staleCaps si =
    if i `mod` 4 == 0 || i `mod` 4 == 1 || i `mod` 4 == 3
      then do
        logDebug ("Startig next run for i=" ++ show i)
        return (startNextRun, si)
      else case si of
        [] -> do
          logDebug
            ( "Not doing next run for i="
                ++ show i
                ++ " because no more test data available"
            )
          return (noNextRun, [])
        (next : rest) -> do
          logDebug ("Inserting test data " ++ show next)
          liftIO
            ( hmfInsert
                hmInput
                (idToByteString (getId next))
                (serialize next)
            )
          return (startNextRun, rest)

set :: (Hashable a) => [a] -> HS.HashSet a
set = HS.fromList

genData
  :: (IsCompParam a)
  => [SomeItem]
  -> [SomeItem]
  -> (CompDef a ())
  -> a
  -> (([SomeItem], [SomeItem]) -> IO ())
  -> IO ()
genData first after start startValue doTest =
  runCompEngineTest first after compDefs doTest
 where
  compDefs =
    do
      subComp <- defineComp start
      defineComp (mainCompDef subComp)
   where
    mainCompDef subComp =
      mkCompDef "main" inMemoryShowCaching $ \() ->
        void $ evalComp subComp startValue

genAllWards
  :: [SomeItem]
  -> [SomeItem]
  -> (([SomeItem], [SomeItem]) -> IO ())
  -> IO ()
genAllWards first after doTest =
  runCompEngineTest first after compDefs doTest
 where
  compDefs =
    do
      subComp <- defineComp wardGenDef
      defineComp (allWardsCompDef subComp)

mkTextRes :: T.Text -> SomeItem
mkTextRes = SomeItem . NItem forgetIdVal

---- TESTS

test_initialEvaluation :: IO ()
test_initialEvaluation =
  genData initItems laterItems wardGenDef wid1 $ \(_, objs) ->
    assertEqual expected objs
 where
  expected =
    [ (mkTextRes "foo")
    ]
  initItems = [SomeItem wardWit1_v1]
  laterItems = []

test_evaluationAfterDepChange :: IO ()
test_evaluationAfterDepChange =
  genData initItems laterItems wardGenDef wid1 $ \(_, objs) ->
    assertEqual (set expected) (set objs)
 where
  expected =
    [ (mkTextRes (T.pack "foo"))
    , (mkTextRes (T.pack "foobar"))
    ]
  initItems = [SomeItem wardWit1_v1]
  laterItems = [SomeItem wardWit1_v2]

test_compRunsGen :: IO ()
test_compRunsGen =
  genAllWards initItems laterItems $ \(_, objs) ->
    assertEqual expected objs
 where
  expected =
    [ (mkTextRes (T.pack "foo"))
    ]
  initItems = [SomeItem wardListWit_v1, SomeItem wardWit1_v1]
  laterItems = []

test_changesWithinRun :: IO ()
test_changesWithinRun =
  forM_ [1 .. 5] $ \c ->
    testGeneric (getFun c) getComps 100
 where
  getFun bCount r = [("AAAAA" ++ (replicate bCount 'B') ++ (repeat 'C')) !! (rc_gets r)]
  compCDef
    :: Comp () String
    -> Comp String String
    -> CompDef () ()
  compCDef compA compB =
    mkCompDef "CompC" memCaching $ \_ ->
      do
        Just _ <- evalComp compA ()
        Just _ <- evalComp compB "labReport1"
        Just _ <- evalComp compB "labReport2"
        Just _ <- evalComp compB "labReport3"
        Just _ <- evalComp compB "labReport4"
        return ()
  compBDef
    :: Comp () String
    -> CompDef String String
  compBDef compA =
    mkCompDef "CompB" memCaching $ \_ ->
      do
        Just msg <- evalComp compA ()
        return (take 5 msg)
  compADef :: CompDef () String
  compADef =
    mkCompDef "CompA" memCaching $ \_ ->
      do
        msg <- requestExt
        let junk = replicate (401 * 1024) '0'
        return (msg ++ junk)
  getComps :: CompDefM (Comp () ())
  getComps =
    do
      compA <- defineComp (compADef)
      compB <- defineComp (compBDef compA)
      defineComp (compCDef compA compB)

-- partial functions in tests can be tolerated
{-# ANN test_revive ("HLint: ignore Partial function" :: String) #-}
test_revive :: IO ()
test_revive =
  do
    x <- testGeneric getFun getComps 7
    assertEqual "DD" (head $ rc_puts x)
 where
  compCDef
    :: Comp () String
    -> Comp String String
    -> CompDef () ()
  compCDef compA compB = mkCompDef "CompC" memCaching $ \_ -> do
    Just x <- evalComp compA ()
    Just y <- case x of
      "A" -> evalComp compB "patItem1"
      "B" -> return $ Just ""
      _ -> evalComp compB "patItem1"
    putReq (x ++ y)
    return ()
  compBDef :: CompDef String String
  compBDef = mkCompDef "CompB" memCaching $ \_ -> requestExt
  compADef :: CompDef () String
  compADef = mkCompDef "CompA" memCaching $ \_ -> requestExt
  getComps =
    do
      compA <- defineComp (compADef)
      compB <- defineComp (compBDef)
      defineComp (compCDef compA compB)
  getFun :: RunCounters -> String
  getFun r =
    let str = "AABBBCDD" ++ (repeat 'D')
        char = str !! (rc_runs r)
     in [char]

test_compRunsGenThenChangesAndRunsOtherGenToo :: IO ()
test_compRunsGenThenChangesAndRunsOtherGenToo =
  genAllWards initItems laterItems $ \(_, objs) ->
    assertEqual (set expected) (set objs)
 where
  expected =
    [ mkTextRes (T.pack "foo")
    , mkTextRes (T.pack "cat")
    ]
  initItems = [SomeItem wardListWit_v1, SomeItem wardWit1_v1]
  laterItems = [SomeItem wardListWit_v2, SomeItem wardWit2_v1]

test_compRunsGenThenChangesAndRunsOnlyOtherGen :: IO ()
test_compRunsGenThenChangesAndRunsOnlyOtherGen =
  genAllWards initItems laterItems $ \(_, objs) ->
    assertEqual (set expected) (set objs) -- todo remove set
 where
  expected =
    [ mkTextRes (T.pack "foo")
    , mkTextRes (T.pack "cat")
    ]
  initItems = [SomeItem wardListWit_v1, SomeItem wardWit1_v1]
  laterItems = [SomeItem wardListWit_v3, SomeItem wardWit2_v1]

test_compRunsGenThenChangesAndRunsOnlyOtherGenEvenThoughFirstChangesToo :: IO ()
test_compRunsGenThenChangesAndRunsOnlyOtherGenEvenThoughFirstChangesToo =
  genAllWards initItems laterItems $ \(_, objs) ->
    assertEqual (set expected) (set objs) -- todo remove set
 where
  expected =
    [ mkTextRes (T.pack "foo")
    , mkTextRes (T.pack "cat")
    ]
  initItems = [SomeItem wardListWit_v1, SomeItem wardWit1_v1]
  laterItems = [SomeItem wardListWit_v3, SomeItem wardWit1_v2, SomeItem wardWit2_v1]
