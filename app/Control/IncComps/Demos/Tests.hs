{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Control.IncComps.Demos.Tests (testMain) where

import Control.IncComps.Utils.Logging
import Test.Framework

-- Generate with
-- egrep -R -l '^(test|prop)_' app | sed 's|src/||g; s|/|.|g; s|.hs$||g' | sort -u | gawk '{ printf "import {-@ HTF_TESTS @-} %s\n", $0 }'
import {-@ HTF_TESTS @-} Control.IncComps.Demos.DirSync.Tests

testMain :: IO ()
testMain = do
  setupLogging WARN
  htfMain htf_importedTests
