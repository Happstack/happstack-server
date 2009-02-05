-- |HUnit tests and QuickQuick properties for HAppS.Server.*
module HAppS.Server.Tests (allTests) where

import Test.HUnit as HU (Test(..),(~:),(~?))

-- |All of the tests for happstack-util should be listed here. 
allTests :: Test
allTests = 
    "happstack-server tests" ~: [dummyTest]

dummyTest :: Test
dummyTest = "dummyTest" ~: True ~? "True"
