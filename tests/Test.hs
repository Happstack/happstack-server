module Main where

import HAppS.Server.Tests (allTests)
import Test.HUnit (errors, failures, putTextToShowS,runTestText, runTestTT)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, stdout)

-- |A simple driver for running the local test suite.
main :: IO ()
main =
    do c <- do istty <- hIsTerminalDevice stdout
               if istty
                  then runTestTT allTests
                  else do (c,st) <- runTestText putTextToShowS allTests
                          putStrLn (st "")
                          return c
       case (failures c) + (errors c) of
         0 -> return ()
         n -> exitFailure
