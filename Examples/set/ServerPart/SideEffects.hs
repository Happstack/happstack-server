module Main where

import Happstack.Server
import Control.Monad
import System.Directory

{-
  interesting urls:
   /
-}

prog = "ghc"

main :: IO ()
main =  simpleHTTP nullConf $ msum
           [ requireM (findExecutable prog) $ \ghcPath ->
                   ok $ prog ++ " path: " ++ ghcPath
           , ok $ "Sorry, couldn't find " ++ prog ]
