module Main where

import Happstack.Server

import System.Directory

{-
  interesting urls:
   /
-}

prog = "ghc"

main :: IO ()
main = do simpleHTTP nullConf
             [ require (findExecutable prog) $ \ghcPath ->
                   [ anyRequest $ ok $ prog ++ " path: " ++ ghcPath ]
             , anyRequest $ ok $ "Sorry, couldn't find " ++ prog ]
