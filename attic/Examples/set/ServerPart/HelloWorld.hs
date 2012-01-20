module Main where

import Happstack.Server

{-
  interesting urls:
   /
-}
main :: IO ()
main = simpleHTTP nullConf $ ok "Hello World"

