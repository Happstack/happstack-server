module Main where

import Happstack.Server

{-
  interesting urls:
   /
   /IO.hs
   /FileServe.hs
-}
main :: IO ()
main = simpleHTTP nullConf $ fileServe ["FileServe.hs"] "."
