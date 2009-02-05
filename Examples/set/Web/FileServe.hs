module Main where

import HAppS.Server

{-
  interesting urls:
   /
   /IO.hs
   /FileServe.hs
-}
main :: IO ()
main = do simpleHTTP nullConf [ fileServe ["FileServe.hs"] "." ]
