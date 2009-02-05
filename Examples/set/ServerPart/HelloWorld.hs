module Main where

import HAppS.Server

{-
  interesting urls:
   /
-}
main :: IO ()
main = do simpleHTTP nullConf [ anyRequest $ ok "Hello World" ]

