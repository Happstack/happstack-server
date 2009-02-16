module Main where

import Happstack.Server

{-
  interesting urls:
   /
-}
main :: IO ()
main = do simpleHTTP nullConf [ anyRequest $ ok "Hello World" ]

