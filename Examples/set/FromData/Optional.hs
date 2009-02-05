module Main where

import HAppS.Server
import Data.Maybe

{-
  interesting urls:
   /?string=hello+world
   /?string
   /
-}

data MyStructure = MyStructure {unpack :: String}
instance FromData MyStructure where
    fromData = do str <- look "string"
                  return $ MyStructure str
main :: IO ()
main = do simpleHTTP nullConf
             [ withData $ \mbStructure ->
                   [ anyRequest $ ok $ "Input: " ++ maybe "default value" unpack mbStructure ]
             ]
