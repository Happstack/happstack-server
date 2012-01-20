module Main where

import Happstack.Server
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
main = simpleHTTP nullConf $ do
             mbStructure <- getData
             ok $ "Input: " ++ maybe "default value" unpack mbStructure
