module Main where

import Happstack.Server
import Control.Monad

{-
  interesting urls:
   /?string=hello+world
-}

data MyStructure = MyStructure String
instance FromData MyStructure where
    fromData = do str <- look "string"
                  return $ MyStructure str

main :: IO ()
main = do simpleHTTP nullConf $ msum
              [
                do
                  (MyStructure str) <- getData >>= maybe mzero return
                  ok $ "You entered: " ++ str
              , ok "Sorry, I don't understand."
              ]

