module Main where

import Happstack.Server
import Control.Monad

{-
  interesting urls:
   /?string=hello+world
-}

main :: IO ()
main = simpleHTTP nullConf $ msum
           [
              do str <- getDataFn (look "string") >>= maybe mzero return
                 ok $ "You entered: " ++ str
            , ok "Sorry, I don't understand."
           ]
