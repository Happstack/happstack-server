module Main where

import Happstack.Server
import Control.Monad
{-
  interesting urls:
   /setcookie/value
   /setcookie/hello+world
-}

data MyStructure = MyStructure String
instance FromData MyStructure where
    fromData = do str <- lookCookieValue "cookie"
                  return $ MyStructure str

main :: IO ()
main = do simpleHTTP nullConf $ msum [
              do (MyStructure str) <- getData >>= maybe mzero return
                 ok $ "Cookie value: " ++ str
            , dir "setcookie" $
                    path $ \value ->
                          do -- Create cookie with a duration of 30 seconds.
                             addCookie 30 (mkCookie "cookie" value)
                             ok "Cookie has been set"
            , ok "Try /setcookie/value" ]
