module Main where

import Happstack.Server

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
main = do simpleHTTP nullConf
              [ withData $ \(MyStructure str) ->
                    [ anyRequest $ ok $ "Cookie value: " ++ str ]
              , dir "setcookie"
                    [ path $ \value ->
                          [ anyRequest $ do -- Create cookie with a duration of 30 seconds.
                                            addCookie 30 (mkCookie "cookie" value)
                                            ok "Cookie has been set"
                          ]
                    ]
              , anyRequest $ do ok "Try /setcookie/value" ]