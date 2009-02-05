module Main where

import HAppS.Server

{-
  interesting urls:
   /?string=hello+world
-}

data MyStructure = MyStructure String
instance FromData MyStructure where
    fromData = do str <- look "string"
                  return $ MyStructure str

main :: IO ()
main = do simpleHTTP nullConf
              [ withData $ \(MyStructure str) ->
                    [ anyRequest $ ok $ "You entered: " ++ str ]
              , anyRequest $ ok "Sorry, I don't understand." ]

