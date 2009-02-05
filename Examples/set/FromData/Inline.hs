module Main where

import HAppS.Server

{-
  interesting urls:
   /?string=hello+world
-}

main :: IO ()
main = do simpleHTTP nullConf
            [ withDataFn (look "string") $ \str ->
                  [ anyRequest $ ok $ "You entered: " ++ str ]
            , anyRequest $ ok "Sorry, I don't understand." ]
