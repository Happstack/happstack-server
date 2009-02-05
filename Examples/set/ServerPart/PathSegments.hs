module Main where

import HAppS.Server

{-
  interesting urls:
   /directory/
   /any/foo
   /any/foo/bar
   /int/10
   /int/no-parse
-}
main :: IO ()
main = do simpleHTTP nullConf
             [ dir "directory"
                       [ anyRequest $ ok "Inside directory" ]
             , dir "any"
                       [ path $ \pathSegment ->
                         [ anyRequest $ ok $ "Path segment: " ++ pathSegment ]
                       ]
             , dir "int"
                       [ path $ \int ->
                         [ anyRequest $ ok $ "Integer segment: " ++ show (int::Int) ]
                       ]
             , anyRequest $ ok "Sorry, couldn't find a matching handler" ]
