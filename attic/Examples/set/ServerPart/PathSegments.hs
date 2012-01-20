module Main where

import Happstack.Server
import Control.Monad

{-
  interesting urls:
   /directory/
   /any/foo
   /any/foo/bar
   /int/10
   /int/no-parse
-}
main :: IO ()
main = simpleHTTP nullConf $ msum
             [ dir "directory" $ ok "Inside directory"
             , dir "any" $
                       path $ \pathSegment ->
                           ok $ "Path segment: " ++ pathSegment
             , dir "int" $
                       path $ \int ->
                           ok $ "Integer segment: " ++ show (int::Int)
             , ok "Sorry, couldn't find a matching handler" ]
