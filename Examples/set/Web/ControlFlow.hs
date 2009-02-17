module Main where

import Happstack.Server
import Control.Monad
{-
  interesting urls:
   /
   /escape
-}
main :: IO ()
main = simpleHTTP nullConf $
             do rq <- askRq
                unless (null $ rqPaths rq) $ escape $ seeOther "http://escape.com/" $ toResponse ()
                ok "Hello World"

