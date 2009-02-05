module Main where

import HAppS.Server
import Control.Monad
{-
  interesting urls:
   /
   /escape
-}
main :: IO ()
main = do simpleHTTP nullConf
             [ withRequest $ \rq -> do unless (null $ rqPaths rq) $ escape $ seeOther "http://escape.com/" ()
                                       ok "Hello World"
             ]
