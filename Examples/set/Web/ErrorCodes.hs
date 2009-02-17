module Main where

import Happstack.Server
import Control.Monad
{-
  interesting urls:
   /badrequest
   /unauthorized
   /notfound
   /seeother
   /found
   /moved
   /tempredirect
   /set/404
-}
main :: IO ()
main = do simpleHTTP nullConf $ msum
             [ dir "badrequest" $ badRequest "badrequest"
             , dir "unauthorized" $ unauthorized "unauthorized"
             , dir "notfound" $ notFound "notfound"
             , dir "seeother" $ seeOther "/notfound/seeother" ""
             , dir "found" $  found "/notfound/found" ""
             , dir "moved" $ movedPermanently "/notfound/moved" ""
             , dir "tempredirect" $ tempRedirect "/notfound/tempredirect" ""
             , dir "set" $
                 path $ \errorCode ->
                   do setResponseCode errorCode
                      return $ "Error code: " ++ show errorCode
             ]
