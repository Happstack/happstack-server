module Main where

import HAppS.Server

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
main = do simpleHTTP nullConf
             [ dir "badrequest"
                       [ anyRequest $ badRequest "badrequest" ]
             , dir "unauthorized"
                       [ anyRequest $ unauthorized "unauthorized" ]
             , dir "notfound"
                       [ anyRequest $ notFound "notfound" ]
             , dir "seeother"
                       [ anyRequest $ seeOther "/notfound/seeother" "" ]
             , dir "found"
                       [ anyRequest $ found "/notfound/found" "" ]
             , dir "moved"
                       [ anyRequest $ movedPermanently "/notfound/moved" "" ]
             , dir "tempredirect"
                       [ anyRequest $ tempRedirect "/notfound/tempredirect" "" ]
             , dir "set"
               [ path $ \errorCode ->
                 [ anyRequest $ do setResponseCode errorCode
                                   return $ "Error code: " ++ show errorCode
                 ]
               ]
             ]
