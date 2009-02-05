{-# LANGUAGE ScopedTypeVariables #-}
module HAppS.Server.StdConfig where

import Control.Monad.Trans
import HAppS.Server.SimpleHTTP
import HAppS.Server.HTTP.FileServe

binarylocation :: String
binarylocation = "haskell/Main"
loglocation :: String
loglocation = "public/log"


errWrap :: MonadIO m => ServerPartT m Response
errWrap =  errorwrapper binarylocation loglocation
--stateFuns -- main actually has state so you can just import them
