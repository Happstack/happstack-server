{-# LANGUAGE ScopedTypeVariables #-}
module Happstack.Server.StdConfig where

import Control.Monad.Trans
import Control.Monad
import Happstack.Server.SimpleHTTP
import Happstack.Server.HTTP.FileServe

binarylocation :: String
binarylocation = "haskell/Main"
loglocation :: String
loglocation = "public/log"


errWrap :: (MonadPlus m, FilterMonad Response m, MonadIO m) => m Response
errWrap =  errorwrapper binarylocation loglocation
--stateFuns -- main actually has state so you can just import them
