{-# LANGUAGE ScopedTypeVariables #-}
module Happstack.Server.StdConfig where

import Control.Monad.Trans
import Control.Monad
import Happstack.Server.SimpleHTTP
import Happstack.Server.HTTP.FileServe

-- | Is equal to "haskell/Main"
binarylocation :: String
binarylocation = "haskell/Main"

-- | Is equal to "public/log"
loglocation :: String
loglocation = "public/log"

-- | Convenience function around 'errorwrapper'
-- with the default binary location set to 'binarylocation' and the
-- log location set to 'loglocation'. 
errWrap :: (MonadPlus m, FilterMonad Response m, MonadIO m) => m Response
errWrap =  errorwrapper binarylocation loglocation
--stateFuns -- main actually has state so you can just import them
