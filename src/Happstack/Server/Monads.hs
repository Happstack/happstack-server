{-# LANGUAGE FlexibleContexts #-}
-- | This module provides four classes and some related functions
-- which provide 'ServerPartT' with much of its web-centric behavior.
--
--  1. 'ServerMonad' provides access to the HTTP 'Request'
--
--  2. 'FilterMonad' provides the ability to apply filters and transformations to a 'Response'
--
--  3. 'WebMonad' provides a way to escape a computation early and return a 'Response'
--
--  4. 'HasRqData' which provides access to the decoded QUERY_STRING and request body/form data
module Happstack.Server.Monads
    ( -- * ServerPartT
      ServerPartT
    , ServerPart
      -- * Happstack class
    , Happstack
      -- * ServerMonad
    , ServerMonad(..)
    , mapServerPartT
    , mapServerPartT'
    , UnWebT
    , filterFun
      -- * FilterMonad
    , FilterMonad(..)
    , ignoreFilters
    , addHeaderM
    , getHeaderM
    , setHeaderM
      -- * WebMonad
    , WebMonad(..)
    , escape
    , escape' 
      -- * MonadPlus helpers
    , require
    , requireM
    ) where

import Control.Applicative               (Alternative, Applicative)         
import Control.Monad                     (MonadPlus(mzero))
import Control.Monad.Trans               (MonadIO(..),MonadTrans(lift))
import qualified Data.ByteString.Char8   as B
import Happstack.Server.Internal.Monads
import Happstack.Server.Types            (Response, addHeader, getHeader, setHeader)
import Happstack.Server.RqData           (HasRqData)

-- | A class alias for all the classes a standard server monad (such as 'ServerPartT') is expected to have instances for. This allows you to keep your type signatures shorter and easier to understand.
class ( ServerMonad m, WebMonad Response m, FilterMonad Response m
      , MonadIO m, MonadPlus m, HasRqData m, Monad m, Functor m
      , Applicative m, Alternative m) => Happstack m


instance (Functor m, Monad m, MonadPlus m, MonadIO m) => Happstack (ServerPartT m)

-- | Get a header out of the request.
getHeaderM :: (ServerMonad m) => String -> m (Maybe B.ByteString)
getHeaderM a = askRq >>= return . (getHeader a)

-- | Add headers into the response.  This method does not overwrite
-- any existing header of the same name, hence the name 'addHeaderM'.
-- If you want to replace a header use 'setHeaderM'.
addHeaderM :: (FilterMonad Response m) => String -> String -> m ()
addHeaderM a v = composeFilter $ \res-> addHeader a v res

-- | Set a header into the response.  This will replace an existing
-- header of the same name.  Use 'addHeaderM' if you want to add more
-- than one header of the same name.
setHeaderM :: (FilterMonad Response m) => String -> String -> m ()
setHeaderM a v = composeFilter $ \res -> setHeader a v res

-- | Run an 'IO' action and, if it returns 'Just', pass it to the
-- second argument.
require :: (MonadIO m, MonadPlus m) => IO (Maybe a) -> (a -> m r) -> m r
require fn handle = do
    mbVal <- liftIO fn
    case mbVal of
        Nothing -> mzero
        Just a -> handle a

-- | A variant of require that can run in any monad, not just 'IO'.
requireM :: (MonadTrans t, Monad m, MonadPlus (t m)) => m (Maybe a) -> (a -> t m r) -> t m r
requireM fn handle = do
    mbVal <- lift fn
    case mbVal of
        Nothing -> mzero
        Just a -> handle a


