
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Server.SimpleHTTP
-- Copyright   :  (c) Happstack.com 2010; (c) HAppS Inc 2007
-- License     :  BSD-like
--
-- Maintainer  :  Happstack team <happs@googlegroups.com>
-- Stability   :  provisional
-- Portability :  requires mtl
--
-- 'simpleHTTP' is a self-contained HTTP server which can be used to
-- run a 'ServerPart'.
--
-- A very simple, \"Hello World!\" web app looks like:
-- 
-- > import Happstack.Server
-- > main = simpleHTTP nullConf $ ok "Hello World!"
--
-- By default the server will listen on port 8000. Run the app and point your browser at: <http://localhost:8000/>
--
-- For FastCGI support see: <http://hackage.haskell.org/package/happstack-fastcgi>
-----------------------------------------------------------------------------
module Happstack.Server.SimpleHTTP
    ( -- * SimpleHTTP
      simpleHTTP
    , simpleHTTP'
    , simpleHTTP''
    , simpleHTTPWithSocket
    , simpleHTTPWithSocket'
    , bindPort
    , bindIPv4
    , parseConfig
    , waitForTermination
    -- * Re-exported modules
    -- ** Basic ServerMonad functionality
    , module Happstack.Server.Monads
    -- ** HTTP Realm Authentication
    , module Happstack.Server.Auth
    -- ** Create and Set Cookies (see also "Happstack.Server.RqData")
    , module Happstack.Server.Cookie
    -- ** Error Handling
    , module Happstack.Server.Error
    -- ** Creating Responses
    , module Happstack.Server.Response
    -- ** Request Routing
    , module Happstack.Server.Routing
    -- ** Proxying
    , module Happstack.Server.Proxy
    -- ** Looking up values in Query String, Request Body, and Cookies
    , module Happstack.Server.RqData
    -- ** Output Validation
    , module Happstack.Server.Validation
    , module Happstack.Server.Types
--    , module Happstack.Server.Internal.Monads

    ) where

-- re-exports

import Happstack.Server.Auth
import Happstack.Server.Monads
import Happstack.Server.Cookie
import Happstack.Server.Error
import Happstack.Server.Types
import Happstack.Server.Proxy
import Happstack.Server.Routing
import Happstack.Server.RqData
import Happstack.Server.Response
import Happstack.Server.Validation


import Data.Maybe                                (fromMaybe)
import qualified Data.Version                    as DV
import Happstack.Server.Internal.Monads          (FilterFun, WebT(..), UnWebT, unFilterFun, mapServerPartT, runServerPartT, ununWebT)
import qualified Happstack.Server.Internal.Listen as Listen (listen, listen',listenOn, listenOnIPv4) -- So that we can disambiguate 'Writer.listen'
import Happstack.Server.Types                    (Conf(port, validator), Request, Response(rsBody, rsCode), nullConf, setHeader)
import Network                                   (Socket)
import qualified Paths_happstack_server          as Cabal
import System.Console.GetOpt                     ( OptDescr(Option)
                                                 , ArgDescr(ReqArg)
                                                 , ArgOrder(Permute)
                                                 , getOpt
                                                 )
#ifdef UNIX
import Control.Concurrent.MVar
import System.Posix.Signals hiding (Handler)
import System.Posix.IO ( stdInput )
import System.Posix.Terminal ( queryTerminal )
#endif

-- | An array of 'OptDescr', useful for processing command line
-- options into an 'Conf' for 'simpleHTTP'.
ho :: [OptDescr (Conf -> Conf)]
ho = [Option [] ["http-port"] (ReqArg (\h c -> c { port = read h }) "port") "port to bind http server"]

-- | Parse command line options into a 'Conf'.
parseConfig :: [String] -> Either [String] Conf
parseConfig args
    = case getOpt Permute ho args of
        (flags,_,[]) -> Right $ foldr ($) nullConf flags
        (_,_,errs)   -> Left errs

-- |start the server, and handle requests using the supplied
-- 'ServerPart'.
--
-- This function will not return, though it may throw an exception.
--
-- NOTE: The server will only listen on IPv4 due to portability issues
-- in the "Network" module. For IPv6 support, use
-- 'simpleHTTPWithSocket' with custom socket.
simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()
simpleHTTP = simpleHTTP' id

-- | A combination of 'simpleHTTP''' and 'mapServerPartT'.  See
-- 'mapServerPartT' for a discussion of the first argument of this
-- function. 
--
-- NOTE: This function always binds to IPv4 ports until Network
-- module is fixed to support IPv6 in a portable way. Use
-- 'simpleHTTPWithSocket' with custom socket if you want different
-- behaviour.
simpleHTTP' :: (ToMessage b, Monad m, Functor m) => (UnWebT m a -> UnWebT IO b)
            -> Conf -> ServerPartT m a -> IO ()
simpleHTTP' toIO conf hs =
    Listen.listen conf (\req -> runValidator (fromMaybe return (validator conf)) =<< (simpleHTTP'' (mapServerPartT toIO hs) req))


-- | Generate a result from a 'ServerPartT' and a 'Request'. This is
-- mainly used by CGI (and fast-cgi) wrappers.
simpleHTTP'' :: (ToMessage b, Monad m, Functor m) => ServerPartT m b -> Request -> m Response
simpleHTTP'' hs req =  (runWebT $ runServerPartT hs req) >>= (return . (maybe standardNotFound id))
    where
        standardNotFound = setHeader "Content-Type" "text/html" $ (toResponse notFoundHtml){rsCode=404}

-- | Run 'simpleHTTP' with a previously bound socket. Useful if you
-- want to run happstack as user on port 80. Use something like this:
--
-- > import System.Posix.User (setUserID, UserEntry(..), getUserEntryForName)
-- >
-- > main = do
-- >     let conf = nullConf { port = 80 }
-- >     socket <- bindPort conf
-- >     -- do other stuff as root here
-- >     getUserEntryForName "www" >>= setUserID . userID
-- >     -- finally start handling incoming requests
-- >     tid <- forkIO $ simpleHTTPWithSocket socket conf impl
--
-- Note: It's important to use the same conf (or at least the same
-- port) for 'bindPort' and 'simpleHTTPWithSocket'.
--
-- see also: 'bindPort', 'bindIPv4'
simpleHTTPWithSocket :: (ToMessage a) => Socket -> Conf -> ServerPartT IO a -> IO ()
simpleHTTPWithSocket = simpleHTTPWithSocket' id

-- | Like 'simpleHTTP'' with a socket.
simpleHTTPWithSocket' :: (ToMessage b, Monad m, Functor m) => (UnWebT m a -> UnWebT IO b)
                      -> Socket -> Conf -> ServerPartT m a -> IO ()
simpleHTTPWithSocket' toIO socket conf hs =
    Listen.listen' socket conf (\req -> runValidator (fromMaybe return (validator conf)) =<< (simpleHTTP'' (mapServerPartT toIO hs) req))

-- | Bind port and return the socket for use with 'simpleHTTPWithSocket'. This
-- function always binds to IPv4 ports until Network module is fixed
-- to support IPv6 in a portable way.
bindPort :: Conf -> IO Socket
bindPort conf = Listen.listenOn (port conf)

-- | Bind to ip and port and return the socket for use with 'simpleHTTPWithSocket'.
--
-- >
-- > import Happstack.Server
-- >
-- > main = do let conf = nullConf
-- >               addr = "127.0.0.1"
-- >           s <- bindIPv4 addr (port conf)
-- >           simpleHTTPWithSocket s conf $ ok $ toResponse $ 
-- >             "now listening on ip addr " ++ addr ++ 
-- >             " and port " ++ show (port conf)
--
bindIPv4 :: String  -- ^ IP address to bind to (must be an IP address and not a host name)
         -> Int     -- ^ port number to bind to
         -> IO Socket
bindIPv4 addr prt = Listen.listenOnIPv4 addr prt

-- | Takes your 'WebT', if it is 'mempty' it returns 'Nothing' else it
-- converts the value to a 'Response' and applies your filter to it.
runWebT :: forall m b. (Functor m, ToMessage b) => WebT m b -> m (Maybe Response)
runWebT = (fmap . fmap) appFilterToResp . ununWebT
    where
      appFilterToResp :: (Either Response b, FilterFun Response) -> Response
      appFilterToResp (e, ff) = unFilterFun ff $ either id toResponse e

notFoundHtml :: String
notFoundHtml = 
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
    ++ "<html><head><title>Happstack "
    ++ ver ++ " File not found</title></head>"
    ++ "<body><h1>Happstack " ++ ver ++ "</h1>"
    ++ "<p>Your file is not found<br>"
    ++ "To try again is useless<br>"
    ++ "It is just not here</p>"
    ++ "</body></html>"
    where ver = DV.showVersion Cabal.version


-- | Wait for a signal.
--   On unix, a signal is sigINT or sigTERM (aka Control-C).
--  
-- On windows, the signal is entering: e <return>
waitForTermination :: IO ()
waitForTermination
    = do
#ifdef UNIX
         istty <- queryTerminal stdInput
         mv <- newEmptyMVar
         installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
         case istty of
           True  -> do installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing
                       return ()
           False -> return ()
         takeMVar mv
#else
         let loop 'e' = return () 
             loop _   = getChar >>= loop
         loop 'c'
#endif
