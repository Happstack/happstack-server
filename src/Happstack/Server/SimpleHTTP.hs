
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
-- Copyright   :  (c) Happstack.com 2009; (c) HAppS Inc 2007
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@vo.com
-- Stability   :  provisional
-- Portability :  requires mtl
--
-- 'SimpleHTTP' provides a back-end independent API for handling HTTP
-- requests.
--
-- By default, the built-in HTTP server will be used. However, other
-- back-ends like CGI\/FastCGI can be used if so desired.
--
-- So the general nature of 'simpleHTTP' is just what you'd expect
-- from a web application container.  First you figure out which
-- function is going to process your request, process the request to
-- generate a response, then return that response to the client. The
-- web application container is started with 'simpleHTTP', which takes
-- a configuration and a response-building structure ('ServerPartT'
-- which I'll return to in a moment), picks the first handler that is
-- willing to accept the request, and passes the request in to the
-- handler.  A simple \"hello world\" style Happstack 'simpleHTTP'
-- server looks like:
--
-- >  main = simpleHTTP nullConf $ return "Hello World!"
--
-- @simpleHTTP nullConf@ creates a HTTP server on port 8000.  return
-- \"Hello World!\" creates a 'ServerPartT' that just returns that
-- text.
--
-- 'ServerPartT' is the basic response builder.  As you might expect,
-- it's a container for a function that takes a 'Request' and converts
-- it to a response suitable for sending back to the server.  Most of
-- the time though you don't even need to worry about that as
-- 'ServerPartT' hides almost all the machinery for building your
-- response by exposing a few type classes.
--
-- 'ServerPartT' is a pretty rich monad.  You can interact with your
-- 'Request', your 'Response', do 'IO', etc.  Here is a do block that
-- validates basic authentication.  It takes a realm name as a string,
-- a Map of username to password and a server part to run if
-- authentication fails.
--
-- 'basicAuth' acts like a guard, and only produces a response when
-- authentication fails.  So put it before any 'ServerPartT' for which
-- you want to demand authentication, in any collection of
-- 'ServerPartT's.
--
-- > main = simpleHTTP nullConf $ myAuth, return "Hello World!"
-- >     where
-- >         myAuth = basicAuth' "Test"
-- >             (M.fromList [("hello", "world")]) (return "Login Failed")
--
-- > basicAuth' realmName authMap unauthorizedPart =
-- >    do
-- >        let validLogin name pass = M.lookup name authMap == Just pass
-- >        let parseHeader = break (':'==) . Base64.decode . B.unpack . B.drop 6
-- >        authHeader <- getHeaderM "authorization"
-- >        case authHeader of
-- >            Nothing -> err
-- >            Just x  -> case parseHeader x of
-- >                (name, ':':pass) | validLogin name pass -> mzero
-- >                                   | otherwise -> err
-- >                _                                       -> err
-- >    where
-- >        err = do
-- >            unauthorized ()
-- >            setHeaderM headerName headerValue
-- >            unauthorizedPart
-- >        headerValue = "Basic realm=\"" ++ realmName ++ "\""
-- >        headerName  = "WWW-Authenticate"
--
-- Here is another example that uses 'liftIO' to embed IO in a request process:
--
-- >  main = simpleHTTP nullConf $ myPart
-- >  myPart = do
-- >    line <- liftIO $ do -- IO
-- >        putStr "return? "
-- >        getLine
-- >    when (take 2 line /= "ok") $ (notfound () >> return "refused")
-- >    return "Hello World!"
--
-- This example will ask in the console \"return? \" if you type \"ok\" it will
-- show \"Hello World!\" and if you type anything else it will return a 404.
--
-----------------------------------------------------------------------------
module Happstack.Server.SimpleHTTP
    ( module Happstack.Server.Types
    , module Happstack.Server.Internal.Monads

    -- * SimpleHTTP
    , simpleHTTP
    , simpleHTTP'
    , simpleHTTP''
    , simpleHTTPWithSocket
    , simpleHTTPWithSocket'
    , bindPort
    , parseConfig
    -- * Basic ServerMonad functionality
    , module Happstack.Server.Monads
    -- * HTTP Realm Authentication
    , module Happstack.Server.Auth
    -- * Create and Set Cookies (see also "Happstack.Server.RqData")
    , module Happstack.Server.Cookie
   -- * Error Handling
    , module Happstack.Server.Error
    -- * Manipulating responses
    , module Happstack.Server.Response
    -- * guards and building blocks for routing requests
    , module Happstack.Server.Routing
      -- * proxying
    , module Happstack.Server.Proxy
     -- * Look up values in Query String, Request Body, and Cookies
    , module Happstack.Server.RqData
      -- * Output Validation
    , module Happstack.Server.Validation
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
import qualified Happstack.Server.Internal.Listen as Listen (listen, listen',listenOn) -- So that we can disambiguate 'Writer.listen'
import Happstack.Server.Types                    (Conf(port, validator), Request, Response(rsBody, rsCode), nullConf, setHeader)
import Network                                   (Socket)
import qualified Paths_happstack_server          as Cabal
import System.Console.GetOpt                     ( OptDescr(Option)
                                                 , ArgDescr(ReqArg)
                                                 , ArgOrder(Permute)
                                                 , getOpt
                                                 )

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

-- | Use the built-in web-server to serve requests according to a
-- 'ServerPartT'.  Use 'msum' to pick the first handler from a list of
-- handlers that doesn't call 'mzero'. This function always binds o
-- IPv4 ports until Network module is fixed to support IPv6 in a
-- portable way. Use 'simpleHTTPWithSocket' with custom socket if you
-- want different behaviour.
simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()
simpleHTTP = simpleHTTP' id

-- | A combination of 'simpleHTTP''' and 'mapServerPartT'.  See
-- 'mapServerPartT' for a discussion of the first argument of this
-- function. This function always binds to IPv4 ports until Network
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
simpleHTTPWithSocket :: (ToMessage a) => Socket -> Conf -> ServerPartT IO a -> IO ()
simpleHTTPWithSocket = simpleHTTPWithSocket' id

-- | Like 'simpleHTTP'' with a socket.
simpleHTTPWithSocket' :: (ToMessage b, Monad m, Functor m) => (UnWebT m a -> UnWebT IO b)
                      -> Socket -> Conf -> ServerPartT m a -> IO ()
simpleHTTPWithSocket' toIO socket conf hs =
    Listen.listen' socket conf (\req -> runValidator (fromMaybe return (validator conf)) =<< (simpleHTTP'' (mapServerPartT toIO hs) req))

-- | Bind port and return the socket for 'simpleHTTPWithSocket'. This
-- function always binds to IPv4 ports until Network module is fixed
-- to support IPv6 in a portable way.
bindPort :: Conf -> IO Socket
bindPort conf = Listen.listenOn (port conf)

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
