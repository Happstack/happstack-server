{-# LANGUAGE UndecidableInstances, OverlappingInstances, ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances,
    MultiParamTypeClasses, PatternGuards, FlexibleContexts, FunctionalDependencies, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

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
-- SimpleHTTP provides a back-end independent API for handling HTTP requests.
--
-- By default, the built-in HTTP server will be used. However, other back-ends
-- like CGI\/FastCGI can used if so desired.
-- 
-- So the general nature of 'simpleHTTP' is no different than what you'd expect
-- from a web application container.  First you figure out when function is
-- going to process your request, process the request to generate a response,
-- then return that response to the client. The web application container is
-- started with 'simpleHTTP', which takes a configuration and a list of
-- possible response-building structures ('ServerPartT' which I'll return too
-- in a moment), and picks the first response builder that is willing to accept
-- the request, passes the request into the response builder.  A simple "hello world" style
-- HAppS simpleHTTP server looks like:
--
-- @
--   main = simpleHTTP nullConf [ return \"Hello World!\" ]
-- @
--
-- @simpleHTTP nullConf@ creates a HTTP server on port 8000.
-- return \"Hello World!\" creates a serverPartT that just returns that text.
--
-- 'ServerPartT' is the basic response builder.  As you might expect, it's a
-- container for a function that takes a Request and converts it a response
-- suitable for sending back to the server.  Most of the time though you don't
-- even need to worry about that as ServerPartT hides almost all the machinery
-- for building your response by exposing a few type classes.
-- 
-- 'ServerPartT' is a pretty rich monad.  You can interact with your request,
-- your response, do IO, etc.  Here is a do block that validates basic
-- authentication It takes a realm name as a string, a Map of username to
-- password and a server part to run if authentication fails.
-- 
-- @basicAuth'@ acts like a guard, and only produces a response when
-- authentication fails.  So put it before any ServerPartTs you want
-- to demand authentication for in any list of ServerPartTs.
--
-- @
--
-- main = simpleHTTP nullConf [ myAuth, return \"Hello World!\" ]
--     where
--         myAuth = basicAuth\' \"Test\"
--             (M.fromList [(\"hello\", \"world\")]) (return \"Login Failed\")
--
-- basicAuth\' realmName authMap unauthorizedPart =
--    do
--        let validLogin name pass = M.lookup name authMap == Just pass
--        let parseHeader = break (\':\'==) . Base64.decode . B.unpack . B.drop 6
--        authHeader <- getHeaderM \"authorization\"
--        case authHeader of
--            Nothing -> err
--            Just x  -> case parseHeader x of 
--                (name, \':\':pass) | validLogin name pass -> mzero
--                _                                       -> err
--    where
--        err = do
--            unauthorized ()
--            addHeaderM headerName headerValue
--            unauthorizedPart
--        headerValue = \"Basic realm=\\\"\" ++ realmName ++ \"\\\"\"
--        headerName  = \"WWW-Authenticate\"
-- @
--
-- Here is another example that uses liftIO to embed IO in a request process
--
-- @
--   main = simpleHTTP nullConf [ myPart ]
--   myPart = do
--     line <- liftIO $ do -- IO
--         putStr \"return? \"
--         getLine
--     when (take 2 line \/= \"ok\") $ (notfound () >> return \"refused\")
--     return \"Hello World!\"
-- @
-- 
-- This example will ask in the console \"return? \" if you type \"ok\" it will
-- show \"Hello World!\" and if you type anything else it will return a 404.
--
-----------------------------------------------------------------------------
module Happstack.Server.SimpleHTTP
    ( module Happstack.Server.HTTP.Types
    , module Happstack.Server.Cookie
    , -- * SimpleHTTP
      simpleHTTP -- , simpleHTTP'
    , parseConfig
    -- * ServerPartT
    , ServerPartT(..)
    , ServerPart
    , runServerPartT
    , withRequest
    , anyRequest
    -- * WebT
    , WebT(..)
    , Web
    , mkWebT
    , ununWebT
    , runWebT
    -- * Type Classes
    , FromReqURI(..)
    , ToMessage(..)

      -- * Manipulating requests
    , FromData(..)
    , ServerMonad(..)
    , RqData
    , noHandle
    , getHeaderM
    , escape
    , escape'
    , multi
      -- * Manipulating responses
    , FilterMonad(..)
    , WebMonad(..)
    , ok
    , modifyResponse
    , setResponseCode
    , badGateway
    , internalServerError
    , badRequest
    , unauthorized 
    , forbidden
    , notFound
    , seeOther
    , found
    , movedPermanently
    , tempRedirect
    , addCookie
    , addCookies
    , addHeaderM

     -- * guards and building blocks
    , dir
    , method
    , methodSP
    , path
    , anyPath
    , anyPath'
    , withData
    , withDataFn
    , require
    , basicAuth
    , uriRest
      -- * Processing requests
    , webQuery
    , webUpdate
    , flatten
    , localContext
      -- * proxying
    , proxyServe
    , rproxyServe
      -- * unknown
    , debugFilter
    , applyRequest
      -- * Parsing input and cookies
    , lookInput   -- :: String -> Data Input
    , lookBS      -- :: String -> Data B.ByteString
    , look        -- :: String -> Data String
    , lookCookie  -- :: String -> Data Cookie
    , lookCookieValue -- :: String -> Data String
    , readCookieValue -- :: Read a => String -> Data a
    , lookRead    -- :: Read a => String -> Data a
    , lookPairs
      -- * XSLT
    , xslt ,doXslt
      -- * Error Handlng
    , errorHandlerSP
    , simpleErrorHandler
      -- * Output Validation
    , setValidator
    , setValidatorSP
    , validateConf
    , runValidator
    , wdgHTMLValidator
    , noopValidator
    , lazyProcValidator
    ) where
import Happstack.Server.HTTP.Client
import Happstack.Data.Xml.HaXml
import qualified Happstack.Server.MinHaXML as H

import Happstack.Server.HTTP.Types hiding (Version(..))
import qualified Happstack.Server.HTTP.Types as Types
import Happstack.Server.HTTP.Listen as Listen
import Happstack.Server.XSLT
import Happstack.Server.SURI (ToSURI)
import Happstack.Util.Common
import Happstack.Server.Cookie
import Happstack.State (QueryEvent, UpdateEvent, query, update)
import Happstack.Data -- used by default implementation of fromData
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Trans()
import Control.Monad.Maybe
import Control.Monad.Writer as Writer
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Generics as G
import qualified Data.Map as M
import Text.Html (Html,renderHtml)
import qualified Text.XHtml as XHtml (Html,renderHtml)
import qualified Happstack.Crypto.Base64 as Base64
import Data.Char
import Data.List
import System.IO
import System.Console.GetOpt
import System.Process (runInteractiveProcess, waitForProcess)
import System.Exit
import Text.Show.Functions ()

-- | An alias for WebT when using the IO monad (which is most of the time)
type Web a = WebT IO a
-- | An alias for using ServerPartT when using the IO monad (again, this is most of the time)
type ServerPart a = ServerPartT IO a


-- HERE BEGINS ServerPartT definitions

-- | ServerPartT is a container for processing requests and returning results
newtype ServerPartT m a = ServerPartT { unServerPartT :: ReaderT Request (WebT m) a }
    deriving (Monad, MonadIO, MonadPlus, Functor)

runServerPartT :: ServerPartT m a -> Request -> WebT m a
runServerPartT = runReaderT . unServerPartT

withRequest :: (Request -> WebT m a) -> ServerPartT m a
withRequest = ServerPartT . ReaderT

instance MonadTrans (ServerPartT) where
    lift m = withRequest (\_ -> lift m)

instance (Monad m) => Monoid (ServerPartT m a)
 where mempty = mzero
       mappend = mplus

instance (Monad m, Functor m) => Applicative (ServerPartT m) where
    pure = return
    (<*>) = ap

instance (Monad m, MonadWriter w m) => MonadWriter w (ServerPartT m) where
    tell = lift . tell
    listen m = withRequest $ \rq ->  Writer.listen (runServerPartT m rq) >>= return
    pass m = withRequest $ \rq -> pass (runServerPartT m rq) >>= return

instance (Monad m, MonadError e m) => MonadError e (ServerPartT m) where
    throwError e = lift $ throwError e
    catchError action handler = withRequest $ \rq -> (runServerPartT action rq) `catchError` ((flip runServerPartT $ rq) . handler)

instance (Monad m, MonadReader r m) => MonadReader r (ServerPartT m) where
    ask = lift ask
    local fn m = withRequest $ \rq-> local fn (runServerPartT m rq)

instance Monad m => FilterMonad Response (ServerPartT m) where
    setFilter = anyRequest . setFilter
    composeFilter = anyRequest . composeFilter
    applyFilter f m = withRequest $ \rq -> applyFilter f (runServerPartT m rq)
    getFilter m = withRequest $ \rq -> getFilter (runServerPartT m rq)

instance Monad m => WebMonad (ServerPartT m) where
    finishWith r = anyRequest $ finishWith r

-- | yes, this is exactly like 'ReaderT' with new names.
-- Why you ask? Because ServerT can lift up a ReaderT.
-- If you did that, it would shadow ServerT's behavior
-- as a ReaderT, thus meaning if you lifted the ReaderT
-- you could no longer modify the Request.  This way
-- you can add a ReaderT to your monad stack without
-- any trouble.
class ServerMonad m where
    askRq :: m Request
    localRq :: (Request->Request)->m a->m a

instance (Monad m) => ServerMonad (ServerPartT m) where
    askRq = ServerPartT $ ask
    localRq f m = ServerPartT $ local f (unServerPartT m)

-- HERE BEGINS WebT definitions
data SetAppend a = Set a | Append a

instance Monoid a => Monoid (SetAppend a) where
   mempty = Append mempty
   Set x    `mappend` Append y = Set (x `mappend` y)
   Append x `mappend` Append y = Append (x `mappend` y)
   _        `mappend` Set y    = Set y

value :: SetAppend t -> t
value (Set x) = x
value (Append x) = x

instance Functor (SetAppend) where
    fmap f (Set x) = Set $ f x
    fmap f (Append x) = Append $ f x

newtype FilterT a m b =
   FilterT { unFilterT :: WriterT (SetAppend (Endo a)) m b }
   deriving (Monad, MonadTrans, Functor, MonadIO)

-- | A set of functions for manipulating filters.
-- A ServerPartT implements FilterMonad Response
-- so these methods are the fundamental ways
-- of manipulating the response object.
class FilterMonad a m | m->a where
    -- | Ignores all previous
    -- alterations to your filter
    --
    -- As an example:
    --
    -- @
    --   do
    --     composeFilter f
    --     setFilter g
    --     return \"Hello World\"
    -- @
    --
    -- setFilter g will cause the first composeFilter to be
    -- ignored.
    setFilter :: (a->a) -> m ()
    -- |
    -- composes your filter function with the
    -- existing filter function.
    composeFilter :: (a->a) -> m ()
    -- while applying your filter to your result.
    applyFilter :: (b->a) -- ^ A function for converting your monad to your filtered type
                   -> m b -- ^ The monad you to apply the filters of
                   -> m a -- ^ The result of your filter application.  This monad should have the
                          -- identity filter.
    -- | retrives the filter from the environment
    getFilter :: m b -> m (b,a->a)


instance (Monad m) => FilterMonad a (FilterT a m) where
    setFilter f = FilterT $ Writer.tell $ Set $ Endo f
    composeFilter f = FilterT $ Writer.tell $ Append $ Endo f
    applyFilter g fm =  FilterT $ do
        (b,sa) <- Writer.listen (unFilterT fm)
        tell $ Set $ Endo id
        return $ (appEndo $ value sa) (g b)
    getFilter m = FilterT $ Writer.listens (appEndo . value)  (unFilterT m) 

newtype WebT m a = WebT { unWebT :: ErrorT Response (FilterT (Either Response Response) (MaybeT m)) a }
    deriving (Monad, MonadIO, Functor)
    
instance Error Response where
    strMsg = toResponse

class WebMonad m where
    -- | A control structure
    -- It ends the computation and returns the Response you passed into it
    -- immediately.
    finishWith :: Response -> m Response

instance (Monad m) => WebMonad (WebT m) where
    finishWith r = WebT $ throwError r

instance MonadTrans WebT where
    lift = WebT . lift . lift . lift

instance (Monad m) => MonadPlus (WebT m) where
    -- | Aborts a computation.
    --
    -- This is primarily useful because msum will take an array
    -- of MonadPlus and return the first one that isn't mzero,
    -- which is exactly the semantics expected from objects
    -- that take arrays of WebT or ServerPartT
    mzero = WebT $ lift $ lift $ mzero
    mplus x y =  WebT $ ErrorT $ FilterT $ (lower x) `mplus` (lower y)
        where lower = (unFilterT . runErrorT . unWebT)

-- | deprecated.  use mzero
noHandle :: (MonadPlus m) => m a
noHandle = mzero
-- rproxy' requires a lifting of the monomorphism restriction
-- so we can make nohandle a direction alias of mzero
{-# DEPRECATED noHandle "Use mzero" #-}

instance (Monad m) => FilterMonad Response (WebT m) where
    setFilter f = WebT $ lift $ setFilter $ either (Left . f) (Right . f)
    composeFilter f = WebT . lift . composeFilter $ either (Left . f) (Right . f)
    applyFilter g fm = WebT $ ErrorT $ applyFilter (fmap g) (runErrorT $ unWebT fm)
    getFilter m = WebT $ ErrorT $ getFilter (runErrorT $ unWebT m) >>= liftWebT
        where liftWebT (Left r, _) = return $ Left r
              -- This function looks odd.  Since our filter is applied to left and right exactly the same,
              -- (see setFilter and composeFilter above), we need to convert our Either Response Response ->
              -- Either Response Response to a Response->Response method.  Since both branches are the same
              -- we pick the Right side
              liftWebT (Right a, f) = return $ Right (a, (either (id) (id)) . (\x -> f (Right x)))

instance (Monad m) => Monoid (WebT m a) where
    mempty = mzero
    mappend = mplus

runWebT :: (ToMessage b, Monad m) => WebT m b -> m (Maybe Response)
runWebT m = runMaybeT $ do
                (r,_) <- (runWriterT $ unFilterT $ runErrorT $ unWebT $
                    applyFilter toResponse m)
                return $ (either id id) r

ununWebT :: WebT m a
    -> m (Maybe
            (Either Response a,
             SetAppend (Endo (Either Response Response))))
ununWebT = runMaybeT . runWriterT . unFilterT . runErrorT . unWebT

mkWebT :: m (Maybe
       (Either Response a,
        SetAppend (Endo (Either Response Response)))) -> WebT m a
mkWebT = WebT . ErrorT . FilterT . WriterT . MaybeT

instance (Monad m, Functor m) => Applicative (WebT m) where
    pure = return
    (<*>) = ap

instance MonadReader r m => MonadReader r (WebT m) where
    ask = lift ask
    local fn m = mkWebT $ local fn (ununWebT m)

instance MonadState st m => MonadState st (WebT m) where
    get = lift get
    put = lift . put

instance MonadError e m => MonadError e (WebT m) where
	throwError err = lift $ throwError err
 	catchError action handler = mkWebT $ catchError (ununWebT action) (ununWebT . handler)

instance MonadWriter w m => MonadWriter w (WebT m) where
    tell = lift . Writer.tell
    listen m = mkWebT $ Writer.listen (ununWebT m) >>= (return . liftWebT)
        where liftWebT (Nothing, _) = Nothing
              liftWebT (Just (Left x,f), _) = Just (Left x,f)
              liftWebT (Just (Right x,f),w) = Just (Right (x,w),f)
    pass m = mkWebT $ ununWebT m >>= liftWebT
        where liftWebT Nothing = return Nothing
              liftWebT (Just (Left x,f)) = return $ Just (Left x, f)
              liftWebT (Just (Right x,f)) = pass (return x)>>= (\a -> return $ Just (Right a,f))

-- | An alias for setFilter id
-- It resets all your filters
ignoreFilters :: (FilterMonad a m) => m ()
ignoreFilters = setFilter id

-- | Used to ignore all your filters
-- and immediately end the computation.  A combination of
-- 'ignoreFilters' and 'finishWith'
escape :: (WebMonad m, FilterMonad a m, Monad m) => m Response -> m Response
escape gen = ignoreFilters >> gen >>= finishWith

-- | An alternate form of 'escape' that can
-- be easily used within a do block.
escape' :: (WebMonad m, FilterMonad a m, Monad m) => Response -> m Response
escape' a = ignoreFilters >> finishWith a

-- | An array of 'OptDescr', useful for processing
-- command line options into an 'Conf' for 'simpleHTTP'
ho :: [OptDescr (Conf -> Conf)]
ho = [Option [] ["http-port"] (ReqArg (\h c -> c { port = read h }) "port") "port to bind http server"]

-- | parseConfig tries to parse your command line options
-- into a Conf.
parseConfig :: [String] -> Either [String] Conf
parseConfig args
    = case getOpt Permute ho args of
        (flags,_,[]) -> Right $ foldr ($) nullConf flags
        (_,_,errs)   -> Left errs

-- | Use the built-in web-server to serve requests according to list of 'ServerPartT's.
-- The array of ServerPartT is passed though msum to pick the first handler that didn't 
-- call noHandle
simpleHTTP :: (ToMessage a) => Conf -> [ServerPartT IO a] -> IO ()
simpleHTTP conf hs = 
    Listen.listen conf (\req -> runValidator (fromMaybe return (validator conf)) =<< simpleHTTP' hs req)


-- | Generate a result from a list of 'ServerParts' and a 'Request'. This is mainly used
-- by CGI (and fast-cgi) wrappers.
simpleHTTP' :: (ToMessage b, Monad m) => [ServerPartT m b] -> Request -> m Response
simpleHTTP' hs req =  (runWebT $ runServerPartT (msum hs) req) >>= (return . (maybe standardNotFound id))
    where
        standardNotFound = result 404 "No suitable handler found"

class FromReqURI a where
    fromReqURI :: String -> Maybe a



instance FromReqURI String where fromReqURI = Just
instance FromReqURI Int where    fromReqURI = readM
instance FromReqURI Integer where    fromReqURI = readM
instance FromReqURI Float where  fromReqURI = readM
instance FromReqURI Double where fromReqURI = readM

type RqData a = ReaderT ([(String,Input)], [(String,Cookie)]) Maybe a

class FromData a where
    fromData :: RqData a

instance (Eq a,Show a,Xml a,G.Data a) => FromData a where
    fromData = do mbA <- lookPairs >>= return . normalize . fromPairs
                  case mbA of
                    Just a -> return a
                    Nothing -> fail "FromData G.Data failure"
--    fromData = lookPairs >>= return . normalize . fromPairs

instance (FromData a, FromData b) => FromData (a,b) where
    fromData = liftM2 (,) fromData fromData
instance (FromData a, FromData b, FromData c) => FromData (a,b,c) where
    fromData = liftM3 (,,) fromData fromData fromData
instance (FromData a, FromData b, FromData c, FromData d) => FromData (a,b,c,d) where
    fromData = liftM4 (,,,) fromData fromData fromData fromData
instance FromData a => FromData (Maybe a) where
    fromData = fmap Just fromData `mplus` return Nothing

{- |
  Minimal definition: 'toMessage'
-}


class ToMessage a where
    toContentType :: a -> B.ByteString
    toContentType _ = B.pack "text/plain"
    toMessage :: a -> L.ByteString
    toMessage = error "Happstack.Server.SimpleHTTP.ToMessage.toMessage: Not defined"
    toResponse:: a -> Response
    toResponse val =
        let bs = toMessage val
            res = Response 200 M.empty nullRsFlags bs Nothing
        in setHeaderBS (B.pack "Content-Type") (toContentType val)
           res

instance ToMessage [Element] where
    toContentType _ = B.pack "application/xml"
    toMessage [el] = L.pack $ H.simpleDoc H.NoStyle $ toHaXmlEl el -- !! OPTIMIZE
    toMessage x    = error ("Happstack.Server.SimpleHTTP 'instance ToMessage [Element]' Can't handle " ++ show x)




instance ToMessage () where
    toContentType _ = B.pack "text/plain"
    toMessage () = L.empty
instance ToMessage String where
    toContentType _ = B.pack "text/plain"
    toMessage = L.pack
instance ToMessage Integer where
    toMessage = toMessage . show
instance ToMessage a => ToMessage (Maybe a) where
    toContentType _ = toContentType (undefined :: a)
    toMessage Nothing = toMessage "nothing"
    toMessage (Just x) = toMessage x


instance ToMessage Html where
    toContentType _ = B.pack "text/html"
    toMessage = L.pack . renderHtml

instance ToMessage XHtml.Html where
    toContentType _ = B.pack "text/html"
    toMessage = L.pack . XHtml.renderHtml

instance ToMessage Response where
    toResponse = id

instance (Xml a)=>ToMessage a where
    toContentType = toContentType . toXml
    toMessage = toMessage . toPublicXml

--    toMessageM = toMessageM . toPublicXml


class MatchMethod m where matchMethod :: m -> Method -> Bool
instance MatchMethod Method where matchMethod m = (== m) 
instance MatchMethod [Method] where matchMethod methods = (`elem` methods)
instance MatchMethod (Method -> Bool) where matchMethod f = f 
instance MatchMethod () where matchMethod () _ = True

webQuery :: (MonadIO m, QueryEvent ev res) => ev -> m res
webQuery = liftIO . query

webUpdate :: (MonadIO m, UpdateEvent ev res) => ev -> m res
webUpdate = liftIO . update

-- | flatten turns your arbitrary @m a@ and converts it too
-- a @m 'Response'@ with @'toResponse'@
flatten :: (ToMessage a, Functor f) => f a -> f Response
flatten = fmap toResponse

-- | TODO
-- This is for applying a WebT function to the msum of the array argument, then
-- stuffing it back into a ServerPartT.  Why would you need this?
localContext :: Monad m => (WebT m a -> WebT m' a) -> [ServerPartT m a] -> ServerPartT m' a
localContext fn hs
    = withRequest $ \rq -> fn (runServerPartT (msum hs) rq)


-- | Pop a path element and run the @[ServerPartT]@ if it matches the given string.
dir :: Monad m => String -> [ServerPartT m a] -> ServerPartT m a
dir staticPath handle
    = withRequest $ \rq -> case rqPaths rq of
                     (p:xs) | p == staticPath -> 
                                   runServerPartT (msum handle) rq{rqPaths = xs}
                     _ -> noHandle

getHeaderM :: (Monad m) => String -> ServerPartT m (Maybe B.ByteString)
getHeaderM a = withRequest $ \rq -> return $ getHeader a rq

addHeaderM :: (FilterMonad Response m) => String -> String -> m ()
addHeaderM a v = composeFilter $ \res-> addHeader a v res

-- | Guard against the method. Note, this function also guards against any
--   remaining path segments. See 'anyRequest'.
methodSP :: (MatchMethod method, Monad m) => method -> ServerPartT m a -> ServerPartT m a
methodSP m handle
    = withRequest $ \rq -> if matchMethod m (rqMethod rq) && null (rqPaths rq)
                           then runServerPartT handle rq
                           else noHandle

-- | Guard against the method. Note, this function also guards against any
--   remaining path segments. See 'anyRequest'.
method :: (MatchMethod method, Monad m) => method -> WebT m a -> ServerPartT m a
method m handle = methodSP m (withRequest $ \_ -> handle)


-- | Pop a path element and parse it.
path :: (FromReqURI a, Monad m) => (a -> [ServerPartT m r]) -> ServerPartT m r
path handle
    = withRequest $ \rq -> 
      case rqPaths rq of
               (p:xs) | Just a <- fromReqURI p
                                  -> runServerPartT (multi $ handle a) rq{rqPaths = xs}
               _ -> noHandle

-- | grabs the rest of the URL (dirs + query) and passes it to your handler
uriRest :: (ServerMonad m, Monad m) => (String -> m a) -> m a
uriRest handle = askRq >>= handle . rqURL

-- | pops any path element and ignores when chosing a ServerPartT to handle the
-- request.
anyPath :: (Monad m) => [ServerPartT m r] -> ServerPartT m r
anyPath x = path $ (\(_::String) -> x)

-- | pops any path element and uses a single ServerPartT to handle the request
anyPath' :: (Monad m) => ServerPartT m r -> ServerPartT m r
anyPath' x = path $ (\(_::String) -> [x])

-- | Retrieve data from the input query or the cookies.
withData :: (FromData a, Monad m) => (a -> [ServerPartT m r]) -> ServerPartT m r
withData = withDataFn fromData

-- | withDataFn is like with data, but you pass in a RqData monad
-- for reading.
withDataFn :: Monad m => RqData a -> (a -> [ServerPartT m r]) -> ServerPartT m r
withDataFn fn handle
    = withRequest $ \rq -> case runReaderT fn (rqInputs rq,rqCookies rq) of
                             Nothing -> noHandle
                             Just a  -> runServerPartT (msum $ handle a) rq

-- | proxyServe is for creating ServerPartT's that proxy.
-- The sole argument [String] is a list of allowed domains for
-- proxying.  This matches the domain part of the request
-- and the wildcard * can be used. E.g.
--
--  * \"*\" to match anything.
--
--  * \"*.example.com\" to match anything under example.com
--
--  * \"example.com\" to match just example.com
proxyServe :: (MonadIO m, WebMonad m) => [String] -> ServerPartT m Response
proxyServe allowed = withRequest $ \rq -> 
                        if cond rq then proxyServe' rq else noHandle 
   where
   cond rq
     | "*" `elem` allowed = True
     | domain `elem` allowed = True
     | superdomain `elem` wildcards =True
     | otherwise = False
     where
     domain = head (rqPaths rq) 
     superdomain = tail $ snd $ break (=='.') domain
     wildcards = (map (drop 2) $ filter ("*." `isPrefixOf`) allowed)

-- | Takes a proxy Request and creates a Response.  Your basic proxy
-- building block.  See 'unproxify'
proxyServe' :: (MonadIO m) => Request-> WebT m Response
proxyServe' rq = liftIO (getResponse (unproxify rq)) >>=
                either (badGateway . toResponse . show) (escape . return)

-- | This is a reverse proxy implementation.
-- see 'unrproxify'
rproxyServe :: (MonadIO m, WebMonad m) =>
    String -- ^ defaultHost
    -> [(String, String)] -- ^ map to look up hostname mappings.  For the reverse proxy
    -> ServerPartT m Response -- ^ the result is a ServerPartT that will reverse proxy for you.
rproxyServe defaultHost list  = withRequest $ \rq ->
                liftIO (getResponse (unrproxify defaultHost list rq)) >>=
                either (badGateway . toResponse . show) (escape . return)

-- | Run an IO action and, if it returns @Just@, pass it to the second argument.
require :: MonadIO m => IO (Maybe a) -> (a -> [ServerPartT m r]) -> ServerPartT m r
require = requireM . liftIO

-- | A varient of require that can run in any monad, not just IO
requireM :: Monad m => m (Maybe a) -> (a -> [ServerPartT m r]) -> ServerPartT m r
requireM fn handle
    = withRequest $ \rq -> do mbVal <- lift fn
                              case mbVal of
                                Nothing -> noHandle
                                Just a  -> runServerPartT (msum $ handle a) rq

-- FIXME: What to do with Escapes?
-- | Use @cmd@ to transform XML against @xslPath@.
--   This function only acts if the content-type is @application\/xml@.
xslt :: (MonadIO m, ToMessage r) =>
        XSLTCmd  -- ^ XSLT preprocessor. Usually 'xsltproc' or 'saxon'.
     -> XSLPath      -- ^ Path to xslt stylesheet.
     -> [ServerPartT m r] -- ^ Affected @ServerParts@.
     -> ServerPartT m Response
xslt cmd xslPath parts =
    withRequest $ \rq -> 
        do res <- runServerPartT (msum parts) rq
           if toContentType res == B.pack "application/xml"
              then liftM toResponse (doXslt cmd xslPath (toResponse res))
              else return (toResponse res)

doXslt :: (MonadIO m) =>
          XSLTCmd -> XSLPath -> Response -> m Response
doXslt cmd xslPath res = 
    do new <- liftIO $ procLBSIO cmd xslPath $ rsBody res
       return $ setHeader "Content-Type" "text/html" $ 
              setHeader "Content-Length" (show $ L.length new) $
              res { rsBody = new }


modifyResponse :: (FilterMonad a m) => (a -> a) -> m()
modifyResponse = composeFilter

setResponseCode :: FilterMonad Response m => Int -> m ()
setResponseCode code
    = modifyResponse $ \r -> r{rsCode = code}

addCookie :: (FilterMonad Response m) => Seconds -> Cookie -> m ()
addCookie sec 
    = modifyResponse . addHeader "Set-Cookie" . mkCookieHeader sec


addCookies :: (FilterMonad Response m, Monad m) => [(Seconds, Cookie)] -> m ()
addCookies = mapM_ (uncurry addCookie)

resp :: (FilterMonad Response m, Monad m) => Int -> b -> m b
resp status val = setResponseCode status >> return val

-- | Respond with @200 OK@.
ok :: (FilterMonad Response m, Monad m) => a -> m a
ok = resp 200

internalServerError :: (FilterMonad Response m, Monad m) => a -> m a
internalServerError = resp 500

badGateway :: (FilterMonad Response m, Monad m) => a -> m a
badGateway = resp 502

-- | Respond with @400 Bad Request@.
badRequest :: (FilterMonad Response m, Monad m) => a -> m a
badRequest = resp 400

-- | Respond with @401 Unauthorized@.
unauthorized :: (FilterMonad Response m, Monad m) => a -> m a
unauthorized = resp 401

-- | Respond with @403 Forbidden@.
forbidden :: (FilterMonad Response m, Monad m) => a -> m a
forbidden = resp 403

-- | Respond with @404 Not Found@.
notFound :: (FilterMonad Response m, Monad m) => a -> m a
notFound = resp 404

-- | Respond with @303 See Other@.
seeOther :: (FilterMonad Response m, Monad m, ToSURI uri) => uri -> res -> m res
seeOther uri res = do modifyResponse $ redirect 303 uri
                      return res

-- | Respond with @302 Found@.
found :: (FilterMonad Response m, Monad m, ToSURI uri) => uri -> res -> m res
found uri res = do modifyResponse $ redirect 302 uri
                   return res

-- | Respond with @301 Moved Permanently@.
movedPermanently :: (FilterMonad Response m, Monad m, ToSURI a) => a -> res -> m res
movedPermanently uri res = do modifyResponse $ redirect 301 uri
                              return res

-- | Respond with @307 Temporary Redirect@.
tempRedirect :: (FilterMonad Response m, Monad m, ToSURI a) => a -> res -> m res
tempRedirect val res = do modifyResponse $ redirect 307 val
                          return res

-- | deprecated.  Just use msum
multi :: Monad m => [ServerPartT m a] -> ServerPartT m a
multi = msum
{-# DEPRECATED multi "Use msum instead" #-}

-- | what is this for, exactly?  I don't understand why @Show a@ is even in the context
-- This appears to do nothing at all.
debugFilter :: (MonadIO m, Show a) => [ServerPartT m a] -> [ServerPartT m a]
debugFilter handle = [
    withRequest $ \rq -> do
                    r <- (runServerPartT (msum handle) rq)
                    return r]

anyRequest :: Monad m => WebT m a -> ServerPartT m a
anyRequest x = withRequest $ \_ -> x

-- | again, why is this useful?
applyRequest :: (ToMessage a, Monad m) =>
                [ServerPartT m a] -> Request -> Either (m Response) b
applyRequest hs = simpleHTTP' hs >>= return . Left

basicAuth :: (WebMonad m, Monad m) => String -> M.Map String String -> [ServerPartT m Response] -> ServerPartT m Response
basicAuth realmName authMap xs = msum $ basicAuthImpl:xs
  where
    basicAuthImpl = withRequest $ \rq ->
      case getHeader "authorization" rq of
        Nothing -> err
        Just x  -> case parseHeader x of 
                     (name, ':':password) | validLogin name password -> noHandle
                     _                                       -> err
    validLogin name password = M.lookup name authMap == Just password
    parseHeader = break (':'==) . Base64.decode . B.unpack . B.drop 6
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""
    err = escape $
          do unauthorized $ addHeader headerName headerValue $ toResponse "Not authorized"


--------------------------------------------------------------
-- Query/Post data validating
--------------------------------------------------------------


lookInput :: String -> RqData Input
lookInput name
    = do inputs <- asks fst
         case lookup name inputs of
           Nothing -> fail "input not found"
           Just i  -> return i

lookBS :: String -> RqData L.ByteString
lookBS = fmap inputValue . lookInput

look :: String -> RqData String
look = fmap L.unpack . lookBS

lookCookie :: String -> RqData Cookie
lookCookie name
    = do cookies <- asks snd
         case lookup (map toLower name) cookies of -- keys are lowercased
           Nothing -> fail "cookie not found"
           Just c  -> return c

lookCookieValue :: String -> RqData String
lookCookieValue = fmap cookieValue . lookCookie

readCookieValue :: Read a => String -> RqData a
readCookieValue name = readM =<< fmap cookieValue (lookCookie name)

lookRead :: Read a => String -> RqData a
lookRead name = readM =<< look name

lookPairs :: RqData [(String,String)]
lookPairs = asks fst >>= return . map (\(n,vbs)->(n,L.unpack $ inputValue vbs))


--------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------

-- | This ServerPart modifier enables the use of throwError and catchError inside the
--   WebT actions, by adding the ErrorT monad transformer to the stack.
--
--   You can wrap the complete second argument to 'simpleHTTP' in this function.
--
--   See 'simpleErrorHandler' for an example error handler.
errorHandlerSP :: (Monad m, Error e) => (Request -> e -> WebT m a) -> [ServerPartT (ErrorT e m) a] -> [ServerPartT m a] 
errorHandlerSP handler sps = [ withRequest $ \req -> mkWebT $ do
			eer <- runErrorT $ ununWebT $ runServerPartT (multi sps) req
			case eer of
				Left err -> ununWebT (handler req err)
				Right res -> return res
		]

-- | An example error Handler to be used with 'errorHandlerSP', which returns the
--   error message as a plain text message to the browser.
--
--   Another possibility is to store the error message, e.g. as a FlashMsg, and
--   then redirect the user somewhere.
simpleErrorHandler :: (Monad m) => Request -> String -> WebT m Response
simpleErrorHandler _ err = ok $ toResponse $ ("An error occured: " ++ err)

--------------------------------------------------------------
-- * Output validation
--------------------------------------------------------------

-- |Set the validator which should be used for this particular 'Response'
-- when validation is enabled.
--
-- Calling this function does not enable validation. That can only be
-- done by enabling the validation in the 'Conf' that is passed to
-- 'simpleHTTP'.
--
-- You do not need to call this function if the validator set in
-- 'Conf' does what you want already.
--
-- Example: (use 'noopValidator' instead of the default supplied by 'validateConf')
--
-- @
--  simpleHTTP validateConf [ anyRequest $ ok . setValidator noopValidator =<< htmlPage ]
-- @
--
-- See also: 'validateConf', 'wdgHTMLValidator', 'noopValidator', 'lazyProcValidator'
setValidator :: (Response -> IO Response) -> Response -> Response
setValidator v r = r { rsValidator = Just v }

-- |ServerPart version of 'setValidator'
--
-- Example: (Set validator to 'noopValidator')
--
-- @
--   simpleHTTP validateConf $ [ setValidatorSP noopValidator (dir "ajax" [ ... ])]
-- @
--
-- See also: 'setValidator'
setValidatorSP :: (ToMessage r) => (Response -> IO Response) -> ServerPartT IO r -> ServerPartT IO Response
setValidatorSP v sp = return . setValidator v . toResponse =<< sp

-- |This extends 'nullConf' by enabling validation and setting
-- 'wdgHTMLValidator' as the default validator for @text\/html@.
--
-- Example:
--
-- @
--  simpleHTTP validateConf [ anyRequest $ ok htmlPage ]
-- @
validateConf :: Conf
validateConf = nullConf { validator = Just wdgHTMLValidator }

-- |Actually perform the validation on a 'Response'
-- 
-- Run the validator specified in the 'Response'. If none is provide
-- use the supplied default instead. 
--
-- Note: This function will run validation unconditionally. You
-- probably want 'setValidator' or 'validateConf'.
runValidator :: (Response -> IO Response) -> Response -> IO Response
runValidator defaultValidator r =
    case rsValidator r of
      Nothing -> defaultValidator r
      (Just altValidator) -> altValidator r

-- |Validate @text\/html@ content with @WDG HTML Validator@.
--
-- This function expects the executable to be named @validate@
-- and it must be in the default @PATH@.
--
-- See also: 'setValidator', 'validateConf', 'lazyProcValidator'
wdgHTMLValidator :: (MonadIO m, ToMessage r) => r -> m Response
wdgHTMLValidator = liftIO . lazyProcValidator "validate" ["-w","--verbose","--charset=utf-8"] Nothing Nothing handledContentTypes . toResponse
    where
      handledContentTypes (Just ct) = elem (takeWhile (\c -> c /= ';' && c /= ' ') (B.unpack ct)) [ "text/html", "application/xhtml+xml" ]
      handledContentTypes Nothing = False

-- |A validator which always succeeds.
--
-- Useful for selectively disabling validation. For example, if you
-- are sending down HTML fragments to an AJAX application and the
-- default validator only understands complete documents.
noopValidator :: Response -> IO Response
noopValidator = return

-- |Validate the 'Response' using an external application.
-- 
-- If the external application returns 0, the original response is
-- returned unmodified. If the external application returns non-zero, a 'Response'
-- containing the error messages and original response body is
-- returned instead.
--
-- This function also takes a predicate filter which is applied to the
-- content-type of the response. The filter will only be applied if
-- the predicate returns true.
--
-- NOTE: This function requirse the use of -threaded to avoid blocking.
-- However, you probably need that for Happstack anyway.
-- 
-- See also: 'wdgHTMLValidator'
lazyProcValidator :: FilePath -- ^ name of executable
               -> [String] -- ^ arguements to pass to the executable
               -> Maybe FilePath -- ^ optional path to working directory
               -> Maybe [(String, String)] -- ^ optional environment (otherwise inherit)
               -> (Maybe B.ByteString -> Bool) -- ^ content-type filter
               -> Response -- ^ Response to validate
               -> IO Response
lazyProcValidator exec args wd env mimeTypePred response
    | mimeTypePred (getHeader "content-type" response) =
        do (inh, outh, errh, ph) <- runInteractiveProcess exec args wd env
           out <- hGetContents outh
           err <- hGetContents errh
           forkIO $ do L.hPut inh (rsBody response)
                       hClose inh
           forkIO $ evaluate (length out) >> return ()
           forkIO $ evaluate (length err) >> return ()
           ec <- waitForProcess ph
           case ec of
             ExitSuccess     -> return response
             (ExitFailure _) -> 
                 return $ toResponse (unlines ([ "ExitCode: " ++ show ec
                                               , "stdout:"
                                               , out
                                               , "stderr:"
                                               , err
                                               , "input:"
                                               ] ++ 
                                               showLines (rsBody response)))
    | otherwise = return response
    where
      column = "  " ++ (take 120 $ concatMap  (\n -> "         " ++ show n) (drop 1 $ cycle [0..9::Int]))
      showLines :: L.ByteString -> [String]
      showLines string = column : zipWith (\n -> \l  -> show n ++ " " ++ (L.unpack l)) [1::Integer ..] (L.lines string)
