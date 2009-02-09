{-# LANGUAGE UndecidableInstances, OverlappingInstances, ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances,
    MultiParamTypeClasses, PatternGuards, PatternSignatures #-}

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
--   main = simpleHTTP nullConf {port = 5000} [anyRequest $ ok $ toResponse \"Hello World!\" ]
-- @
--
-- @simpleHTTP nullConf {port =5000}@ creates a HTTP server on port 5000.
-- @anyRequest@ simply ignores your request (so the particular URL or method
-- you use doesn't matter).  @ok@ sets the response code to "200" (i.e. HTTP
-- OK).  Finally toResponse converts it's argument to a 'Response' (which among
-- other things tells the server the mime type you should be returning).
--
-- 'ServerPartT' is the basic response builder.  As you might expect, it's a
-- container for a function that takes a Request and converts it a response
-- suitable for sending back to the server.  The simplist 'ServerPartT' ignores
-- the request and simply provides a response.  Not coincidentally, this is called
-- 'anyRequest' and we used it in our "hello word" example above.  Its definition
-- is quite simple:
--
-- @
--  anyRequest aResponse = ServerPartT \\x -> aResponse
-- @
-- 
-- 'ServerPartT' is also avalible in monadic form.  Here is a do block
-- that validates basic authentication
-- It takes a realm name as a string, a Map of username to password
-- and a server part to run if authentication fails.
--
-- 
-- @basicAuth'@ acts like a guard, and only produces a response when
-- authentication fails.  So put it before any ServerPartTs you want
-- to demand authentication for in any list of ServerPartTs.
--
-- @
--  escapeSP a = anyRequest $ escape' a
--  nohandleSP = anyRequest $ noHandle
--  getHeaderSP a = ServerPartT $ \rq -> return $ getHeader a rq
--  basicAuth' realmName authMap unauthorizedPart = do
--      authHeader <- getHeaderSP \"authorization\"
--      case authHeader of
--         Nothing -> err
--         Just x  -> case parseHeader x of 
--                      (name, ':':pass) | validLogin name pass -> noHandleSP
--                      _                                       -> err
--      validLogin name pass = M.lookup name authMap == Just pass
--      parseHeader = break (':'==) . Base64.decode . B.unpack . B.drop 6
--      headerName  = \"WWW-Authenticate\"
--      headerValue = \"Basic realm=\\\"\" ++ realmName ++ \"\\\"\"
--      err = unauthorizedPart >>= escapeSP
-- @
-- 
-- -- TODO move escapeSP noHandleSP and getHeaderSP into the module proper
--
-- The response object that a 'ServerPartT' expects is a 'WebT' which is
-- container (i.e. Monad) for responses.  Let's start with a few examples.
--
-- @
--   main = simpleHTTP nullConf {port = 5000} [ anyRequest $ myWebT ]
--   myWebT = do
--     setResponseCode 200
--     return $ toResponse \"Hello World!\"
-- @
--
-- This is exactly the same as our first example, though a little more verbose.
-- Why the extra verbosity?  The monadic form gives us access to two control
-- structures, noHandle and escape'.  noHandle causes the do block to 
-- exit immediately and fail to be chosen to handle the request while escape'
-- stops all processing and produces the result you have generated so far.  Here
-- is an example of "noHandle"
--
-- @
--   main = simpleHTTP nullConf {port = 5000} [ anyRequest $ myWebT ]
--   myWebT = do
--     line <- liftIO $ do -- IO
--         putStr \"return? \"
--         getLine
--     when (take 2 line /= \"ok\") $ noHandle
--     setResponseCode 200
--     return $ toResponse \"Hello World!\"
-- @
-- 
-- This example will ask in the console \"return? \" if you type \"ok\" it will
-- show \"Hello World!\" and if you don\'t it will return a 404.
--
-- TODO -- while the monad forms of ServerPartT and WebT are quite handy
-- we're missing virtually all the functions that could be useful in a monad.
-- I.e. getHeaderSP, setHeaderW, escapeSP and noHandleSP referenced above don't
-- actually exist in the module
-- 
-----------------------------------------------------------------------------
module Happstack.Server.SimpleHTTP
    ( module Happstack.Server.HTTP.Types
    , module Happstack.Server.Cookie
    , -- * SimpleHTTP
      simpleHTTP -- , simpleHTTP'
    , parseConfig
    , FromReqURI(..)
    , RqData
    , FromData(..)
    , ToMessage(..)
    , ServerPart
    , ServerPartT(..)
    , Web
    , WebT(..)
    , Result(..)
    , executeSP
    , executeW


      -- * Processing requests
    , webQuery
    , webUpdate
    , flatten
    , localContext
    , dir         -- :: String -> [ServerPart] -> ServerPart
    , method      -- :: MatchMethod m => m -> IO Result -> ServerPart
    , methodSP
--    , method'     -- :: MatchMethod m => m -> IO (Maybe Result) -> ServerPart
    , path        -- :: FromReqURI a => (a -> [ServerPart]) -> ServerPart
    , proxyServe
    , rproxyServe
--    , limProxyServe
    , uriRest 
    , anyPath
    , anyPath'
    , withData    -- :: FromData a => (a -> [ServerPart]) -> ServerPart
    , withDataFn
--    , modXml
    , require     -- :: IO (Maybe a) -> (a -> [ServerPart]) -> ServerPart
    , multi       -- :: [ServerPart] -> ServerPart
    , withRequest -- :: (Request -> IO Result) -> ServerPart
    , debugFilter
    , anyRequest
    , applyRequest
    , modifyResponse
    , setResponseCode
    , basicAuth
      -- * Creating Results.
    , noHandle
    , finishWith
    , escape
    , setResponseFilter
    , ignoreResponseFilter
    , escape'
    , ok          -- :: ToMessage a => a -> IO Result
--    , mbOk
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
import Happstack.Server.HTTP.Listen
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

-- | ServerPartT is a container for methods that process Request  and return a WebT
-- The basic request-response object.
newtype ServerPartT m a = ServerPartT { unServerPartT :: Request -> WebT m a }

instance (Monad m) => Monad (ServerPartT m) where
    f >>= g = ServerPartT $ \rq ->
              do a <- unServerPartT f rq
                 unServerPartT (g a) rq
    return x = ServerPartT $ \_ -> return x

instance (MonadIO m) => MonadIO (ServerPartT m) where
    liftIO = ServerPartT . const . liftIO

newtype WebT m a = WebT { unWebT :: m (Result a) }

-- | Result is part of the implementation of the WebT do semantics
data Result a = NoHandle -- ^ NoHandle is essentially "fail"  It aborts the Monad computation.
                         --  It's a lot like Nothing from Maybe that way.
              | Ok (Response -> Response) a -- ^ Ok contains a filter and a result
                                            -- (>>=) ensures that filters in Ok are always composed
              | Escape Response -- ^ causes the computation in the do block to immediate exit
                                -- It's used to implement finishWith.  It's called "Escape"
                                -- and not "Finish" for historical reasons.
              | SetFilter (Response -> Response) a -- ^ SetFilter is a lot like Ok, except
                                                   -- it's filter doesn't compose with previous filters
                deriving Show

instance Functor Result where
    fmap _ NoHandle = NoHandle
    fmap fn (Ok out a) = Ok out (fn a)
    fmap fn (SetFilter out a) = SetFilter out (fn a)
    fmap _ (Escape r) = Escape r

instance Monad m => Monad (WebT m) where
    f >>= g = WebT $ do r <- unWebT f
                        case r of
                          NoHandle    -> return NoHandle
                          Escape res -> return $ Escape res
                          Ok out a    -> do r' <- unWebT (g a)
                                            case r' of
                                              NoHandle    -> return NoHandle
                                              Escape res -> return $ Escape $ out res
                                              Ok out' a'  -> return $ Ok (out' . out) a'
                                              SetFilter out' a' -> return $ SetFilter out' a'
                          SetFilter out a -> do r' <- unWebT (g a)
                                                case r' of
                                                  NoHandle -> return NoHandle
                                                  Escape res -> return $ Escape $ out res
                                                  Ok out' a' -> return $ SetFilter (out' . out) a'
                                                  SetFilter out' a' -> return $ SetFilter out' a'
    return x = WebT $ return (Ok id x)

instance (Monad m) => MonadPlus (ServerPartT m)
 where mzero = ServerPartT $ \_ -> noHandle
       mplus a b = ServerPartT $ \rq -> (unServerPartT a rq)
                     `mplus` (unServerPartT b rq)

instance (Monad m) => MonadPlus (WebT m) where
 mzero = noHandle
 mplus a b = WebT $ do a' <- unWebT a
                       case a' of
                         NoHandle -> unWebT b
                         _        -> return a'

instance (Monad m) => Monoid (ServerPartT m a)
 where mempty = ServerPartT $ \_ -> noHandle
       mappend a b = ServerPartT $ \rq -> (unServerPartT a rq)
                     `mappend` (unServerPartT b rq)

instance (Monad m) => Monoid (WebT m a) where
 mempty = noHandle
 mappend a b = WebT $ do a' <- unWebT a
                         case a' of
                             NoHandle -> unWebT b
                             _        -> return a'

instance MonadTrans WebT where
    lift = WebT . liftM (Ok id)

instance MonadIO m => MonadIO (WebT m) where
    liftIO = WebT . liftM (Ok id) . liftIO

instance Functor m => Functor (WebT m) where
    fmap fn (WebT m) = WebT $ fmap (fmap fn) m

instance Functor m => Functor (ServerPartT m) where
    fmap fn (ServerPartT m) = ServerPartT $ fmap (fmap fn) m

instance (Monad m, Functor m) => Applicative (ServerPartT m) where
    pure = return
    (<*>) = ap

instance (Monad m, Functor m) => Applicative (WebT m) where
    pure = return
    (<*>) = ap

instance MonadReader r m => MonadReader r (WebT m) where
    ask = lift ask
    local fn = WebT . local fn . unWebT

instance MonadState st m => MonadState st (WebT m) where
    get = lift get
    put = lift . put

instance MonadError e m => MonadError e (WebT m) where
	throwError = WebT . throwError 
 	catchError action handler = WebT $ catchError (unWebT action) (unWebT . handler)


-- | noHandle is a control structure for use in WebT monads.
-- It aborts a computation.
--
-- This is primarily useful because msum will take an array
-- of WebT or ServerPartT and return the first one that
-- isn't NoHandle, which is what simpleHTTP will do for you.
noHandle :: Monad m => WebT m a
noHandle = WebT $ return NoHandle

-- | finishWith is a contorl structure for use in WebT monads.
-- It ends the computation and returns the result you passed into it
-- immediately.
finishWith :: (Monad m, ToMessage a1) => a1 -> WebT m a
finishWith = WebT . return . Escape . toResponse

-- | Inside the WebT monad, if you want to ignore all previous
-- alterations to your Response (primarily modifyResponse
-- or previous calls of setResponseFilter) you can use this.
--
-- As an example:
--
-- @
--   do
--     modifyResponse f
--     setResponseFilter id
--     return "Hello World"
-- @
--
-- setResponseFilter id will cause the first modifyResponse to be
-- ignored.  This is particuarly useful for resetting your headers
setResponseFilter:: Monad m => (Response->Response) -> WebT m ()
setResponseFilter f = WebT $ return $ SetFilter f ()

-- | this is an alias for setResponseFilter id
-- It directly sets (as opposed to composing)
-- your response filter.  In particular,
-- it resets all your headers.
ignoreResponseFilter :: Monad m => WebT m ()
ignoreResponseFilter = setResponseFilter id

-- | escape is used to create a WebT that resets headers
-- and immediately ends the computation.  A combination of
-- 'ignoreResponseFilter' and 'finishWith'
escape :: (Monad m, ToMessage resp) => WebT m resp -> WebT m a
escape gen = ignoreResponseFilter >> gen >>= finishWith

-- | escape' is an alternate form of escape that can
-- be easily used within a WebT do block.
escape' :: (Monad m, ToMessage a1) => a1 -> WebT m a
escape' a = ignoreResponseFilter >> finishWith a

-- | ho is an array of OptDescr, useful for processing
-- command line options into an Conf for simpleHTTP
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
simpleHTTP :: ToMessage a => Conf -> [ServerPartT IO a] -> IO ()
simpleHTTP conf hs
    = listen conf (\req -> runValidator (fromMaybe return (validator conf)) =<< simpleHTTP' hs req)


-- | Generate a result from a list of 'ServerParts' and a 'Request'. This is mainly used
-- by CGI (and fast-cgi) wrappers.
simpleHTTP' :: (ToMessage a, Monad m) => [ServerPartT m a] -> Request -> m Response
simpleHTTP' hs req =  executeW standardNotFound $ executeSP (msum hs) req
    where
        standardNotFound = return $ result 404 "No suitable handler found"

executeSP :: ServerPartT m a -> Request -> WebT m a
executeSP = unServerPartT

executeW :: (Monad m, ToMessage msg) => m Response -> WebT m msg -> m Response
executeW def web = do r <-unWebT $  web >>= finishWith
                      case r of
                        (Escape r') -> return r'
                        -- Note that because we called "finishWith" Ok isn't possible here
                        -- this can only be NoHandle
                        _ -> def 
                  
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

webQuery :: (MonadIO m, QueryEvent ev res) => ev -> WebT m res
webQuery = liftIO . query

webUpdate :: (MonadIO m, UpdateEvent ev res) => ev -> WebT m res
webUpdate = liftIO . update

-- | flatten turns your arbitrary ServerPartT m a and converts it too
-- a 'Response' with 'toResponse'
flatten :: (ToMessage a, Monad m) => ServerPartT m a -> ServerPartT m Response
flatten = liftM toResponse

-- | TODO
-- This is for applying a WebT function to the msum of the array argument, then
-- stuffing it back into a ServerPartT.  Why would you need this?
localContext :: Monad m => (WebT m a -> WebT m' a) -> [ServerPartT m a] -> ServerPartT m' a
localContext fn hs
    = ServerPartT $ \rq -> fn (unServerPartT (multi hs) rq)


-- | Pop a path element and run the @[ServerPartT]@ if it matches the given string.
dir :: Monad m => String -> [ServerPartT m a] -> ServerPartT m a
dir staticPath handle
    = ServerPartT $ \rq -> case rqPaths rq of
                             (p:xs) | p == staticPath -> 
                                           unServerPartT (multi handle) rq{rqPaths = xs}
                             _ -> noHandle


-- | Guard against the method. Note, this function also guards against any
--   remaining path segments. See 'anyRequest'.
methodSP :: (MatchMethod method, Monad m) => method -> ServerPartT m a -> ServerPartT m a
methodSP m handle
    = ServerPartT $ \rq -> if matchMethod m (rqMethod rq) && null (rqPaths rq)
                           then unServerPartT handle rq
                           else noHandle

-- | Guard against the method. Note, this function also guards against any
--   remaining path segments. See 'anyRequest'.
method :: (MatchMethod method, Monad m) => method -> WebT m a -> ServerPartT m a
method m handle = methodSP m (ServerPartT $ \_ -> handle)


-- | Pop a path element and parse it.
path :: (FromReqURI a, Monad m) => (a -> [ServerPartT m r]) -> ServerPartT m r
path handle
    = ServerPartT $ \rq -> 
      case rqPaths rq of
               (p:xs) | Just a <- fromReqURI p
                                  -> unServerPartT (multi $ handle a) rq{rqPaths = xs}
               _ -> noHandle

-- | grabs the rest of the URL (dirs + query) and passes it to your handler
uriRest :: Monad m => (String -> ServerPartT m a) -> ServerPartT m a
uriRest handle = withRequest $ \rq ->
                  unServerPartT (handle (rqURL rq)) rq

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
    = ServerPartT $ \rq -> case runReaderT fn (rqInputs rq,rqCookies rq) of
                             Nothing -> noHandle
                             Just a  -> unServerPartT (multi $ handle a) rq

-- | proxyServe is for creating ServerPartT's that proxy.
-- The sole argument [String] is a list of allowed domains for
-- proxying.  This matches the domain part of the request
-- and the wildcard * can be used. E.g.
--
--  - \"*\" to match anything.
--  - \"*.example.com\" to match anything under example.com
--  - \"example.com\" to match just example.com
proxyServe :: MonadIO m => [String] -> ServerPartT m Response
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
proxyServe' :: (MonadIO m) => Request -> WebT m Response
proxyServe' rq = liftIO (getResponse (unproxify rq)) >>=
                either (badGateway . toResponse . show) (escape . return)

-- | This is a reverse proxy implementation.
-- see 'unrproxify'
rproxyServe :: MonadIO m =>
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
    = ServerPartT $ \rq -> do mbVal <- lift fn
                              case mbVal of
                                Nothing -> noHandle
                                Just a  -> unServerPartT (multi $ handle a) rq

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
        do res <- unServerPartT (multi parts) rq
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



--io :: IO Result -> ServerPart
--io action = ReaderT $ \_ -> Just action


modifyResponse :: Monad m => (Response -> Response) -> WebT m ()
modifyResponse modFn = WebT $ return $ Ok modFn ()

setResponseCode :: Monad m => Int -> WebT m ()
setResponseCode code
    = modifyResponse $ \r -> r{rsCode = code}

addCookie :: Monad m => Seconds -> Cookie -> WebT m ()
addCookie sec 
    = modifyResponse . addHeader "Set-Cookie" . mkCookieHeader sec

addCookies :: Monad m => [(Seconds, Cookie)] -> WebT m ()
addCookies = mapM_ (uncurry addCookie)

resp :: (Monad m) => Int -> b -> WebT m b
resp status val = setResponseCode status >> return val

-- | Respond with @200 OK@.
ok :: Monad m => a -> WebT m a
ok = resp 200

internalServerError::Monad m => a -> WebT m a
internalServerError = resp 500

badGateway::Monad m=> a-> WebT m a
badGateway = resp 502

-- | Respond with @400 Bad Request@.
badRequest :: Monad m => a -> WebT m a
badRequest = resp 400

-- | Respond with @401 Unauthorized@.
unauthorized :: Monad m => a -> WebT m a
unauthorized = resp 401

-- | Respond with @403 Forbidden@.
forbidden :: Monad m => a -> WebT m a
forbidden = resp 403

-- | Respond with @404 Not Found@.
notFound :: Monad m => a -> WebT m a
notFound = resp 404

-- | Respond with @303 See Other@.
seeOther :: (Monad m, ToSURI uri) => uri -> res -> WebT m res
seeOther uri res = do modifyResponse $ redirect 303 uri
                      return res

-- | Respond with @302 Found@.
found :: (Monad m, ToSURI uri) => uri -> res -> WebT m res
found uri res = do modifyResponse $ redirect 302 uri
                   return res

-- | Respond with @301 Moved Permanently@.
movedPermanently :: (Monad m, ToSURI a) => a -> res -> WebT m res
movedPermanently uri res = do modifyResponse $ redirect 301 uri
                              return res

-- | Respond with @307 Temporary Redirect@.
tempRedirect :: (Monad m, ToSURI a) => a -> res -> WebT m res
tempRedirect val res = do modifyResponse $ redirect 307 val
                          return res


multi :: Monad m => [ServerPartT m a] -> ServerPartT m a
multi = msum

withRequest :: (Request -> WebT m a) -> ServerPartT m a
withRequest = ServerPartT

debugFilter :: (MonadIO m, Show a) => [ServerPartT m a] -> [ServerPartT m a]
debugFilter handle = [
    ServerPartT $ \rq -> WebT $ do
                    r <- unWebT (unServerPartT (multi handle) rq)
                    return r]

anyRequest :: Monad m => WebT m a -> ServerPartT m a
anyRequest x = withRequest $ \_ -> x

applyRequest :: (ToMessage a, Monad m) =>
                [ServerPartT m a] -> Request -> Either (m Response) b
applyRequest hs = simpleHTTP' hs >>= return . Left

basicAuth :: (MonadIO m) => String -> M.Map String String -> [ServerPartT m a] -> ServerPartT m a
basicAuth realmName authMap xs = multi $ basicAuthImpl:xs
  where
    basicAuthImpl = withRequest $ \rq ->
      case getHeader "authorization" rq of
        Nothing -> err
        Just x  -> case parseHeader x of 
                     (name, ':':pass) | validLogin name pass -> noHandle
                     _                                       -> err
    validLogin name pass = M.lookup name authMap == Just pass
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
errorHandlerSP handler sps = [ ServerPartT $ \req -> WebT $ do
			eer <- runErrorT $ unWebT $ unServerPartT (multi sps) req
			case eer of
				Left err -> unWebT (handler req err)
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
