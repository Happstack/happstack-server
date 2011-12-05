{-# LANGUAGE FlexibleInstances, PatternGuards, ScopedTypeVariables, TypeSynonymInstances #-}
-- | Route an incoming 'Request' to a handler. For more in-depth documentation see this section of the Happstack Crash Course: <http://happstack.com/docs/crashcourse/RouteFilters.html>
module Happstack.Server.Routing 
    ( -- * Route by request method
      methodM
    , methodOnly
    , methodSP
    , method
    , MatchMethod(..)
      -- * Route by pathInfo
    , dir
    , dirs
    , nullDir
    , trailingSlash
    , anyPath
    , path
    , uriRest
    -- * Route by host
    , host
    , withHost
      -- * Route by (Request -> Bool)
    , guardRq
    ) where

import           Control.Monad                    (MonadPlus(mzero,mplus), unless)
import qualified Data.ByteString.Char8            as B
import           Happstack.Server.Monads          (ServerPartT, ServerMonad(..))
import           Happstack.Server.Internal.Monads (WebT, anyRequest)
import           Happstack.Server.Types           (Request(..), Method(..), FromReqURI(..), getHeader, rqURL)
import           System.FilePath                  (makeRelative, splitDirectories)

-- | instances of this class provide a variety of ways to match on the 'Request' method.
--
-- Examples
-- 
-- > method GET                  -- match GET
-- > method [HEAD, GET]          -- match HEAD or GET
-- > method (not . (==) DELETE)  -- match any method except DELETE
-- > method ()                   -- match any method
class MatchMethod m where matchMethod :: m -> Method -> Bool
instance MatchMethod Method where matchMethod m = (== m)
instance MatchMethod [Method] where matchMethod methods = (`elem` methods)
instance MatchMethod (Method -> Bool) where matchMethod f = f
instance MatchMethod () where matchMethod () _ = True

-------------------------------------
-- guards

-- | Guard using an arbitrary function on the 'Request'.
guardRq :: (ServerMonad m, MonadPlus m) => (Request -> Bool) -> m ()
guardRq f = do
    rq <- askRq
    unless (f rq) mzero

-- | Guard against the method only (as opposed to 'methodM').
--
-- Example:
--
-- > handler :: ServerPart Response
-- > handler =
-- >     do methodOnly [GET, HEAD]
-- >        ...
method :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
method meth = guardRq $ \rq -> matchMethod meth (rqMethod rq)


-- | Guard against the method. This function also guards against
-- *any remaining path segments*. See 'method' for the version
-- that guards only by method.
--
-- Example:
--
-- > handler :: ServerPart Response
-- > handler =
-- >     do methodM [GET, HEAD]
-- >        ...
-- 
-- NOTE: This function is largely retained for backwards
-- compatibility. The fact that implicitly calls 'nullDir' is often
-- forgotten and leads to confusion. It is probably better to just use
-- 'method' and call 'nullDir' explicitly.
-- 
-- This function will likely be deprecated in the future.
methodM :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
methodM meth = methodOnly meth >> nullDir

-- | Guard against the method only (as opposed to 'methodM').
--
-- Example:
--
-- > handler :: ServerPart Response
-- > handler =
-- >     do methodOnly [GET, HEAD]
-- >        ...
methodOnly :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
methodOnly = method
{-# DEPRECATED methodOnly "this function is just an alias for method now" #-}

-- | Guard against the method. Note, this function also guards against
-- any remaining path segments. Similar to 'methodM' but with a different type signature.
--
-- Example:
--
-- > handler :: ServerPart Response
-- > handler = methodSP [GET, HEAD] $ subHandler
-- 
-- NOTE: This style of combinator is going to be deprecated in the
-- future. It is better to just use 'method'.
methodSP :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m b-> m b
methodSP m handle = methodM m >> handle

-- | guard which only succeeds if there are no remaining path segments
--
-- Often used if you want to explicitly assign a route for '/'
-- 
nullDir :: (ServerMonad m, MonadPlus m) => m ()
nullDir = guardRq $ \rq -> null (rqPaths rq)

-- | Pop a path element and run the supplied handler if it matches the
-- given string.
-- 
-- > handler :: ServerPart Response
-- > handler = dir "foo" $ dir "bar" $ subHandler
-- 
-- The path element can not contain \'/\'. See also 'dirs'.
dir :: (ServerMonad m, MonadPlus m) => String -> m a -> m a
dir staticPath handle =
    do
        rq <- askRq
        case rqPaths rq of
            (p:xs) | p == staticPath -> localRq (\newRq -> newRq{rqPaths = xs}) handle
            _ -> mzero
            
-- | Guard against a 'FilePath'. Unlike 'dir' the 'FilePath' may
-- contain \'/\'. If the guard succeeds, the matched elements will be
-- popped from the directory stack.
--
-- > dirs "foo/bar" $ ...
--          
-- See also: 'dir'.
dirs :: (ServerMonad m, MonadPlus m) => FilePath -> m a -> m a 
dirs fp m = 
     do let parts = splitDirectories (makeRelative "/" fp) 
        foldr dir m parts

-- | Guard against the host.
--
-- This matches against the @host@ header specified in the incoming 'Request'.
--
-- Can be used to support virtual hosting, <http://en.wikipedia.org/wiki/Virtual_hosting>
-- 
-- see also: 'withHost'
host :: (ServerMonad m, MonadPlus m) => String -> m a -> m a
host desiredHost handle =
    do rq <- askRq
       case getHeader "host" rq of
         (Just hostBS) | desiredHost == B.unpack hostBS -> handle
         _ -> mzero

-- | Lookup the @host@ header in the incoming request and pass it to the handler.
--
-- see also: 'host'
withHost :: (ServerMonad m, MonadPlus m) => (String -> m a) -> m a
withHost handle =
    do rq <- askRq
       case getHeader "host" rq of
         (Just hostBS) -> handle (B.unpack hostBS)
         _ -> mzero


-- | Pop a path element and parse it using the 'fromReqURI' in the
-- 'FromReqURI' class.
path :: (FromReqURI a, MonadPlus m, ServerMonad m) => (a -> m b) -> m b
path handle = do
    rq <- askRq
    case rqPaths rq of
        (p:xs) | Just a <- fromReqURI p
                            -> localRq (\newRq -> newRq{rqPaths = xs}) (handle a)
        _ -> mzero

-- | Grab the rest of the URL (dirs + query) and passes it to your
-- handler.
uriRest :: (ServerMonad m) => (String -> m a) -> m a
uriRest handle = askRq >>= handle . rqURL

-- | Pop any path element and run the handler.
-- 
-- Succeeds if a path component was popped. Fails is the remaining path was empty.
anyPath :: (ServerMonad m, MonadPlus m) => m r -> m r
anyPath x = path $ (\(_::String) -> x)

-- | Guard which checks that the Request URI ends in @\'\/\'@.  Useful
-- for distinguishing between @foo@ and @foo/@
trailingSlash :: (ServerMonad m, MonadPlus m) => m ()
trailingSlash = guardRq $ \rq -> (last (rqUri rq)) == '/'
