{-# LANGUAGE FlexibleInstances, PatternGuards, ScopedTypeVariables, TypeSynonymInstances #-}
-- | Incoming 'Request' routing for mapping requests to handlers. <http://happstack.com/docs/crashcourse/RouteFilters.html>
module Happstack.Server.Routing where

import           Control.Monad                   (MonadPlus(mzero,mplus), unless)
import qualified Data.ByteString.Char8           as B
import           Happstack.Server.Monads         (ServerPartT, ServerMonad(..))
import           Happstack.Server.Scary          (WebT, anyRequest)
import           Happstack.Server.HTTP.Types     (Request(..), Method(..), getHeader, rqURL)
import           Happstack.Util.Common           (readM)
import           System.FilePath                 (makeRelative, splitDirectories)

class MatchMethod m where matchMethod :: m -> Method -> Bool
instance MatchMethod Method where matchMethod m = (== m)
instance MatchMethod [Method] where matchMethod methods = (`elem` methods)
instance MatchMethod (Method -> Bool) where matchMethod f = f
instance MatchMethod () where matchMethod () _ = True

-- | This class is used by 'path' to parse a path component into a
-- value.  At present, the instances for number types ('Int', 'Float',
-- etc) just call 'readM'. The instance for 'String' however, just
-- passes the path component straight through. This is so that you can
-- read a path component which looks like this as a 'String'
-- \/somestring\/ instead of requiring the path component to look like
-- \/\"somestring\"\/.
class FromReqURI a where
    fromReqURI :: String -> Maybe a

instance FromReqURI String  where fromReqURI = Just
instance FromReqURI Int     where fromReqURI = readM
instance FromReqURI Integer where fromReqURI = readM
instance FromReqURI Float   where fromReqURI = readM
instance FromReqURI Double  where fromReqURI = readM


-------------------------------------
-- guards

-- | Guard using an arbitrary function on the 'Request'.
guardRq :: (ServerMonad m, MonadPlus m) => (Request -> Bool) -> m ()
guardRq f = do
    rq <- askRq
    unless (f rq) mzero

-- | Guard against the method. This function also guards against
-- any remaining path segments. See 'methodOnly' for the version
-- that guards only by method.
methodM :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
methodM meth = methodOnly meth >> nullDir

-- | Guard against the method only (as opposed to 'methodM').
methodOnly :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
methodOnly meth = guardRq $ \rq -> matchMethod meth (rqMethod rq)

-- | Guard against the method. Note, this function also guards against
-- any remaining path segments.
methodSP :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m b-> m b
methodSP m handle = methodM m >> handle

-- | Guard against the method. Note, this function also guards against any
-- remaining path segments.  This function is deprecated.  You can probably
-- just use 'methodSP' (or 'methodM') now.
method :: (MatchMethod method, Monad m) => method -> WebT m a -> ServerPartT m a
method m handle = methodSP m (anyRequest handle)
{-# DEPRECATED method "you should be able to use methodSP" #-}

-- | Guard against non-empty remaining path segments.
nullDir :: (ServerMonad m, MonadPlus m) => m ()
nullDir = guardRq $ \rq -> null (rqPaths rq)

-- | Pop a path element and run the 'ServerPartT' if it matches the
-- given string.
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
host :: (ServerMonad m, MonadPlus m) => String -> m a -> m a
host desiredHost handle =
    do rq <- askRq
       case getHeader "host" rq of
         (Just hostBS) | desiredHost == B.unpack hostBS -> handle
         _ -> mzero

-- | Lookup the host header and pass it to the handler.
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

-- | Pop any path element and ignore when choosing a 'ServerPartT' to
-- handle the request.
anyPath :: (ServerMonad m, MonadPlus m) => m r -> m r
anyPath x = path $ (\(_::String) -> x)

-- | Deprecated: use 'anyPath'.
anyPath' :: (ServerMonad m, MonadPlus m) => m r -> m r
anyPath' = anyPath
{-# DEPRECATED anyPath' "Use anyPath" #-}

-- | Guard which checks that the Request URI ends in @\'\/\'@.  Useful
-- for distinguishing between @foo@ and @foo/@
trailingSlash :: (ServerMonad m, MonadPlus m) => m ()
trailingSlash = guardRq $ \rq -> (last (rqUri rq)) == '/'
