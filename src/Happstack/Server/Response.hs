{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, ScopedTypeVariables #-}
-- | Functions and classes related to generating a 'Response' and setting the response code. For detailed instruction see the Happstack Crash Course: <http://happstack.com/docs/crashcourse/HelloWorld.html#response_code>
module Happstack.Server.Response 
    ( -- * Converting values to a 'Response'
      ToMessage(..)
    , flatten
    , toResponseBS
      -- * Setting the Response Code
    , setResponseCode
    , resp
    , ok
    , noContent
    , internalServerError
    , badGateway
    , badRequest
    , unauthorized
    , forbidden
    , notFound
    , seeOther
    , found
    , movedPermanently
    , tempRedirect
    -- * Handling if-modified-since
    , ifModifiedSince
    ) where

import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as L
import qualified Data.ByteString.Lazy.UTF8       as LU (fromString)
import qualified Data.Map                        as M
import           Happstack.Server.Monads         (FilterMonad(composeFilter))
import           Happstack.Server.Types          (Response(..), Request(..), nullRsFlags, getHeader, noContentLength, redirect, result, setHeader, setHeaderBS)
import           Happstack.Server.SURI           (ToSURI)
import           System.Locale                   (defaultTimeLocale)
import           System.Time                     (CalendarTime, formatCalendarTime)
import qualified Text.Blaze                      as Blaze
import qualified Text.Blaze.Renderer.Utf8        as Blaze
import           Text.Html                       (Html, renderHtml)
import qualified Text.XHtml                      as XHtml (Html, renderHtml)

-- | A low-level function to build a 'Response' from a content-type
-- and a 'ByteString'.
--
-- Creates a 'Response' in a manner similar to the 'ToMessage' class,
-- but without requiring an instance declaration.
toResponseBS :: B.ByteString -- ^ content-type
             -> L.ByteString -- ^ response body
             -> Response
toResponseBS contentType message =
    let res = Response 200 M.empty nullRsFlags message Nothing
    in setHeaderBS (B.pack "Content-Type") contentType res


-- | Used to convert a value into an HTTP 'Response'
--
-- Calling 'toResponse' will convert a value into a 'Response' body,
-- set the @content-type@, and set a default @response code@.
--
-- Example:
--
-- > main = simpleHTTP nullConf $ toResponse "hello, world!"
--
-- will generate a 'Response' with the content-type @text/plain@,
-- the response code @200 OK@, and the body: @hello, world!@.
--
-- 'simpleHTTP' will call 'toResponse' automatically, so the above can be shortened to:
--
--  > main = simpleHTTP nullConf $ "hello, world!"
--
-- Minimal definition: 'toMessage' (and usually 'toContentType'). 
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
{-
instance ToMessage [Element] where
    toContentType _ = B.pack "application/xml; charset=UTF-8"
    toMessage [el] = LU.fromString $ H.simpleDoc H.NoStyle $ toHaXmlEl el -- !! OPTIMIZE
    toMessage x    = error ("Happstack.Server.SimpleHTTP 'instance ToMessage [Element]' Can't handle " ++ show x)
-}

instance ToMessage () where
    toContentType _ = B.pack "text/plain"
    toMessage () = L.empty

instance ToMessage String where
    toContentType _ = B.pack "text/plain; charset=UTF-8"
    toMessage = LU.fromString

instance ToMessage Integer where
    toMessage = toMessage . show

instance ToMessage a => ToMessage (Maybe a) where
    toContentType _ = toContentType (undefined :: a)
    toMessage Nothing = toMessage "nothing"
    toMessage (Just x) = toMessage x


instance ToMessage Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage = LU.fromString . renderHtml

instance ToMessage XHtml.Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage = LU.fromString . XHtml.renderHtml

instance ToMessage Blaze.Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage       = Blaze.renderHtml

instance ToMessage Response where
    toResponse = id
{-

-- This instances causes awful error messages. I am removing it and
-- seeing if anyone complains. I doubt they will.

instance (Xml a)=>ToMessage a where
    toContentType = toContentType . toXml
    toMessage = toMessage . toPublicXml
-}

--    toMessageM = toMessageM . toPublicXml

-- | alias for: @fmap toResponse@
--
-- turns @m a@ into @m 'Response'@ using 'toResponse'.
flatten :: (ToMessage a, Functor f) => f a -> f Response
flatten = fmap toResponse


-- |Honor an @if-modified-since@ header in a 'Request'.
-- If the 'Request' includes the @if-modified-since@ header and the
-- 'Response' has not been modified, then return 304 (Not Modified),
-- otherwise return the 'Response'.
ifModifiedSince :: CalendarTime -- ^ mod-time for the 'Response' (MUST NOT be later than server's time of message origination)
                -> Request -- ^ incoming request (used to check for if-modified-since)
                -> Response -- ^ Response to send if there are modifications
                -> Response
ifModifiedSince modTime request response =
    let repr = formatCalendarTime defaultTimeLocale "%a, %d %b %Y %X GMT" modTime
        notmodified = getHeader "if-modified-since" request == Just (B.pack $ repr)
    in if notmodified
          then noContentLength $ result 304 "" -- Not Modified
          else setHeader "Last-modified" repr response

-- | Deprecated:  use 'composeFilter'.
modifyResponse :: (FilterMonad a m) => (a -> a) -> m()
modifyResponse = composeFilter
{-# DEPRECATED modifyResponse "Use composeFilter" #-}

-- | Set the return code in your response.
setResponseCode :: FilterMonad Response m => Int -> m ()
setResponseCode code
    = composeFilter $ \r -> r{rsCode = code}

-- | Same as @'setResponseCode' status >> return val@.
resp :: (FilterMonad Response m) => Int -> b -> m b
resp status val = setResponseCode status >> return val

-- | Respond with @200 OK@.
ok :: (FilterMonad Response m) => a -> m a
ok = resp 200

-- | Respond with @204 No Content@
--
-- A @204 No Content@ response may not contain a message-body. If you try to supply one, it will be dutifully ignored.
noContent :: (FilterMonad Response m) => a -> m a
noContent val = composeFilter (\r -> noContentLength (r { rsCode = 204, rsBody = L.empty })) >> return val

-- | Respond with @500 Internal Server Error@.
internalServerError :: (FilterMonad Response m) => a -> m a
internalServerError = resp 500

-- | Responds with @502 Bad Gateway@.
badGateway :: (FilterMonad Response m) => a -> m a
badGateway = resp 502

-- | Respond with @400 Bad Request@.
badRequest :: (FilterMonad Response m) => a -> m a
badRequest = resp 400

-- | Respond with @401 Unauthorized@.
unauthorized :: (FilterMonad Response m) => a -> m a
unauthorized = resp 401

-- | Respond with @403 Forbidden@.
forbidden :: (FilterMonad Response m) => a -> m a
forbidden = resp 403

-- | Respond with @404 Not Found@.
notFound :: (FilterMonad Response m) => a -> m a
notFound = resp 404

-- | Respond with @303 See Other@.
seeOther :: (FilterMonad Response m, ToSURI uri) => uri -> res -> m res
seeOther uri res = do modifyResponse $ redirect 303 uri
                      return res

-- | Respond with @302 Found@.
found :: (FilterMonad Response m, ToSURI uri) => uri -> res -> m res
found uri res = do modifyResponse $ redirect 302 uri
                   return res

-- | Respond with @301 Moved Permanently@.
movedPermanently :: (FilterMonad Response m, ToSURI a) => a -> res -> m res
movedPermanently uri res = do modifyResponse $ redirect 301 uri
                              return res

-- | Respond with @307 Temporary Redirect@.
tempRedirect :: (FilterMonad Response m, ToSURI a) => a -> res -> m res
tempRedirect val res = do modifyResponse $ redirect 307 val
                          return res
