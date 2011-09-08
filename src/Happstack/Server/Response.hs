{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, ScopedTypeVariables #-}
-- | Functions and classes related to generating a 'Response' and setting the response code. For detailed instruction see the Happstack Crash Course: <http://happstack.com/docs/crashcourse/HelloWorld.html#response_code>
module Happstack.Server.Response 
    ( -- * Converting values to a 'Response'
      ToMessage(..)
    , flatten
    , toResponseBS
      -- * Setting the Response Code
    , ok
    , noContent
    , internalServerError
    , badGateway
    , badRequest
    , unauthorized
    , forbidden
    , notFound
    , requestEntityTooLarge
    , seeOther
    , found
    , movedPermanently
    , tempRedirect
    , setResponseCode
    , resp
    -- * Handling if-modified-since
    , ifModifiedSince
    ) where

import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as L
import qualified Data.ByteString.Lazy.UTF8       as LU (fromString)
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.Text.Lazy.Encoding         as LT
import           Happstack.Server.Internal.Monads         (FilterMonad(composeFilter))
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
-- 
-- example:
-- 
-- > import Data.ByteString.Char8 as C
-- > import Data.ByteString.Lazy.Char8 as L
-- > import Happstack.Server
-- >
-- > main = simpleHTTP nullConf $ ok $ toResponseBS (C.pack "text/plain") (L.pack "hello, world")
--
-- (note: 'C.pack' and 'L.pack' only work for ascii. For unicode strings you would need to use @utf8-string@, @text@, or something similar to create a valid 'ByteString').
toResponseBS :: B.ByteString -- ^ content-type
             -> L.ByteString -- ^ response body
             -> Response
toResponseBS contentType message =
    let res = Response 200 M.empty nullRsFlags message Nothing
    in setHeaderBS (B.pack "Content-Type") contentType res


-- | 'toResponse' will convert a value into a 'Response' body,
-- set the @content-type@, and set the default response code for that type.
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

instance ToMessage T.Text where
    toContentType _ = B.pack "text/plain; charset=UTF-8"
    toMessage t = L.fromChunks [T.encodeUtf8 t]

instance ToMessage LT.Text where
    toContentType _ = B.pack "text/plain; charset=UTF-8"
    toMessage = LT.encodeUtf8

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

instance ToMessage L.ByteString where
    toResponse bs = Response 200 M.empty nullRsFlags bs Nothing

instance ToMessage B.ByteString where
    toResponse bs = toResponse (L.fromChunks [bs])

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
--
-- > main = simpleHTTP nullConf $ flatten $ do return "flatten me."
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

-- | Set an arbitrary return code in your response.
--
-- A filter for setting the response code. Generally you will use a
-- helper function like 'ok' or 'seeOther'.
-- 
-- > main = simpleHTTP nullConf $ do setResponseCode 200
-- >                                 return "Everything is OK"
-- 
-- see also: 'resp'
setResponseCode :: FilterMonad Response m => 
                   Int -- ^ response code
                -> m ()
setResponseCode code
    = composeFilter $ \r -> r{rsCode = code}

-- | Same as @'setResponseCode' status >> return val@.
-- 
-- Use this if you want to set a response code that does not already
-- have a helper function. 
-- 
-- > main = simpleHTTP nullConf $ resp 200 "Everything is OK"
resp :: (FilterMonad Response m) => 
        Int -- ^ response code
     -> b   -- ^ value to return
     -> m b
resp status val = setResponseCode status >> return val

-- | Respond with @200 OK@.
-- 
-- > main = simpleHTTP nullConf $ ok "Everything is OK"
ok :: (FilterMonad Response m) => a -> m a
ok = resp 200

-- | Respond with @204 No Content@
--
-- A @204 No Content@ response may not contain a message-body. If you try to supply one, it will be dutifully ignored.
--
-- > main = simpleHTTP nullConf $ noContent "This will be ignored."
noContent :: (FilterMonad Response m) => a -> m a
noContent val = composeFilter (\r -> noContentLength (r { rsCode = 204, rsBody = L.empty })) >> return val

-- | Respond with @500 Internal Server Error@.
--
-- > main = simpleHTTP nullConf $ internalServerError "Sorry, there was an internal server error."
internalServerError :: (FilterMonad Response m) => a -> m a
internalServerError = resp 500

-- | Responds with @502 Bad Gateway@.
--
-- > main = simpleHTTP nullConf $ badGateway "Bad Gateway."
badGateway :: (FilterMonad Response m) => a -> m a
badGateway = resp 502

-- | Respond with @400 Bad Request@.
--
-- > main = simpleHTTP nullConf $ badRequest "Bad Request."
badRequest :: (FilterMonad Response m) => a -> m a
badRequest = resp 400

-- | Respond with @401 Unauthorized@.
--
-- > main = simpleHTTP nullConf $ unauthorized "You are not authorized."
unauthorized :: (FilterMonad Response m) => a -> m a
unauthorized = resp 401

-- | Respond with @403 Forbidden@.
--
-- > main = simpleHTTP nullConf $ forbidden "Sorry, it is forbidden."
forbidden :: (FilterMonad Response m) => a -> m a
forbidden = resp 403

-- | Respond with @404 Not Found@.
-- 
-- > main = simpleHTTP nullConf $ notFound "What you are looking for has not been found."
notFound :: (FilterMonad Response m) => a -> m a
notFound = resp 404

-- | Respond with @413 Request Entity Too Large@.
--
-- > main = simpleHTTP nullConf $ requestEntityTooLarge "That's too big for me to handle."
requestEntityTooLarge :: (FilterMonad Response m) => a -> m a
requestEntityTooLarge = resp 413

-- | Respond with @303 See Other@.
--
-- > main = simpleHTTP nullConf $ seeOther "http://example.org/" "What you are looking for is now at http://example.org/"
--
-- NOTE: The second argument of 'seeOther' is the message body which will sent to the browser. According to the HTTP 1.1 spec,
--
-- @the entity of the response SHOULD contain a short hypertext note with a hyperlink to the new URI(s).@
--
-- This is because pre-HTTP\/1.1 user agents do not support 303. However, in practice you can probably just use @\"\"@ as the second argument.
seeOther :: (FilterMonad Response m, ToSURI uri) => uri -> res -> m res
seeOther uri res = do modifyResponse $ redirect 303 uri
                      return res

-- | Respond with @302 Found@.
-- 
-- You probably want 'seeOther'. This method is not in popular use anymore, and is generally treated like 303 by most user-agents anyway.
found :: (FilterMonad Response m, ToSURI uri) => uri -> res -> m res
found uri res = do modifyResponse $ redirect 302 uri
                   return res

-- | Respond with @301 Moved Permanently@.
--
-- > main = simpleHTTP nullConf $ movedPermanently "http://example.org/" "What you are looking for is now at http://example.org/"
movedPermanently :: (FilterMonad Response m, ToSURI a) => a -> res -> m res
movedPermanently uri res = do modifyResponse $ redirect 301 uri
                              return res

-- | Respond with @307 Temporary Redirect@.
--
-- > main = simpleHTTP nullConf $ tempRedirect "http://example.org/" "What you are looking for is temporarily at http://example.org/"
tempRedirect :: (FilterMonad Response m, ToSURI a) => a -> res -> m res
tempRedirect val res = do modifyResponse $ redirect 307 val
                          return res
