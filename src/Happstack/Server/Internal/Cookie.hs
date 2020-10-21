{-# LANGUAGE CPP, DeriveDataTypeable #-}

-- http://tools.ietf.org/html/rfc2109
module Happstack.Server.Internal.Cookie
    ( Cookie(..)
    , CookieLife(..)
    , SameSite(..)
    , calcLife
    , mkCookie
    , mkCookieHeader
    , getCookies
    , getCookie
    , getCookies'
    , getCookie'
    , parseCookies
    , cookiesParser
    )
    where

import Control.Monad
import Control.Monad.Fail (MonadFail)
import qualified Data.ByteString.Char8 as C
import Data.Char             (chr, toLower)
import Data.Data             (Data, Typeable)
import Data.List             ((\\), intersperse)
import Data.Time.Clock       (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Happstack.Server.Internal.Clock (getApproximateUTCTime)
import Network.URI           (escapeURIString)
import Text.ParserCombinators.Parsec hiding (token)

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#else
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)
#endif

-- | a type for HTTP cookies. Usually created using 'mkCookie'.
data Cookie = Cookie
    { cookieVersion :: String
    , cookiePath    :: String
    , cookieDomain  :: String
    , cookieName    :: String
    , cookieValue   :: String
    , secure        :: Bool
    , httpOnly      :: Bool
    , sameSite      :: SameSite
    } deriving(Show,Eq,Read,Typeable,Data)

-- | Specify the lifetime of a cookie.
--
-- Note that we always set the max-age and expires headers because
-- internet explorer does not honor max-age. You can specific 'MaxAge'
-- or 'Expires' and the other will be calculated for you. Choose which
-- ever one makes your life easiest.
--
data CookieLife
    = Session         -- ^ session cookie - expires when browser is closed
    | MaxAge Int      -- ^ life time of cookie in seconds
    | Expires UTCTime -- ^ cookie expiration date
    | Expired         -- ^ cookie already expired
      deriving (Eq, Ord, Read, Show, Typeable)

-- | Options for specifying third party cookie behaviour.
--
-- Note that most or all web clients require the cookie to be secure if "none" is
-- specified.
data SameSite
    = SameSiteLax
    -- ^ The cookie is sent in first party contexts as well as linked requests initiated
    -- from other contexts.
    | SameSiteStrict
    -- ^ The cookie is sent in first party contexts only.
    | SameSiteNone
    -- ^ The cookie is sent in first as well as third party contexts if the cookie is
    -- secure.
    | SameSiteNoValue
    -- ^ The default; used if you do not wish a SameSite attribute present at all.
      deriving (Eq, Ord, Typeable, Data, Show, Read)

displaySameSite :: SameSite -> String
displaySameSite ss =
  case ss of
    SameSiteLax     -> "SameSite=Lax"
    SameSiteStrict  -> "SameSite=Strict"
    SameSiteNone    -> "SameSite=None"
    SameSiteNoValue -> ""

-- convert 'CookieLife' to the argument needed for calling 'mkCookieHeader'
calcLife :: CookieLife -> IO (Maybe (Int, UTCTime))
calcLife Session = return Nothing
calcLife (MaxAge s) =
          do now <- getApproximateUTCTime
             return (Just (s, addUTCTime (fromIntegral s) now))
calcLife (Expires expirationDate) =
          do now <- getApproximateUTCTime
             return $ Just (round  $ expirationDate `diffUTCTime` now, expirationDate)
calcLife Expired =
          return $ Just (0, posixSecondsToUTCTime 0)


-- | Creates a cookie with a default version of 1, empty domain, a
-- path of "/", secure == False, httpOnly == False and
-- sameSite == SameSiteNoValue
--
-- see also: 'addCookie'
mkCookie :: String  -- ^ cookie name
         -> String  -- ^ cookie value
         -> Cookie
mkCookie key val = Cookie "1" "/" "" key val False False SameSiteNoValue

-- | Set a Cookie in the Result.
-- The values are escaped as per RFC 2109, but some browsers may
-- have buggy support for cookies containing e.g. @\'\"\'@ or @\' \'@.
--
-- Also, it seems that chrome, safari, and other webkit browsers do
-- not like cookies which have double quotes around the domain and
-- reject/ignore the cookie. So, we no longer quote the domain.
--
-- internet explorer does not honor the max-age directive so we set
-- both max-age and expires.
--
-- See 'CookieLife' and 'calcLife' for a convenient way of calculating
-- the first argument to this function.
mkCookieHeader :: Maybe (Int, UTCTime) -> Cookie -> String
mkCookieHeader mLife cookie =
  let
    l =
      [ (,) "Domain="  (cookieDomain cookie)
      , (,) "Max-Age=" (maybe "" (show . max 0 . fst) mLife)
      , (,) "expires=" (maybe "" (formatTime'  . snd) mLife)
      , (,) "Path="    (cookiePath cookie)
      , (,) "Version=" (s cookieVersion)
      ]
    formatTime' =
      formatTime defaultTimeLocale "%a, %d-%b-%Y %X GMT"
    encode =
      escapeURIString
        (\c -> c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~"))
    s f | f cookie == "" = ""
        | otherwise      = '\"' : (encode $ f cookie) ++ "\""
  in
    concat $ intersperse ";" $
         (cookieName cookie++"="++s cookieValue):[ (k++v) | (k,v) <- l, "" /= v ]
      ++ (if secure   cookie then ["Secure"]   else [])
      ++ (if httpOnly cookie then ["HttpOnly"] else [])
      ++ (if sameSite cookie /= SameSiteNoValue
          then [displaySameSite . sameSite $ cookie] else [])

-- | Not an supported api.  Takes a cookie header and returns
-- either a String error message or an array of parsed cookies
parseCookies :: String -> Either String [Cookie]
parseCookies str = either (Left . show) Right $ parse cookiesParser str str

-- | not a supported api.  A parser for RFC 2109 cookies
cookiesParser :: GenParser Char st [Cookie]
cookiesParser = cookies
    where -- Parsers based on RFC 2109
          cookies = do
            ws
            ver<-option "" $ try (cookie_version >>= (\x -> cookieSep >> return x))
            cookieList<-(cookie_value ver) `sepBy1` try cookieSep
            ws
            eof
            return cookieList
          cookie_value ver = do
            name<-name_parser
            cookieEq
            val<-value
            path<-option "" $ try (cookieSep >> cookie_path)
            domain<-option "" $ try (cookieSep >> cookie_domain)
            return $ Cookie ver path domain (low name) val False False SameSiteNoValue
          cookie_version = cookie_special "$Version"
          cookie_path = cookie_special "$Path"
          cookie_domain = cookie_special "$Domain"
          cookie_special s = do
            void $ string s
            cookieEq
            value
          cookieSep = ws >> oneOf ",;" >> ws
          cookieEq = ws >> char '=' >> ws
          ws = spaces
          value         = word
          word          = try quoted_string <|> try incomp_token <|> return ""

          -- Parsers based on RFC 2068
          quoted_string = do
            void $ char '"'
            r <-many ((try quotedPair) <|> (oneOf qdtext))
            void $ char '"'
            return r

          -- Custom parsers, incompatible with RFC 2068, but more forgiving ;)
          incomp_token  = many1 $ oneOf ((chars \\ ctl) \\ " \t\";")
          name_parser   = many1 $ oneOf ((chars \\ ctl) \\ "= ;,")

          -- Primitives from RFC 2068
          ctl           = map chr (127:[0..31])
          chars         = map chr [0..127]
          octet         = map chr [0..255]
          text          = octet \\ ctl
          qdtext        = text \\ "\""
          quotedPair    = char '\\' >> anyChar

-- | Get all cookies from the HTTP request. The cookies are ordered per RFC from
-- the most specific to the least specific. Multiple cookies with the same
-- name are allowed to exist.
getCookies :: MonadFail m => C.ByteString -> m [Cookie]
getCookies h = getCookies' h >>=  either (fail. ("Cookie parsing failed!"++)) return

-- | Get the most specific cookie with the given name. Fails if there is no such
-- cookie or if the browser did not escape cookies in a proper fashion.
-- Browser support for escaping cookies properly is very diverse.
getCookie :: MonadFail m => String -> C.ByteString -> m Cookie
getCookie s h = getCookie' s h >>= either (const $ fail ("getCookie: " ++ show s)) return

getCookies' :: Monad m => C.ByteString -> m (Either String [Cookie])
getCookies' header | C.null header = return $ Right []
                   | otherwise     = return $ parseCookies (C.unpack header)

getCookie' :: Monad m => String -> C.ByteString -> m (Either String Cookie)
getCookie' s h = do
    cs <- getCookies' h
    return $ do -- Either
       cooks <- cs
       case filter (\x->(==)  (low s)  (cookieName x) ) cooks of
            [] -> Left "No cookie found"
            f -> return $ head f

low :: String -> String
low = map toLower
