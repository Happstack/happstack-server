{-# LANGUAGE DeriveDataTypeable #-}

-- http://tools.ietf.org/html/rfc2109
module Happstack.Server.Cookie
    ( Cookie(..)
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

import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Char
import Data.List
import Data.Generics
import Happstack.Util.Common (Seconds)
import Text.ParserCombinators.Parsec hiding (token)

data Cookie = Cookie
    { cookieVersion :: String
    , cookiePath    :: String
    , cookieDomain  :: String
    , cookieName    :: String
    , cookieValue   :: String
    , secure        :: Bool
    } deriving(Show,Eq,Read,Typeable,Data)

-- | Creates a cookie with a default version of 1 and path of "/"
mkCookie :: String -> String -> Cookie
mkCookie key val = Cookie "1" "/" "" key val False

-- | Set a Cookie in the Result.
-- The values are escaped as per RFC 2109, but some browsers may
-- have buggy support for cookies containing e.g. @\'\"\'@ or @\' \'@.
mkCookieHeader :: Seconds -> Cookie -> String
mkCookieHeader sec cookie =
    let l = [("Domain=",s cookieDomain)
            ,("Max-Age=",if sec < 0 then "" else show sec)
            ,("Path=", cookiePath cookie)
            ,("Version=", s cookieVersion)]
        s f | f cookie == "" = ""
        s f   = '\"' : concatMap e (f cookie) ++ "\""
        e c | fctl c || c == '"' = ['\\',c]
            | otherwise          = [c]
    in concat $ intersperse ";" ((cookieName cookie++"="++s cookieValue):[ (k++v) | (k,v) <- l, "" /= v ] ++ if secure cookie then ["Secure"] else [])

fctl :: Char -> Bool
fctl ch = ch == chr 127 || ch <= chr 31

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
            return $ Cookie ver path domain (low name) val False
          cookie_version = cookie_special "$Version"
          cookie_path = cookie_special "$Path"
          cookie_domain = cookie_special "$Domain"
          cookie_special s = do
            string s
            cookieEq
            value
          cookieSep = ws >> oneOf ",;" >> ws
          cookieEq = ws >> char '=' >> ws
          ws = spaces
          value         = word
          word          = try (quoted_string) <|> incomp_token

          -- Parsers based on RFC 2068
          quoted_string = do
            char '"'
            r <-many (oneOf qdtext)
            char '"'
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

-- | Get all cookies from the HTTP request. The cookies are ordered per RFC from
-- the most specific to the least specific. Multiple cookies with the same
-- name are allowed to exist.
getCookies :: Monad m => C.ByteString -> m [Cookie]
getCookies h = getCookies' h >>=  either (fail. ("Cookie parsing failed!"++)) return

-- | Get the most specific cookie with the given name. Fails if there is no such
-- cookie or if the browser did not escape cookies in a proper fashion.
-- Browser support for escaping cookies properly is very diverse.
getCookie :: Monad m => String -> C.ByteString -> m Cookie
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
            [] -> fail "No cookie found"
            f -> return $ head f

low :: String -> String
low = map toLower

