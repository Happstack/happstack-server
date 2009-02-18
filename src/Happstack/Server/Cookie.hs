{-# LANGUAGE DeriveDataTypeable #-}

-- http://tools.ietf.org/html/rfc2109
module Happstack.Server.Cookie
    ( Cookie(..), mkCookie, mkCookieHeader
    , getCookies, getCookie )
    where

import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import Data.Generics
import Happstack.Util.Common (Seconds)
import Text.ParserCombinators.ReadP

data Cookie = Cookie
    { cookieVersion :: String
    , cookiePath    :: String
    , cookieDomain  :: String
    , cookieName    :: String
    , cookieValue   :: String
    } deriving(Show,Eq,Read,Typeable,Data)

-- | Creates a cookie with a default version of 1 and path of "/"
mkCookie :: String -> String -> Cookie
mkCookie key val = Cookie "1" "/" "" key val

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
    in concat $ intersperse ";" ((cookieName cookie++"="++s cookieValue):[ (k++v) | (k,v) <- l, "" /= v ])


{- Cookie syntax:
   av-pairs        =       av-pair *(";" av-pair)
   av-pair         =       attr ["=" value]        ; optional value
   attr            =       token
   value           =       word
   word            =       token | quoted-string
-}

gmany :: ReadP a -> ReadP [a]
gmany  p = gmany1 p <++ return []
gmany1 :: ReadP a -> ReadP [a]
gmany1 p = do x  <- p
              xs <- gmany1 p <++ return []
              return (x:xs)
gskipMany1 :: ReadP a -> ReadP ()
gskipMany1 p = p >> (gskipMany p <++ return ())
gskipMany :: ReadP a -> ReadP ()
gskipMany  p = gskipMany1 p <++ return ()

fctl :: Char -> Bool
fctl         = \ch -> ch == chr 127 || ch <= chr 31
fseparator :: Char -> Bool
fseparator   = \ch -> ch `elem` "()<>@,;:\\\"[]?={} \t" -- ignore '/' here
fchar :: Char -> Bool
fchar        = \ch -> ch <= chr 127
ftoken :: Char -> Bool
ftoken       = \ch -> fchar ch && not (fctl ch || fseparator ch)
lws :: ReadP ()
lws          = ((char '\r' >> char '\n') <++ return ' ') >> gskipMany (satisfy (\ch -> ch == ' ' || ch == '\t'))
token :: ReadP [Char]
token        = gmany $ satisfy ftoken
quotedString :: ReadP [Char]
quotedString = do char '"'  -- " stupid emacs syntax highlighting
                  x <- many ((char '\\' >> satisfy fchar) <++ (satisfy $ \ch -> ch /= '"' && fchar ch && (ch == ' ' || ch == '\t' || not (fctl ch))))
                  char '"' -- " stupid emacs syntax highlighting
                  return x
word :: ReadP [Char]
word = quotedString <++ token

avPair :: ReadP (String, [Char])
avPair = do
  k <- token
  lws >> char '=' >> lws
  v <- word
  return (low k,v)

sep :: ReadP ()
sep = lws >> satisfy (\ch -> ch == ',' || ch == ';') >> lws

cookies :: ReadP [Cookie]
cookies = do
  let kpw n = do lws
                 (k,v) <- avPair
                 if k == n then return v else fail "Invalid key"
  ver <- ((kpw "$version" <~ sep) <++ return "")
  let ci = do (k,v) <- avPair
              p <- (sep >> kpw "$path")   <++ return ""
              d <- (sep >> kpw "$domain") <++ return ""
              return $ Cookie ver p d k v
  x  <- lws >> ci
  xs <- gmany (sep >> ci) <~ lws
  return (x:xs)

(<~) :: Monad m => m a -> m b -> m a
(<~) a b = do x <- a; b; return x

parse :: Monad m => String -> m [Cookie]
parse i = case readP_to_S cookies i of
            [(res,"")] -> return res
            xs         -> fail ("Invalid cookie syntax!: at position "++show (length i - length xs)++" input "++show i)

-- | Get all cookies from the HTTP request. The cookies are ordered per RFC from
-- the most specific to the least specific. Multiple cookies with the same
-- name are allowed to exist.
getCookies :: Monad m => C.ByteString -> m [Cookie]
getCookies header | C.null header = return []
                  | otherwise     = parse (C.unpack header)


-- | Get the most specific cookie with the given name. Fails if there is no such
-- cookie or if the browser did not escape cookies in a proper fashion.
-- Browser support for escaping cookies properly is very diverse.
getCookie :: Monad m => String -> C.ByteString -> m Cookie
getCookie s h = do cs <- getCookies h
                   case filter ((==) (low s) . cookieName) cs of
                     [r] -> return r
                     _   -> fail ("getCookie: " ++ show s)

low :: String -> String
low = map toLower
