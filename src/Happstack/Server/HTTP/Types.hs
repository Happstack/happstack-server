{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

module Happstack.Server.HTTP.Types
    (Request(..), Response(..), RqBody(..), Input(..), HeaderPair(..),
     rqURL, mkHeaders,
     getHeader, getHeaderBS, getHeaderUnsafe,
     hasHeader, hasHeaderBS, hasHeaderUnsafe,
     setHeader, setHeaderBS, setHeaderUnsafe,
     addHeader, addHeaderBS, addHeaderUnsafe,
     setRsCode, -- setCookie, setCookies,
     Conf(..), nullConf, result, resultBS,
     redirect, -- redirect_, redirect', redirect'_,
     RsFlags(..), nullRsFlags, noContentLength,
     Version(..), Method(..), Headers, continueHTTP,
     Host, ContentType(..)
    ) where


import qualified Data.Map as M
import Data.Typeable(Typeable)
import Data.Maybe
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString,pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server.SURI
import Data.Char (toLower)

import Happstack.Server.HTTP.Multipart ( ContentType(..) )
import Happstack.Server.Cookie
import Data.List
import Text.Show.Functions ()

-- | HTTP version
data Version = Version Int Int
             deriving(Read,Eq)

instance Show Version where
  show (Version x y) = (show x) ++ "." ++ (show y)

isHTTP1_1 :: Request -> Bool
isHTTP1_1 rq = case rqVersion rq of Version 1 1 -> True; _ -> False
isHTTP1_0 :: Request -> Bool
isHTTP1_0 rq = case rqVersion rq of Version 1 0 -> True; _ -> False

-- | Should the connection be used for further messages after this.
-- | isHTTP1_0 && hasKeepAlive || isHTTP1_1 && hasNotConnectionClose
continueHTTP :: Request -> Response -> Bool
--continueHTTP rq res = isHTTP1_1 rq && getHeader' connectionC rq /= Just closeC && rsfContentLength (rsFlags res)
continueHTTP rq res = (isHTTP1_0 rq && checkHeaderBS connectionC keepaliveC rq) ||
                      (isHTTP1_1 rq && not (checkHeaderBS connectionC closeC rq)) && rsfContentLength (rsFlags res)

-- | HTTP configuration
data Conf = Conf { port      :: Int -- ^ Port for the server to listen on.
                 , validator  :: Maybe (Response -> IO Response)
                 } 
nullConf :: Conf
nullConf = Conf { port      = 8000
                , validator  = Nothing
                }



-- | HTTP request method
data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT
               deriving(Show,Read,Eq)

data HeaderPair = HeaderPair { hName :: ByteString, hValue :: [ByteString] } deriving (Read,Show)
-- | Combined headers.
type Headers = M.Map ByteString HeaderPair -- lowercased name -> (realname, value)



-- | Result flags
data RsFlags = RsFlags 
    { rsfContentLength :: Bool -- ^ whether a content-length header will be added to the result.
    } deriving(Show,Read,Typeable)
nullRsFlags :: RsFlags
nullRsFlags = RsFlags { rsfContentLength = True }
-- | Don't display a Content-Lenght field for the 'Result'.
noContentLength :: Response -> Response
noContentLength res = res { rsFlags = upd } where upd = (rsFlags res) { rsfContentLength = False }

data Input = Input
    { inputValue :: L.ByteString
    , inputFilename :: Maybe String
    , inputContentType :: ContentType
    } deriving (Show,Read,Typeable)

type Host = (String,Int)

data Response  = Response  { rsCode    :: Int,
                             rsHeaders :: Headers,
                             rsFlags   :: RsFlags,
                             rsBody    :: L.ByteString,
                             rsValidator:: Maybe (Response -> IO Response)
                           } deriving (Show,Typeable) 

data Request = Request { rqMethod  :: Method,
                         rqPaths   :: [String],
			 rqUri	   :: String,
                         rqQuery   :: String,
                         rqInputs  :: [(String,Input)],
                         rqCookies :: [(String,Cookie)],
                         rqVersion :: Version,
                         rqHeaders :: Headers,
                         rqBody    :: RqBody,
                         rqPeer    :: Host
                       } deriving(Show,Read,Typeable)



rqURL :: Request -> String
rqURL rq = '/':intercalate "/" (rqPaths rq) ++ (rqQuery rq)

class HasHeaders a where 
    updateHeaders::(Headers->Headers)->a->a
    headers::a->Headers

instance HasHeaders Response where updateHeaders f rs = rs{rsHeaders=f $ rsHeaders rs}
                                   headers = rsHeaders
instance HasHeaders Request where updateHeaders f rq = rq{rqHeaders = f $ rqHeaders rq} 
                                  headers = rqHeaders

instance HasHeaders Headers where updateHeaders f = f
                                  headers = id

newtype RqBody = Body L.ByteString deriving (Read,Show,Typeable)


setRsCode :: (Monad m) => Int -> Response -> m Response
setRsCode code rs = return rs {rsCode = code}

mkHeaders :: [(String,String)] -> Headers
mkHeaders hdrs
    = M.fromListWith join [ (P.pack (map toLower key), HeaderPair (P.pack key) [P.pack value]) | (key,value) <- hdrs ]
    where join (HeaderPair key vs1) (HeaderPair _ vs2) = HeaderPair key (vs1++vs2)

--------------------------------------------------------------
-- Retrieving header information
--------------------------------------------------------------

-- | Lookup header value. Key is case-insensitive.
getHeader :: HasHeaders r => String -> r -> Maybe ByteString
getHeader = getHeaderBS . pack

-- | Lookup header value. Key is a case-insensitive bytestring.
getHeaderBS :: HasHeaders r => ByteString -> r -> Maybe ByteString
getHeaderBS = getHeaderUnsafe . P.map toLower

-- | Lookup header value with a case-sensitive key. The key must be lowercase.
getHeaderUnsafe :: HasHeaders r => ByteString -> r -> Maybe ByteString
getHeaderUnsafe key var = listToMaybe =<< fmap hValue (getHeaderUnsafe' key var)

-- | Lookup header with a case-sensitive key. The key must be lowercase.
getHeaderUnsafe' :: HasHeaders r => ByteString -> r -> Maybe HeaderPair
getHeaderUnsafe' key = M.lookup key . headers

--------------------------------------------------------------
-- Querying header status
--------------------------------------------------------------


hasHeader :: HasHeaders r => String -> r -> Bool
hasHeader key r = isJust (getHeader key r)

hasHeaderBS :: HasHeaders r => ByteString -> r -> Bool
hasHeaderBS key r = isJust (getHeaderBS key r)

hasHeaderUnsafe :: HasHeaders r => ByteString -> r -> Bool
hasHeaderUnsafe key r = isJust (getHeaderUnsafe' key r)

checkHeaderBS :: HasHeaders r => ByteString -> ByteString -> r -> Bool
checkHeaderBS key val = checkHeaderUnsafe (P.map toLower key) (P.map toLower val)

checkHeaderUnsafe :: HasHeaders r => ByteString -> ByteString -> r -> Bool
checkHeaderUnsafe key val r
    = case getHeaderUnsafe key r of
        Just val' | P.map toLower val' == val -> True
        _ -> False


--------------------------------------------------------------
-- Setting header status
--------------------------------------------------------------

setHeader :: HasHeaders r => String -> String -> r -> r
setHeader key val = setHeaderBS (pack key) (pack val)

setHeaderBS :: HasHeaders r => ByteString -> ByteString -> r -> r
setHeaderBS key val = setHeaderUnsafe (P.map toLower key) (HeaderPair key [val])

setHeaderUnsafe :: HasHeaders r => ByteString -> HeaderPair -> r -> r
setHeaderUnsafe key val = updateHeaders (M.insert key val)

--------------------------------------------------------------
-- Adding headers
--------------------------------------------------------------

addHeader :: HasHeaders r => String -> String -> r -> r
addHeader key val = addHeaderBS (pack key) (pack val)

addHeaderBS :: HasHeaders r => ByteString -> ByteString -> r -> r
addHeaderBS key val = addHeaderUnsafe (P.map toLower key) (HeaderPair key [val])

addHeaderUnsafe :: HasHeaders r => ByteString -> HeaderPair -> r -> r
addHeaderUnsafe key val = updateHeaders (M.insertWith join key val)
    where join (HeaderPair k vs1) (HeaderPair _ vs2) = HeaderPair k (vs1++vs2)


result :: Int -> String -> Response
result code = resultBS code . L.pack

resultBS :: Int -> L.ByteString -> Response
resultBS code s = Response code M.empty nullRsFlags s Nothing

redirect :: (ToSURI s) => Int -> s -> Response -> Response
redirect c s resp = setHeaderBS locationC (pack (render (toSURI s))) resp{rsCode = c}



-- constants here
locationC :: ByteString
locationC   = P.pack "Location"
closeC :: ByteString
closeC      = P.pack "close"
connectionC :: ByteString
connectionC = P.pack "Connection"
keepaliveC :: ByteString
keepaliveC  = P.pack "Keep-Alive"

