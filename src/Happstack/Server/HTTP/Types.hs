{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

module Happstack.Server.HTTP.Types
    (Request(..), Response(..), RqBody(..), Input(..), HeaderPair(..),
     takeRequestBody,
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


import Control.Monad.Error (Error(strMsg))
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Data (Data)
import Data.IORef (IORef, atomicModifyIORef, readIORef)
import Data.Typeable(Typeable)
import Data.Maybe
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString,pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server.SURI
import Data.Char (toLower)

import Happstack.Server.HTTP.RFC822Headers ( ContentType(..) )
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

-- | Default configuration contains no validator and the port is set to 8000
nullConf :: Conf
nullConf = Conf { port      = 8000
                , validator  = Nothing
                }

-- | HTTP request method
data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT
               deriving(Show,Read,Eq,Ord,Typeable,Data)

data HeaderPair = HeaderPair { hName :: ByteString, hValue :: [ByteString] } deriving (Read,Show)
-- | Combined headers.
type Headers = M.Map ByteString HeaderPair -- lowercased name -> (realname, value)



-- | Result flags
data RsFlags = RsFlags 
    { rsfContentLength :: Bool -- ^ whether a content-length header will be added to the result.
    } deriving(Show,Read,Typeable)

-- | Default RsFlags that will include the content-length header
nullRsFlags :: RsFlags
nullRsFlags = RsFlags { rsfContentLength = True }
-- | Don't display a Content-Lenght field for the 'Result'.
noContentLength :: Response -> Response
noContentLength res = res { rsFlags = upd } where upd = (rsFlags res) { rsfContentLength = False }

data Input = Input
    { inputValue       :: Either FilePath L.ByteString
    , inputFilename    :: Maybe FilePath
    , inputContentType :: ContentType
    } deriving (Show,Read,Typeable)

type Host = (String,Int)

data Response  = Response  { rsCode      :: Int,
                             rsHeaders   :: Headers,
                             rsFlags     :: RsFlags,
                             rsBody      :: L.ByteString,
                             rsValidator :: Maybe (Response -> IO Response)
                           }
               | SendFile  { rsCode      :: Int,
                             rsHeaders   :: Headers,
                             rsFlags     :: RsFlags,
                             rsValidator :: Maybe (Response -> IO Response),
                             sfFilePath  :: FilePath,  -- file handle to send from
                             sfOffset    :: Integer, -- offset to start at
                             sfCount     :: Integer  -- number of bytes to send
                           }
               deriving (Show,Typeable)

-- what should the status code be ?
instance Error Response where
  strMsg str = 
      setHeader "Content-Type" "text/plain; charset=UTF-8" $ 
       result 500 str

data Request = Request { rqMethod      :: Method,
                         rqPaths       :: [String],
                         rqUri         :: String,
                         rqQuery       :: String,
                         rqInputsQuery :: [(String,Input)],
                         rqInputsBody  :: MVar [(String,Input)],
                         rqCookies     :: [(String,Cookie)],
                         rqVersion     :: Version,
                         rqHeaders     :: Headers,
                         rqBody        :: MVar RqBody,
                         rqPeer        :: Host
                       } deriving(Typeable)

-- | get the request body from the Request and replace it with Nothing
--
-- IMPORTANT: You can really only call this function once. Subsequent
-- calls will return 'Nothing'.
takeRequestBody :: Request -> IO (Maybe RqBody)
takeRequestBody rq = tryTakeMVar (rqBody rq) 
-- takeRequestBody rq = return (rqBody rq)
{-
takeRequestBody rq = 
    do body <- atomicModifyIORef (rqBody rq) (\bdy -> (Nothing, bdy))
       newBD <- readIORef (rqBody rq)
       print newBD
       newBD `seq` return body
-}
-- | Converts a Request into a String representing the corresponding URL
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

newtype RqBody = Body { unBody :: L.ByteString } deriving (Read,Show,Typeable)

-- | Sets the Response status code to the provided Int and lifts the computation
-- into a Monad.
setRsCode :: (Monad m) => Int -> Response -> m Response
setRsCode code rs = return rs {rsCode = code}

-- | Takes a list of (key,val) pairs and converts it into Headers.  The
-- keys will be converted to lowercase
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

-- | Returns True if the associated key is found in the Headers.  The lookup
-- is case insensitive.
hasHeader :: HasHeaders r => String -> r -> Bool
hasHeader key r = isJust (getHeader key r)

-- | Acts as 'hasHeader' with ByteStrings
hasHeaderBS :: HasHeaders r => ByteString -> r -> Bool
hasHeaderBS key r = isJust (getHeaderBS key r)

-- | Acts as 'hasHeaderBS' but the key is case sensitive.  It should be
-- in lowercase.
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

-- | Associates the key/value pair in the headers.  Forces the key to be
-- lowercase.
setHeader :: HasHeaders r => String -> String -> r -> r
setHeader key val = setHeaderBS (pack key) (pack val)

-- | Acts as 'setHeader' but with ByteStrings.
setHeaderBS :: HasHeaders r => ByteString -> ByteString -> r -> r
setHeaderBS key val = setHeaderUnsafe (P.map toLower key) (HeaderPair key [val])

-- | Sets the key to the HeaderPair.  This is the only way to associate a key
-- with multiple values via the setHeader* functions.  Does not force the key
-- to be in lowercase or guarantee that the given key and the key in the HeaderPair will match. 
setHeaderUnsafe :: HasHeaders r => ByteString -> HeaderPair -> r -> r
setHeaderUnsafe key val = updateHeaders (M.insert key val)

--------------------------------------------------------------
-- Adding headers
--------------------------------------------------------------

-- | Add a key/value pair to the header.  If the key already has a value
-- associated with it, then the value will be appended.  
-- Forces the key to be lowercase.
addHeader :: HasHeaders r => String -> String -> r -> r
addHeader key val = addHeaderBS (pack key) (pack val)

-- | Acts as addHeader except for ByteStrings
addHeaderBS :: HasHeaders r => ByteString -> ByteString -> r -> r
addHeaderBS key val = addHeaderUnsafe (P.map toLower key) (HeaderPair key [val])

-- | Add a key/value pair to the header using the underlying HeaderPair data
-- type.  Does not force the key to be in lowercase or guarantee that the given key and the key in the HeaderPair will match. 
addHeaderUnsafe :: HasHeaders r => ByteString -> HeaderPair -> r -> r
addHeaderUnsafe key val = updateHeaders (M.insertWith join key val)
    where join (HeaderPair k vs1) (HeaderPair _ vs2) = HeaderPair k (vs1++vs2)

-- | Creates a Response with the given Int as the status code and the provided
-- String as the body of the Response 
result :: Int -> String -> Response
result code = resultBS code . L.pack

-- | Acts as 'result' but works with ByteStrings directly.
resultBS :: Int -> L.ByteString -> Response
resultBS code s = Response code M.empty nullRsFlags s Nothing

-- | Sets the Response's status code to the given Int and redirects to the given URI
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

