{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, RankNTypes #-}

module Happstack.Server.Internal.Types
    (Request(..), Response(..), RqBody(..), Input(..), HeaderPair(..),
     takeRequestBody, readInputsBody,
     rqURL, mkHeaders,
     getHeader, getHeaderBS, getHeaderUnsafe,
     hasHeader, hasHeaderBS, hasHeaderUnsafe,
     setHeader, setHeaderBS, setHeaderUnsafe,
     addHeader, addHeaderBS, addHeaderUnsafe,
     setRsCode, -- setCookie, setCookies,
     Conf(..), nullConf, result, resultBS,
     redirect, -- redirect_, redirect', redirect'_,
     isHTTP1_0, isHTTP1_1,
     RsFlags(..), nullRsFlags, contentLength, chunked, noContentLength,
     HttpVersion(..), Length(..), Method(..), Headers, continueHTTP,
     Host, ContentType(..),
     readDec', readM, FromReqURI(..)
    ) where


import Control.Monad.Error (Error(strMsg))
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Data (Data)
import Data.IORef (IORef, atomicModifyIORef, readIORef)
import Data.Time.Format (FormatTime(..))
import Data.Typeable(Typeable)
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString,pack)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8  as LU (fromString)
import Data.Int   (Int, Int8, Int16, Int32, Int64)
import Data.Maybe
import Data.List
import Data.Word  (Word, Word8, Word16, Word32, Word64)
import Happstack.Server.SURI
import Data.Char (toLower)
import Happstack.Server.Internal.RFC822Headers ( ContentType(..) )
import Happstack.Server.Internal.Cookie
import Happstack.Util.LogFormat (formatRequestCombined)
import Numeric (readDec)
import System.Log.Logger (Priority(..), logM)
import Text.Show.Functions ()

-- | HTTP version
data HttpVersion = HttpVersion Int Int
             deriving(Read,Eq)

instance Show HttpVersion where
  show (HttpVersion x y) = (show x) ++ "." ++ (show y)

-- | 'True' if 'Request' is HTTP version @1.1@
isHTTP1_1 :: Request -> Bool
isHTTP1_1 rq = case rqVersion rq of HttpVersion 1 1 -> True; _ -> False

-- | 'True' if 'Request' is HTTP version @1.0@
isHTTP1_0 :: Request -> Bool
isHTTP1_0 rq = case rqVersion rq of HttpVersion 1 0 -> True; _ -> False

-- | Should the connection be used for further messages after this.
-- | isHTTP1_0 && hasKeepAlive || isHTTP1_1 && hasNotConnectionClose
continueHTTP :: Request -> Response -> Bool
--continueHTTP rq res = isHTTP1_1 rq && getHeader' connectionC rq /= Just closeC && rsfContentLength (rsFlags res)
continueHTTP rq res = (isHTTP1_0 rq && checkHeaderBS connectionC keepaliveC rq   && rsfLength (rsFlags res) == ContentLength) ||
                      (isHTTP1_1 rq && not (checkHeaderBS connectionC closeC rq) && rsfLength (rsFlags res) /= NoContentLength)

-- | HTTP configuration
data Conf = Conf { port       :: Int -- ^ Port for the server to listen on.
                 , validator  :: Maybe (Response -> IO Response) -- ^ a function to validate the output on-the-fly
                 , logAccess  :: forall t. FormatTime t => Maybe (String -> String -> t -> String -> Int -> Integer -> String -> String -> IO ()) -- ^ function to log access requests (see also: 'logMAccess')
                 , timeout    :: Int -- ^ number of seconds to wait before killing an inactive thread
                 } 

-- | Default configuration contains no validator and the port is set to 8000
nullConf :: Conf
nullConf = Conf { port      = 8000
                , validator = Nothing
                , logAccess = Just logMAccess
                , timeout   = 30
                }

-- | log access requests using hslogger and apache-style log formatting
--
-- see also: 'Conf'
logMAccess host user time requestLine responseCode size referer userAgent =
    logM "Happstack.Server.AccessLog.Combined" INFO $ formatRequestCombined host user time requestLine responseCode size referer userAgent

-- | HTTP request method
data Method  = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT
               deriving(Show,Read,Eq,Ord,Typeable,Data)

-- | an HTTP header
data HeaderPair 
    = HeaderPair { hName :: ByteString     -- ^ header name
                 , hValue :: [ByteString]  -- ^ header value (or values if multiple occurances of the header are present)
                 } 
      deriving (Read,Show)
-- | Combined headers.

-- | a Map of HTTP headers
-- 
-- the Map key is the header converted to lowercase
type Headers = M.Map ByteString HeaderPair -- ^ lowercased name -> (realname, value)

-- | A flag value set in the 'Response' which controls how the
-- @Content-Length@ header is set, and whether *chunked* output
-- encoding is used.
--
-- see also: 'nullRsFlags', 'notContentLength', and 'chunked'
data Length 
    = ContentLength             -- ^ automatically add a @Content-Length@ header to the 'Response'
    | TransferEncodingChunked   -- ^ do not add a @Content-Length@ header. Do use @chunked@ output encoding
    | NoContentLength           -- ^ do not set @Content-Length@ or @chunked@ output encoding.
      deriving (Eq, Ord, Read, Show, Enum)

-- | Result flags
data RsFlags = RsFlags 
    { rsfLength :: Length
    } deriving (Show,Read,Typeable)

-- | Default RsFlags: automatically use @Transfer-Encoding: Chunked@.
nullRsFlags :: RsFlags
nullRsFlags = RsFlags { rsfLength = TransferEncodingChunked }

-- | Do not automatically add a Content-Length field to the 'Response'
noContentLength :: Response -> Response
noContentLength res = res { rsFlags = flags } where flags = (rsFlags res) { rsfLength = NoContentLength }

-- | Do not automatically add a Content-Length header. Do automatically use Transfer-Encoding: Chunked
chunked :: Response -> Response
chunked res         = res { rsFlags = flags } where flags = (rsFlags res) { rsfLength = TransferEncodingChunked }

-- | Automatically add a Content-Length header. Do not use Transfer-Encoding: Chunked
contentLength :: Response -> Response
contentLength res   = res { rsFlags = flags } where flags = (rsFlags res) { rsfLength = ContentLength }


-- | a value extract from the @QUERY_STRING@ or 'Request' body
--
-- If the input value was a file, then it will be saved to a temporary file on disk and 'inputValue' will contain @Left pathToTempFile@.
data Input = Input
    { inputValue       :: Either FilePath L.ByteString
    , inputFilename    :: Maybe FilePath
    , inputContentType :: ContentType
    } deriving (Show,Read,Typeable)

-- | hostname & port
type Host = (String, Int) -- ^ (hostname, port)

-- | an HTTP Response
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
                             sfFilePath  :: FilePath,  -- ^ file handle to send from
                             sfOffset    :: Integer,   -- ^ offset to start at
                             sfCount     :: Integer    -- ^ number of bytes to send
                           }
               deriving (Typeable)

instance Show Response where
    showsPrec _ res@Response{}  =
        showString   "================== Response ================" .
        showString "\nrsCode      = " . shows      (rsCode res)     .
        showString "\nrsHeaders   = " . shows      (rsHeaders res)  .
        showString "\nrsFlags     = " . shows      (rsFlags res)    .
        showString "\nrsBody      = " . shows      (rsBody res)     .
        showString "\nrsValidator = " . shows      (rsValidator res)
    showsPrec _ res@SendFile{}  =
        showString   "================== Response ================" .
        showString "\nrsCode      = " . shows      (rsCode res)     .
        showString "\nrsHeaders   = " . shows      (rsHeaders res)  .
        showString "\nrsFlags     = " . shows      (rsFlags res)    .
        showString "\nrsValidator = " . shows      (rsValidator res).
        showString "\nsfFilePath  = " . shows      (sfFilePath res) .
        showString "\nsfOffset    = " . shows      (sfOffset res)   .
        showString "\nsfCount     = " . shows      (sfCount res)

-- what should the status code be ?
instance Error Response where
  strMsg str = 
      setHeader "Content-Type" "text/plain; charset=UTF-8" $ 
       result 500 str

-- | an HTTP request
data Request = Request { rqMethod      :: Method,
                         rqPaths       :: [String],
                         rqUri         :: String,
                         rqQuery       :: String,
                         rqInputsQuery :: [(String,Input)],
                         rqInputsBody  :: MVar [(String,Input)],
                         rqCookies     :: [(String,Cookie)],
                         rqVersion     :: HttpVersion,
                         rqHeaders     :: Headers,
                         rqBody        :: MVar RqBody,
                         rqPeer        :: Host
                       } deriving(Typeable)

instance Show Request where
    showsPrec _ rq =
        showString   "================== Request =================" .
        showString "\nrqMethod      = " . shows      (rqMethod rq) .
        showString "\nrqPaths       = " . shows      (rqPaths rq) .
        showString "\nrqUri         = " . showString (rqUri rq) .
        showString "\nrqQuery       = " . showString (rqQuery rq) .
        showString "\nrqInputsQuery = " . shows      (rqInputsQuery rq) .
        showString "\nrqInputsBody  = " . showString "<<mvar>>" .
        showString "\nrqCookies     = " . shows      (rqCookies rq) .
        showString "\nrqVersion     = " . shows      (rqVersion rq) .
        showString "\nrqHeaders     = " . shows      (rqHeaders rq) .
        showString "\nrqBody        = " . showString "<<mvar>>" .
        showString "\nrqPeer        = " . shows      (rqPeer rq)

-- | get the request body from the Request and replace it with Nothing
--
-- IMPORTANT: You can really only call this function once. Subsequent
-- calls will return 'Nothing'.
takeRequestBody :: (MonadIO m) => Request -> m (Maybe RqBody)
takeRequestBody rq = liftIO $ tryTakeMVar (rqBody rq) 
-- takeRequestBody rq = return (rqBody rq)


-- | read the request body inputs
--
-- This will only work if the body inputs have already been decoded. Otherwise it will return Nothing.
readInputsBody :: Request -> IO (Maybe [(String, Input)])
readInputsBody req =
    do mbi <- tryTakeMVar (rqInputsBody req)
       case mbi of
         (Just bi) ->
                do putMVar (rqInputsBody req) bi
                   return (Just bi)
         Nothing -> return Nothing

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

-- | a class for working with types that contain HTTP headers
class HasHeaders a where 
    updateHeaders :: (Headers->Headers) -> a -> a -- ^ modify the headers
    headers       :: a -> Headers -- ^ extract the headers

instance HasHeaders Response where updateHeaders f rs = rs{rsHeaders=f $ rsHeaders rs}
                                   headers = rsHeaders
instance HasHeaders Request where updateHeaders f rq = rq{rqHeaders = f $ rqHeaders rq} 
                                  headers = rqHeaders

instance HasHeaders Headers where updateHeaders f = f
                                  headers = id

-- | The body of an HTTP 'Request'
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
result code = resultBS code . LU.fromString

-- | Acts as 'result' but works with ByteStrings directly.
-- 
-- By default, Transfer-Encoding: chunked will be used
resultBS :: Int -> L.ByteString -> Response
resultBS code s = Response code M.empty nullRsFlags s Nothing

-- | Sets the Response's status code to the given Int and redirects to the given URI
redirect :: (ToSURI s) => Int -> s -> Response -> Response
redirect c s resp = setHeaderBS locationC (pack (render (toSURI s))) resp{rsCode = c}

-- constants here

-- | @Location@
locationC :: ByteString
locationC   = P.pack "Location"

-- | @close@
closeC :: ByteString
closeC      = P.pack "close"

-- | @Connection@
connectionC :: ByteString
connectionC = P.pack "Connection"

-- | @Keep-Alive@
keepaliveC :: ByteString
keepaliveC  = P.pack "Keep-Alive"

readDec' :: (Num a) => String -> a
readDec' s =
  case readDec s of
    [(n,[])] -> n
    _    -> error "readDec' failed."
    
-- | Read in any monad.
readM :: (Monad m, Read t) => String -> m t
readM s = case reads s of
            [(v,"")] -> return v
            _        -> fail "readM: parse error"
            
-- |convert a 'ReadS a' result to 'Maybe a'
fromReadS :: [(a, String)] -> Maybe a
fromReadS [(n,[])] = Just n
fromReadS _        = Nothing
    
-- | This class is used by 'path' to parse a path component into a
-- value.
-- 
-- The instances for number types ('Int', 'Float', etc) use 'readM' to
-- parse the path component.
--
-- The instance for 'String', on the other hand, returns the
-- unmodified path component.
--
-- See the following section of the Happstack Crash Course for
-- detailed instructions using and extending 'FromReqURI':
--
--  <http://www.happstack.com/docs/crashcourse/RouteFilters.html#FromReqURI>

class FromReqURI a where
    fromReqURI :: String -> Maybe a

instance FromReqURI String  where fromReqURI = Just
instance FromReqURI Char    where fromReqURI s = case s of [c] -> Just c ; _ -> Nothing
instance FromReqURI Int     where fromReqURI = fromReadS . readDec
instance FromReqURI Int8    where fromReqURI = fromReadS . readDec
instance FromReqURI Int16   where fromReqURI = fromReadS . readDec                                  
instance FromReqURI Int32   where fromReqURI = fromReadS . readDec                                  
instance FromReqURI Int64   where fromReqURI = fromReadS . readDec                                  
instance FromReqURI Integer where fromReqURI = fromReadS . readDec
instance FromReqURI Word    where fromReqURI = fromReadS . readDec
instance FromReqURI Word8   where fromReqURI = fromReadS . readDec                                  
instance FromReqURI Word16  where fromReqURI = fromReadS . readDec                                  
instance FromReqURI Word32  where fromReqURI = fromReadS . readDec                                  
instance FromReqURI Word64  where fromReqURI = fromReadS . readDec                                  
instance FromReqURI Float   where fromReqURI = readM
instance FromReqURI Double  where fromReqURI = readM
instance FromReqURI Bool    where 
  fromReqURI s =
    let s' = map toLower s in
    case s' of
      "0"     -> Just False
      "false" -> Just False
      "1"     -> Just True
      "True"  -> Just True
      _       -> Nothing
