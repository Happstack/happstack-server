{-# LANGUAGE TypeSynonymInstances, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, RankNTypes #-}

module Happstack.Server.Internal.Types
    (Request(..), Response(..), RqBody(..), Input(..), HeaderPair(..),
     takeRequestBody, readInputsBody,
     rqURL, mkHeaders,
     getHeader, getHeaderBS, getHeaderUnsafe,
     hasHeader, hasHeaderBS, hasHeaderUnsafe,
     setHeader, setHeaderBS, setHeaderUnsafe,
     addHeader, addHeaderBS, addHeaderUnsafe,
     setRsCode, -- setCookie, setCookies,
     LogAccess, logMAccess, Conf(..), nullConf, result, resultBS,
     redirect, -- redirect_, redirect', redirect'_,
     isHTTP1_0, isHTTP1_1,
     RsFlags(..), nullRsFlags, contentLength, chunked, noContentLength,
     HttpVersion(..), Length(..), Method(..), canHaveBody, Headers, continueHTTP,
     Host, ContentType(..),
     readDec', fromReadS, readM, FromReqURI(..),
     showRsValidator, EscapeHTTP(..)
    ) where


import Control.DeepSeq (NFData)
import Control.Exception (Exception, SomeException)
import Control.Monad.Error (Error(strMsg))
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Control.Concurrent.Thread.Group as TG
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Data (Data)
import Data.String (fromString)
import Data.Time.Format (FormatTime(..))
import Data.Typeable(Typeable)
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString,pack)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8  as LU (fromString)
import Data.Int   (Int8, Int16, Int32, Int64)
import Data.Maybe
import Data.List
import Data.Word  (Word, Word8, Word16, Word32, Word64)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import GHC.Generics (Generic)
import Happstack.Server.SURI
import Data.Char (toLower)
import Happstack.Server.Internal.RFC822Headers ( ContentType(..) )
import Happstack.Server.Internal.Cookie
import Happstack.Server.Internal.LogFormat (formatRequestCombined)
import Happstack.Server.Internal.TimeoutIO (TimeoutIO)
import Numeric (readDec, readSigned)
import System.Log.Logger (Priority(..), logM)

-- | HTTP version
data HttpVersion = HttpVersion Int Int
             deriving(Read,Eq)

instance Show HttpVersion where
  show (HttpVersion x y) = (show x) ++ "." ++ (show y)

-- | 'True' if 'Request' is HTTP version @1.1@
isHTTP1_1 :: Request -> Bool
isHTTP1_1 rq =
    case rqVersion rq of
      HttpVersion 1 1 -> True
      _               -> False

-- | 'True' if 'Request' is HTTP version @1.0@
isHTTP1_0 :: Request -> Bool
isHTTP1_0 rq =
    case rqVersion rq of
      HttpVersion 1 0 -> True
      _               -> False

-- | Should the connection be used for further messages after this.
-- isHTTP1_0 && hasKeepAlive || isHTTP1_1 && hasNotConnectionClose
--
-- In addition to this rule All 1xx (informational), 204 (no content),
-- and 304 (not modified) responses MUST NOT include a message-body
-- and therefore are eligible for connection keep-alive.
continueHTTP :: Request -> Response -> Bool
continueHTTP rq rs =
    (isHTTP1_0 rq && checkHeaderBS connectionC keepaliveC rq   &&
       (rsfLength (rsFlags rs) == ContentLength || isNoMessageBodyResponse rs)) ||
    (isHTTP1_1 rq && not (checkHeaderBS connectionC closeC rq) &&
       (rsfLength (rsFlags rs) /= NoContentLength || isNoMessageBodyResponse rs))
  where
    isNoMessageBodyCode code = (code >= 100 && code <= 199) || code == 204 || code == 304
    isNoMessageBodyResponse rs' = isNoMessageBodyCode (rsCode rs') && L.null (rsBody rs')

-- | function to log access requests (see also: 'logMAccess')
-- type LogAccess time =
--    (   String  -- ^ host
--     -> String  -- ^ user
--     -> time    -- ^ time
--     -> String  -- ^ requestLine
--     -> Int     -- ^ responseCode
--     -> Integer -- ^ size
--     -> String  -- ^ referer
--     -> String  -- ^ userAgent
--     -> IO ())
type LogAccess time =
    (   String
     -> String
     -> time
     -> String
     -> Int
     -> Integer
     -> String
     -> String
     -> IO ())

-- | HTTP configuration
data Conf = Conf
    { port        :: Int             -- ^ Port for the server to listen on.
    , validator   :: Maybe (Response -> IO Response) -- ^ a function to validate the output on-the-fly
    , logAccess   :: forall t. FormatTime t => Maybe (LogAccess t) -- ^ function to log access requests (see also: 'logMAccess')
    , timeout     :: Int             -- ^ number of seconds to wait before killing an inactive thread
    , threadGroup :: Maybe TG.ThreadGroup -- ^ ThreadGroup for registering spawned threads for handling requests
    }

-- | Default configuration contains no validator and the port is set to 8000
nullConf :: Conf
nullConf =
    Conf { port        = 8000
         , validator   = Nothing
         , logAccess   = Just logMAccess
         , timeout     = 30
         , threadGroup = Nothing
         }

-- | log access requests using hslogger and apache-style log formatting
--
-- see also: 'Conf'
logMAccess :: forall t. FormatTime t => LogAccess t
logMAccess host user time requestLine responseCode size referer userAgent =
    logM "Happstack.Server.AccessLog.Combined" INFO $ formatRequestCombined host user time requestLine responseCode size referer userAgent

-- | HTTP request method
data Method = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT | PATCH | EXTENSION ByteString
    deriving (Show,Read,Eq,Ord,Typeable,Data)

-- | Does the method support a message body?
--
-- For extension methods, we assume yes.
canHaveBody :: Method
            -> Bool
canHaveBody POST          = True
canHaveBody PUT           = True
canHaveBody PATCH         = True
canHaveBody DELETE        = True
canHaveBody (EXTENSION _) = True
canHaveBody _             = False

-- | an HTTP header
data HeaderPair = HeaderPair
    { hName :: ByteString     -- ^ header name
    , hValue :: [ByteString]  -- ^ header value (or values if multiple occurances of the header are present)
    }
    deriving (Read,Show,Generic,NFData)

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
      deriving (Eq, Ord, Read, Show, Enum, Generic, NFData)

-- | Result flags
data RsFlags = RsFlags
    { rsfLength :: Length
    } deriving (Show,Read,Typeable,Generic,NFData)

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
    } deriving (Show, Read, Typeable)

-- | hostname & port
type Host = (String, Int) -- ^ (hostname, port)

-- | an HTTP Response
data Response
    = Response  { rsCode      :: Int
                , rsHeaders   :: Headers
                , rsFlags     :: RsFlags
                , rsBody      :: L.ByteString
                , rsValidator :: Maybe (Response -> IO Response)
                }
    | SendFile  { rsCode      :: Int
                , rsHeaders   :: Headers
                , rsFlags     :: RsFlags
                , rsValidator :: Maybe (Response -> IO Response)
                , sfFilePath  :: FilePath  -- ^ file handle to send from
                , sfOffset    :: Integer   -- ^ offset to start at
                , sfCount     :: Integer    -- ^ number of bytes to send
                }
      deriving (Generic, NFData, Typeable)

instance Show Response where
    showsPrec _ res@Response{}  =
        showString   "================== Response ================"                    .
        showString "\nrsCode      = " . shows      (rsCode res)                        .
        showString "\nrsHeaders   = " . shows      (rsHeaders res)                     .
        showString "\nrsFlags     = " . shows      (rsFlags res)                       .
        showString "\nrsBody      = " . shows      (rsBody res)                        .
        showString "\nrsValidator = " . shows      (showRsValidator (rsValidator res))
    showsPrec _ res@SendFile{}  =
        showString   "================== Response ================"                    .
        showString "\nrsCode      = " . shows      (rsCode res)                        .
        showString "\nrsHeaders   = " . shows      (rsHeaders res)                     .
        showString "\nrsFlags     = " . shows      (rsFlags res)                       .
        showString "\nrsValidator = " . shows      (showRsValidator (rsValidator res)) .
        showString "\nsfFilePath  = " . shows      (sfFilePath res)                    .
        showString "\nsfOffset    = " . shows      (sfOffset res)                      .
        showString "\nsfCount     = " . shows      (sfCount res)

showRsValidator :: Maybe (Response -> IO Response) -> String
showRsValidator = maybe "Nothing" (const "Just <function>")

-- what should the status code be ?
instance Error Response where
  strMsg str =
      setHeader "Content-Type" "text/plain; charset=UTF-8" $
       result 500 str

-- | an HTTP request
data Request = Request
    { rqSecure      :: Bool                  -- ^ request uses https:\/\/
    , rqMethod      :: Method                -- ^ request method
    , rqPaths       :: [String]              -- ^ the uri, split on /, and then decoded
    , rqUri         :: String                -- ^ the raw rqUri
    , rqQuery       :: String                -- ^ the QUERY_STRING
    , rqInputsQuery :: [(String,Input)]      -- ^ the QUERY_STRING decoded as key/value pairs
    , rqInputsBody  :: MVar [(String,Input)] -- ^ the request body decoded as key/value pairs (when appropriate)
    , rqCookies     :: [(String,Cookie)]     -- ^ cookies
    , rqVersion     :: HttpVersion           -- ^ HTTP version
    , rqHeaders     :: Headers               -- ^ the HTTP request headers
    , rqBody        :: MVar RqBody           -- ^ the raw, undecoded request body
    , rqPeer        :: Host                  -- ^ (hostname, port) of the client making the request
    } deriving (Typeable)

instance Show Request where
    showsPrec _ rq =
        showString   "================== Request =================" .
        showString "\nrqSecure      = " . shows      (rqSecure rq) .
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

-- | Converts a Request into a String representing the corresponding URL
rqURL :: Request -> String
rqURL rq = '/':intercalate "/" (rqPaths rq) ++ (rqQuery rq)

-- | a class for working with types that contain HTTP headers
class HasHeaders a where
    updateHeaders :: (Headers->Headers) -> a -> a -- ^ modify the headers
    headers       :: a -> Headers                 -- ^ extract the headers

instance HasHeaders Response where
    updateHeaders f rs = rs {rsHeaders=f $ rsHeaders rs }
    headers            = rsHeaders

instance HasHeaders Request where
    updateHeaders f rq = rq {rqHeaders = f $ rqHeaders rq }
    headers            = rqHeaders

instance HasHeaders Headers where
    updateHeaders f = f
    headers         = id

-- | The body of an HTTP 'Request'
newtype RqBody = Body { unBody :: L.ByteString } deriving (Read,Show,Typeable)

-- | Sets the Response status code to the provided Int and lifts the computation
-- into a Monad.
setRsCode :: (Monad m) => Int -> Response -> m Response
setRsCode code rs = return rs { rsCode = code }

-- | Takes a list of (key,val) pairs and converts it into Headers.  The
-- keys will be converted to lowercase
mkHeaders :: [(String,String)] -> Headers
mkHeaders hdrs
    = M.fromListWith join [ (P.pack (map toLower key), HeaderPair (P.pack key) [P.pack value]) | (key,value) <- hdrs ]
    where join (HeaderPair key vs1) (HeaderPair _ vs2) = HeaderPair key (vs2++vs1)

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
    where join (HeaderPair k vs1) (HeaderPair _ vs2) = HeaderPair k (vs2++vs1)

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

readDec' :: (Num a, Eq a) => String -> a
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
instance FromReqURI Text.Text where fromReqURI = fmap fromString . fromReqURI
instance FromReqURI Lazy.Text where fromReqURI = fmap fromString . fromReqURI
instance FromReqURI Char    where fromReqURI s = case s of [c] -> Just c ; _ -> Nothing
instance FromReqURI Int     where fromReqURI = fromReadS . readSigned readDec
instance FromReqURI Int8    where fromReqURI = fromReadS . readSigned readDec
instance FromReqURI Int16   where fromReqURI = fromReadS . readSigned readDec
instance FromReqURI Int32   where fromReqURI = fromReadS . readSigned readDec
instance FromReqURI Int64   where fromReqURI = fromReadS . readSigned readDec
instance FromReqURI Integer where fromReqURI = fromReadS . readSigned readDec
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
      "true"  -> Just True
      _       -> Nothing

------------------------------------------------------------------------------
-- EscapeHTTP - escape hatched use by websockets
------------------------------------------------------------------------------

-- | Escape from the HTTP world and get direct access to the underlying 'TimeoutIO' functions
data EscapeHTTP
  = EscapeHTTP (TimeoutIO -> IO ())
    deriving (Typeable)

instance Exception EscapeHTTP

instance Show EscapeHTTP where
  show (EscapeHTTP {})         = "<EscapeHTTP _>"
