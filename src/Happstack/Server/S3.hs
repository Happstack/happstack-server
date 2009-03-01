{-# LANGUAGE PatternGuards #-}
module Happstack.Server.S3
    ( newS3        -- :: AccessKey -> SecretKey -> URI -> IO S3
    , closeS3      -- :: S3 -> IO ()
    , createBucket -- :: S3 -> BucketId -> IO ()
    , createObject -- :: S3 -> BucketId -> ObjectId -> String -> IO ()
    , getObject    -- :: S3 -> BucketId -> ObjectId -> IO String
    , deleteBucket -- :: S3 -> BucketId -> IO ()
    , deleteObject -- :: S3 -> BucketId -> ObjectId -> IO ()
    , listObjects  -- :: S3 -> BucketId -> IO [String]
    , sendRequest  -- :: S3 -> Request -> IO String
    , sendRequest_ -- :: S3 -> Request -> IO ()
--    , sendRequests -- :: S3 -> [Request] -> IO ()
    , BucketId, ObjectId, AccessKey, SecretKey
    , amazonURI
    ) where

import Happstack.Crypto.HMAC             ( hmacSHA1 )
import Happstack.Server.HTTPClient.HTTP
import qualified Happstack.Server.HTTPClient.Stream as Stream

import Network.URI hiding (path)
import Control.Concurrent               ( newMVar, modifyMVar, swapMVar
                                        , modifyMVar_, MVar )
import Data.Maybe                       ( fromJust, fromMaybe )
import Data.List                        ( intersperse )
import System.Time                      ( getClockTime, toCalendarTime
                                        , formatCalendarTime )
import System.Locale                    ( defaultTimeLocale, rfc822DateFormat )

import Text.XML.HaXml                   ( xmlParse, Document(..), Content(..) )
import Text.XML.HaXml.Xtract.Parse      ( xtract )

type BucketId = String
type ObjectId = String
type AccessKey = String
type SecretKey = String

data S3
    = S3
    { s3AccessKey        :: AccessKey
    , s3SecretKey        :: SecretKey
    , s3URI              :: URI
--    , s3KeepAliveTimeout :: Int
    , s3Conn             :: MVar (Maybe Connection)
    }

{- |
  Sign a request using the access key and secret key from the S3 data
  type.
-}
signRequest :: S3 -> Request -> IO Request
signRequest s3
    = let akey = s3AccessKey s3
          skey = s3SecretKey s3
      in signRequest' akey skey

{- |
  Fill in necessary information (such as a date header) and then sign
  then request.
-}
signRequest' :: AccessKey -> SecretKey -> Request -> IO Request
signRequest' akey skey request
    = do now <- getClockTime
         cal <- toCalendarTime now
         let isoDate = formatCalendarTime defaultTimeLocale rfc822DateFormat cal
             auth = fromJust (uriAuthority (rqURI request))
--             authErr = error "S3.hs: internal error: failed to parse authority"
         let dat = concat $ intersperse "\n"
                   [show (rqMethod request)
                   ,lookupHeader HdrContentMD5
                   ,lookupHeader HdrContentType
                   ,isoDate
                   ,uriPath (rqURI request)]
             authorization = Header HdrAuthorization $ "AWS " ++ akey ++ ":" ++ signature
             signature = hmacSHA1 skey dat
             lookupHeader hn = fromMaybe "" (findHeader hn request)
             dateHdr = Header HdrDate isoDate
             lengthHdr = Header HdrContentLength (show $ length (rqBody request))
             connHdr = Header HdrConnection "Keep-Alive"
             hostHdr = Header HdrHost (uriRegName auth)
         return $ request
                    { rqHeaders = hostHdr:connHdr:lengthHdr:dateHdr:
                                  authorization:rqHeaders request
                    , rqURI = (rqURI request) { uriScheme = ""
                                              , uriAuthority = Nothing}}

{- |
  Return a connection to an S3 server. Will initiate a new
  connection if no previous was found.
-}
getConnection :: S3 -> IO Connection
getConnection s3
    = modifyMVar (s3Conn s3) $ \mbConn ->
      case mbConn of
        Just conn -> return (mbConn,conn)
        Nothing -> do print (uriRegName auth, uriPort auth)
                      c <- openTCPPort (uriRegName auth) (if null $ uriPort auth then 80 else read$ uriPort auth)
                      return (Just c,c)
    where auth = fromJust (uriAuthority (s3URI s3))

createRequest :: S3 -> RequestMethod -> String -> String -> Request
createRequest _s3 method path body
    = Request uri method [] body
    where uri = localhost { uriPath = '/':escapeURIString isAllowedInURI path }

{- |
  Send a single request to an S3 server returning the body
  of the result.
-}
sendRequest :: S3 -> Request -> IO String
sendRequest s3 request
    = loop =<< signRequest s3 request
    where loop request'
              = do c <- getConnection s3
                   ret <- sendHTTP c request'
                   case ret of
                     Left ErrorClosed
                         -> do putStrLn "Connection closed."
                               swapMVar (s3Conn s3) Nothing
                               loop request'
                     Left err  -> error ("Failed to connect: " ++ show err) -- FIXME
                     Right res
                         | (2,_,_) <- rspCode res -> return (rspBody res)
                         | otherwise -> error ("Server error: " ++ rspReason res)

{- |
  Same as 'sendRequest' except that it ignored the result.
-}
sendRequest_ :: S3 -> Request -> IO ()
sendRequest_ s3 request
    = do sendRequest s3 request
         return ()

{-
  Sign and send requests pipelined over a keep-alive connection.
-}
{-
  S3 imposes a quite severe limitation on pipelined requests.
  Sending too many requests or exceeding a size limit will
  result in a disconnect. The precise borders of these limits
  are hard-wired and unknown to the general public. At the time
  of this writing, sending three requests at a time seems optimal.

  Quote from Amazons web-forum:
  (http://developer.amazonwebservices.com/connect/thread.jspa?messageID=39883)
  "OK, we have located the cause of the behavior you are seeing.
   Your pipelined requests are being aborted because one of the
   network devices handling the connection has certain limitations
   on the amount of pipelined data it is willing to accept per
   connection, and that limit is being exceeded. Unfortunately, this
   limit is phrased in very low-level terms so it isn't possible to
   say in a platform or network independent way whether any given
   size, sequence or number of requests will exceed the limit and
   cause a disconnection or not. We have engaged with the device
   vendor and found out that this limit is hard-wired and that this
   behavior is not likely to change any time soon.

   In light of these facts, here is some guidance on pipelining HTTP
   requests to Amazon S3.
    1) Be optimistic and pipeline a modest number of GET or HEAD
       requests, say two to four.
    2) Handle asynchronous disconnects by re-connecting and re-sending
       unacknowledged requests left in your pipeline. (As Colin points
       out, correct HTTP clients must do this anyway)
    3) If possible, try to minimize the number of TCP segments your
       pipelined requests generate. In particular, leave the TCP socket
       no delay option off and send as many requests per socket write call
       as is practical."
-}

--------------------------------------------------------------
-- Initiate
--------------------------------------------------------------

newS3 :: AccessKey -> SecretKey -> URI -> IO S3
newS3 akey skey uri
    = do conn <- newMVar Nothing
         return $ S3 { s3AccessKey = akey
                     , s3SecretKey = skey
                     , s3URI       = uri
--                     , s3KeepAliveTimeout :: Int
                     , s3Conn      = conn
                     }

closeS3 :: S3 -> IO ()
closeS3 s3
    = modifyMVar_ (s3Conn s3) $ \mbConn ->
      case mbConn of
        Nothing -> return Nothing
        Just conn -> do Stream.close conn
                        return Nothing


--------------------------------------------------------------
-- Requests
--------------------------------------------------------------


createBucket :: S3 -> BucketId -> Request
createBucket s3 bucket
    = createRequest s3 PUT bucket ""

createObject :: S3 -> BucketId -> ObjectId -> String -> Request
createObject s3 bucket object
    = createRequest s3 PUT (bucket ++ "/" ++ object)

getObject :: S3 -> BucketId -> ObjectId -> Request
getObject s3 bucket object
    = createRequest s3 GET (bucket ++ "/" ++ object) ""

deleteBucket :: S3 -> BucketId -> Request
deleteBucket s3 bucket
    = createRequest s3 DELETE bucket ""

deleteObject :: S3 -> BucketId -> ObjectId -> Request
deleteObject s3 bucket object
    = createRequest s3 DELETE (bucket ++ "/" ++ object) ""

--------------------------------------------------------------
-- Actions
--------------------------------------------------------------


listObjects :: S3 -> BucketId -> IO [String]
listObjects s3 bucket
    = do lst <- sendRequest s3 (createRequest s3 GET bucket "")
         return $ ppContent . auxFilter . getContent . xmlParse bucket $ lst
    where auxFilter = xtract "*/Key/-"
          getContent (Document _ _ e _) = CElem e

          ppContent xs  = [ s | CString _ s <- xs ]



amazonURI :: URI
amazonURI = fromJust $ parseURI "http://s3.amazonaws.com/"
localhost :: URI
localhost = fromJust $ parseURI "http://localhost/"

