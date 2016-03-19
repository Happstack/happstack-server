{-# LANGUAGE FlexibleContexts #-}
module Happstack.Server.RangeTest (rangeTests) where

import Control.Monad (MonadPlus(..), msum)
import Control.Monad.Trans (MonadIO(..))
import Control.Concurrent.MVar
import Data.CaseInsensitive (original)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as CLazy
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Happstack.Server hiding (path)
-- import Happstack.Server.Internal.Multipart (queryInput)
import Happstack.Server.Internal.MessageWrap (pathEls, queryInput)
import Happstack.Server.Internal.Multipart as Happstack (BodyPart(..), parseMultipartBody)
import Happstack.Server.Internal.Types as Happstack (SendFile(..), getHeaderUnsafe)
import Happstack.Server.Range (withRange)
import Happstack.Server.SURI(ToSURI(..), path, query)
-- import Happstack.Server.Internal.Range
import Network.HTTP.Types (ByteRanges, ByteRange(..), hRange, hIfRange, hContentType, renderByteRange, renderByteRanges, parseByteRanges)
import Network.HTTP.Types.Header (hAcceptRanges, hContentRange, hContentLength)
-- import Network.Multipart as MultiPart (showMultipartBody, BodyPart(..), MultiPart(..), HeaderName(..))

import Test.Hspec

main :: IO ()
main = server

server :: IO ()
server = simpleHTTP nullConf (withRange rangePart)

mkRequest :: Method -> String -> [(String, Cookie)] -> Headers -> Lazy.ByteString -> IO Request
mkRequest method uri cookies headers body =
    do let u = toSURI uri
       ib <- newEmptyMVar
       b  <- newMVar (Body body)
       return $ Request { rqMethod      = method
                        , rqPaths       = (pathEls (path u))
                        , rqUri         = (path u)
                        , rqQuery       = (query u)
                        , rqInputsQuery = (queryInput u)
                        , rqInputsBody  = ib
                        , rqCookies     = cookies
                        , rqVersion     = HttpVersion 1 1
                        , rqHeaders     = headers
                        , rqBody        = b
                        , rqPeer        = ("",0)
                        , rqSecure      = False
                        }

-- rangeTests :: IO ()
rangeTests :: Spec
rangeTests =
  describe "RFC7233" $ do
   describe "2.1 Byte Ranges" $ do
    describe "byte-range parsing" $ do
      it "The first 500 bytes (byte offsets 0-499, inclusive)" $ do
         parseByteRanges (C.pack "bytes=0-499") == Just [ByteRangeFromTo 0 499]
      it "The second 500 bytes (byte offsets 500-999, inclusive)" $ do
         parseByteRanges (C.pack "bytes=500-999") == Just [ByteRangeFromTo 500 999]
      it "The final 500 bytes (byte offsets 9500-9999, inclusive) via -500" $ do
         parseByteRanges (C.pack "bytes=-500") == Just [ByteRangeSuffix 500]
      it "The final 500 bytes (byte offsets 9500-9999, inclusive) via 9500-" $ do
         parseByteRanges (C.pack "bytes=9500-") == Just [ByteRangeFrom 9500]
      it "The first and last bytes only (bytes 0 and 9999) via bytes=0-0,-1" $ do
         parseByteRanges (C.pack "bytes=0-0,-1") == Just [ByteRangeFromTo 0 0, ByteRangeSuffix 1]
      it "bytes=500-600,601-999" $ do
         parseByteRanges (C.pack "bytes=500-600,601-999") == Just [ByteRangeFromTo 500 600, ByteRangeFromTo 601 999]
      it "bytes=500-700,601-999" $ do
         parseByteRanges (C.pack "bytes=500-700,601-999") == Just [ByteRangeFromTo 500 700, ByteRangeFromTo 601 999]
   describe "3.1 Range" $ do
    it "Ignore Range when 304 (Not Modified)" $ do
     res <- fakeServer GET "/" [] Map.empty Lazy.empty (notModified (toResponse ()))
     pure (rsCode res) `shouldReturn` (304 :: Int)
   describe "3.2 If-Range" $ do
    it "Implement If-Range tests." $
     False
   describe "4.1 206 (Partial Content)" $ do
    describe "Lazy ByteString Responses" $ do
     describe "Range: 5-8 bytes" $ do
      let brs = [ByteRangeFromTo 5 8]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
       it "Response has Content-Range" $ \res -> do
        (getHeader "Content-Range" res) `shouldBe` (Just (C.pack "bytes 5-8/10"))
       it "Response has Content-Length: 4 header" $ \res -> do
        (getHeader "Content-Length" res) `shouldBe` (Just (C.pack "4"))
       it "Response has correct length body" $ \res -> do
        rsBody res `shouldBe` (CLazy.pack "5678")
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
     describe "Range: -5 bytes" $ do
      let brs = [ByteRangeSuffix 5]
      before (fakeServer GET "/" [] (addHeaderBS (original hRange) (renderByteRanges brs) Map.empty)
              Lazy.empty (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
       it "Response has Content-Range" $ \res -> do
        (getHeader "Content-Range" res) `shouldBe` (Just (C.pack "bytes 5-9/10"))
       it "Response has Content-Length: 5 header" $ \res -> do
        (getHeader "Content-Length" res) `shouldBe` (Just (C.pack "5"))
       it "Response has ranged body" $ \res -> do
        rsBody res `shouldBe` (CLazy.pack "56789")
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
     describe "Range: 5- bytes" $ do
      let brs = [ByteRangeFrom 5]
      before (fakeServer GET "/" [] (addHeaderBS (original hRange) (renderByteRanges brs) Map.empty)
              Lazy.empty (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
       it "Response has Content-Range" $ \res -> do
        (getHeader "Content-Range" res) `shouldBe` (Just (C.pack "bytes 5-9/10"))
       it "Response has Content-Length: 5 header" $ \res -> do
        (getHeader "Content-Length" res) `shouldBe` (Just (C.pack "5"))
       it "Response has ranged body" $ \res -> do
        rsBody res `shouldBe` (CLazy.pack "56789")
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
    describe "SendFile Responses" $ do
     describe "Range: 5-8 bytes" $ do
      let brs = [ByteRangeFromTo 5 8]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (serveFile (asContentType "text/plain") "tests/digits")) $ do
       it "Response has Content-Range" $ \res -> do
        (getHeader "Content-Range" res) `shouldBe` (Just (C.pack "bytes 5-8/10"))
       it "Response has Content-Length: 4 header" $ \res -> do
        (getHeader "Content-Length" res) `shouldBe` (Just (C.pack "4"))
       it "Response has correct length body" $ \res -> do
        sfSendFile res `shouldBe` (SinglePart "tests/digits" 5 4)
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
     describe "Range: -5 bytes" $ do
      let brs = [ByteRangeSuffix 5]
      before (fakeServer GET "/" [] (addHeaderBS (original hRange) (renderByteRanges brs) Map.empty) Lazy.empty
               (serveFile (asContentType "text/plain") "tests/digits")) $ do
       it "Response has Content-Range" $ \res -> do
        (getHeader "Content-Range" res) `shouldBe` (Just (C.pack "bytes 5-9/10"))
       it "Response has Content-Length: 5 header" $ \res -> do
        (getHeader "Content-Length" res) `shouldBe` (Just (C.pack "5"))
       it "Response has ranged body" $ \res -> do
        sfSendFile res `shouldBe` (SinglePart "tests/digits" 5 5)
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
     describe "Range: 5- bytes" $ do
      let brs = [ByteRangeFrom 5]
      before (fakeServer GET "/" [] (addHeaderBS (original hRange) (renderByteRanges brs) Map.empty) Lazy.empty
               (serveFile (asContentType "text/plain") "tests/digits")) $ do
       it "Response has Content-Range" $ \res -> do
        (getHeader "Content-Range" res) `shouldBe` (Just (C.pack "bytes 5-9/10"))
       it "Response has Content-Length: 5 header" $ \res -> do
        (getHeader "Content-Length" res) `shouldBe` (Just (C.pack "5"))
       it "Response has ranged body" $ \res -> do
        sfSendFile res `shouldBe` (SinglePart "tests/digits" 5 5)
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
    describe "Lazy ByteString Multipart Responses" $ do
     describe "Range: bytes=1-3,5-8" $ do
      let brs = [ByteRangeFromTo 1 3, ByteRangeFromTo 5 8]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
       it "Response has Content-Type: multipart/byteranges;" $ \res -> do
        (getHeader "Content-Type" res) `shouldBe` (Just (C.pack "multipart/byteranges; boundary=multipart_byteranges_boundary_XXX"))
       it "Response has correct body" $ \res -> do
         parseMultipartBody (CLazy.pack "multipart_byteranges_boundary_XXX") (rsBody res) `shouldSatisfy`
           (\parts -> case parts of
                        ([Happstack.BodyPart h1 b1, Happstack.BodyPart h2 b2], _) -> (b1 == CLazy.pack "123") && (b2 == CLazy.pack "5678")
                        _ -> False)
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
     describe "Range: bytes=1-3,-4" $ do
      let brs = [ByteRangeFromTo 1 3, ByteRangeSuffix 4]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
       it "Response has Content-Type: multipart/byteranges;" $ \res -> do
        (getHeader "Content-Type" res) `shouldBe` (Just (C.pack "multipart/byteranges; boundary=multipart_byteranges_boundary_XXX"))
       it "Response has correct body" $ \res -> do
         parseMultipartBody (CLazy.pack "multipart_byteranges_boundary_XXX") (rsBody res) `shouldSatisfy`
           (\parts -> case parts of
                        ([Happstack.BodyPart h1 b1, Happstack.BodyPart h2 b2], _) -> (b1 == CLazy.pack "123") && (b2 == CLazy.pack "6789")
                        _ -> False)
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
     describe "Range: bytes=1-3,6-" $ do
      let brs = [ByteRangeFromTo 1 3, ByteRangeSuffix 4]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
       it "Response has Content-Type: multipart/byteranges;" $ \res -> do
        (getHeader "Content-Type" res) `shouldBe` (Just (C.pack "multipart/byteranges; boundary=multipart_byteranges_boundary_XXX"))
       it "Response has correct body" $ \res -> do
         parseMultipartBody (CLazy.pack "multipart_byteranges_boundary_XXX") (rsBody res) `shouldSatisfy`
           (\parts -> case parts of
                        ([Happstack.BodyPart h1 b1, Happstack.BodyPart h2 b2], _) -> (b1 == CLazy.pack "123") && (b2 == CLazy.pack "6789")
                        _ -> False)
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
     -- a weak attempt to check that laziness is working
     describe "Range: bytes=1-3,5-8" $ do
      let brs = [ByteRangeFromTo 1 3, ByteRangeFromTo 5 8]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (ok $ addHeaderBS (original hContentLength) (C.pack "10000000000") $ toResponse (take 10000000000 $ cycle "0123456789"))) $ do
       it "Response has Content-Type: multipart/byteranges;" $ \res -> do
        (getHeader "Content-Type" res) `shouldBe` (Just (C.pack "multipart/byteranges; boundary=multipart_byteranges_boundary_XXX"))
       it "Response has correct body" $ \res -> do
         parseMultipartBody (CLazy.pack "multipart_byteranges_boundary_XXX") (rsBody res) `shouldSatisfy`
           (\parts -> case parts of
                        ([Happstack.BodyPart h1 b1, Happstack.BodyPart h2 b2], _) -> (b1 == CLazy.pack "123") && (b2 == CLazy.pack "5678")
                        _ -> False)
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
     {- This test exposes a flaw in withRange/Happstack. Even though withRange aims to allow the garbage collector to collect the unused parts of the response
        body -- something is holding a reference to the Response and so we end up forcing the entire Response into memory.
     -}
     describe "Range: bytes=1-3,5-8" $ do
      let brs = [ByteRangeFromTo 1 3, ByteRangeFromTo 1000000000 1000000001 ]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (ok $ addHeaderBS (original hContentLength) (C.pack "10000000000") $ toResponse (take 10000000000 $ cycle "0123456789"))) $ do
       it "Response has Content-Type: multipart/byteranges;" $ \res -> do
        (getHeader "Content-Type" res) `shouldBe` (Just (C.pack "multipart/byteranges; boundary=multipart_byteranges_boundary_XXX"))
       it "Response has correct body" $ \res -> do
         parseMultipartBody (CLazy.pack "multipart_byteranges_boundary_XXX") (rsBody res) `shouldSatisfy`
           (\parts -> case parts of
                        ([Happstack.BodyPart h1 b1, Happstack.BodyPart h2 b2], _) -> (b1 == CLazy.pack "123") && (b2 == CLazy.pack "5678")
                        _ -> False)
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
    -}
    describe "SendFile Multipart Responses" $ do
     describe "Range: bytes=1-3,5-8" $ do
      let brs = [ByteRangeFromTo 1 3, ByteRangeFromTo 5 8]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (serveFile (asContentType "text/plain") "tests/digits")) $ do
       it "Response has Content-Type: multipart/byteranges;" $ \res -> do
        (getHeader "Content-Type" res) `shouldBe` (Just (C.pack "multipart/byteranges; boundary=multipart_byteranges_boundary_XXX"))
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
       it "Response has correct body" $ \res -> do
        sfSendFile res `shouldBe` (Happstack.MultiPart "tests/digits" 10 (C.pack "text/plain") (C.pack "multipart_byteranges_boundary_XXX") [(1,3), (5,8)])

     describe "Range: bytes=1-3,-4" $ do
      let brs = [ByteRangeFromTo 1 3, ByteRangeSuffix 4]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (serveFile (asContentType "text/plain") "tests/digits")) $ do
       it "Response has Content-Type: multipart/byteranges;" $ \res -> do
        (getHeader "Content-Type" res) `shouldBe` (Just (C.pack "multipart/byteranges; boundary=multipart_byteranges_boundary_XXX"))
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
       it "Response has correct body" $ \res -> do
        sfSendFile res `shouldBe` (Happstack.MultiPart "tests/digits" 10 (C.pack "text/plain") (C.pack "multipart_byteranges_boundary_XXX") [(1,3), (6,9)])

     describe "Range: bytes=1-3,6-" $ do
      let brs = [ByteRangeFromTo 1 3, ByteRangeSuffix 4]
      before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
               (serveFile (asContentType "text/plain") "tests/digits")) $ do
       it "Response has Content-Type: multipart/byteranges;" $ \res -> do
        (getHeader "Content-Type" res) `shouldBe` (Just (C.pack "multipart/byteranges; boundary=multipart_byteranges_boundary_XXX"))
       it "Response has response code 206" $ \res -> do
        rsCode res `shouldBe` 206
       it "Response has correct body" $ \res -> do
        sfSendFile res `shouldBe` (Happstack.MultiPart "tests/digits" 10 (C.pack "text/plain") (C.pack "multipart_byteranges_boundary_XXX") [(1,3), (6,9)])

    describe "4.1 416 (Range Not Satisfiable)" $ do -- TODO: check that Content-Range is set on 416 Responses
      describe "Ranges out of order" $ do
        let brs = [ByteRangeFromTo 5 8, ByteRangeFromTo 1 4]
        before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
             (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
         it  "Response code should be 416" $ \res -> do
           rsCode res `shouldBe` 416
         it  "Message body should be empty" $ \res -> do
           rsBody res `shouldBe` CLazy.empty
      describe "ByteRanges after ByteRangeSuffix disallowed" $ do
        let brs = [ByteRangeSuffix 1, ByteRangeFromTo 1 4]
        before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
             (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
         it  "Response code should be 416" $ \res -> do
           rsCode res `shouldBe` 416
         it  "Message body should be empty" $ \res -> do
           rsBody res `shouldBe` CLazy.empty
      describe "ByteRanges after ByteRangeFrom disallowed" $ do
        let brs = [ByteRangeFrom 2, ByteRangeFromTo 5 6]
        before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
                  (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
         it  "Response code should be 416" $ \res -> do
           rsCode res `shouldBe` 416
         it  "Message body should be empty" $ \res -> do
           rsBody res `shouldBe` CLazy.empty
      describe "Overlapping ByteRanges disallowed" $ do
        let brs = [ByteRangeFromTo 2 4, ByteRangeFromTo 3 5]
        before (fakeServer GET "/" [] (addHeaderBS (C.pack "Range") (renderByteRanges brs) Map.empty) Lazy.empty
                  (ok $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse "0123456789")) $ do
         it  "Response code should be 416" $ \res -> do
           rsCode res `shouldBe` 416
         it  "Message body should be empty" $ \res -> do
           rsBody res `shouldBe` CLazy.empty

fakeServer :: Method              -- ^ request method (GET, POST, etc)
           -> String              -- ^ uri
           -> [(String, Cookie)]  -- ^ cookies
           -> Headers             -- ^ headers
           -> Lazy.ByteString     -- ^ body
           -> ServerPart Response -- ^ request handler
           -> IO Response
fakeServer method url cookies headers body handler =
    do req <- mkRequest method url cookies headers body
       simpleHTTP'' (withRange handler) req


-- how does compression and range interact?
rangePart :: ServerPart Response
rangePart =
    do req <- askRq
       liftIO $ print req
       msum [ nullDir >> (ok $ contentLength $ addHeaderBS (C.pack "Date") (C.pack "Wed, 18 Nov 2015 03:09:23 GMT")
                              $ addHeaderBS (original hContentLength) (C.pack "10") $ toResponse $ "0123456789")
            , dir "sendfile" $ serveFile (asContentType "text/plain") "tests/digits"
            ]
{-

{-
    do mh <- getHeaderBS (original hRange) <$> askRq
       let mbr = case mh of
                   Nothing -> Nothing
                   (Just br) ->
                       parseByteRanges br
       liftIO $ print mbr
       case mbr of
         (Just [br]) ->
             ok $ toResponse $ WithByteRange br (show [1..100])
         Nothing ->
             ok $ toResponse $ (show [1..100])
-}
data WithByteRange a = WithByteRange ByteRange a

instance ToMessage a => ToMessage (WithByteRange a) where
    toContentType (WithByteRange _ a) = toContentType a
    toMessage (WithByteRange br a) =
        case br of
          (ByteRangeFromTo f t) -> Lazy.take (fromInteger (1 + (t - f))) $ Lazy.drop (fromInteger f) $ toMessage a

{-
TODO:

A server that supports range requests MAY ignore or reject a Range
   header field that consists of more than two overlapping ranges, or a
   set of many small ranges that are not listed in ascending order,
   since both are indications of either a broken client or a deliberate
   denial-of-service attack (Section 6.1).  A client SHOULD NOT request
   multiple ranges that are inherently less efficient to process and
   transfer than a single range that encompasses the same data.

---

 The Range header field is evaluated after evaluating the precondition
   header fields defined in [RFC7232], and only if the result in absence
   of the Range header field would be a 200 (OK) response.  In other
   words, Range is ignored when a conditional GET would result in a 304
   (Not Modified) response. [1]

---

 If all of the preconditions are true, the server supports the Range
   header field for the target resource, and the specified range(s) are
   valid and satisfiable (as defined in Section 2.1), the server SHOULD
   send a 206 (Partial Content) response with a payload containing one
   or more partial representations that correspond to the satisfiable
   ranges requested, as defined in Section 4.

---

 If all of the preconditions are true, the server supports the Range
   header field for the target resource, and the specified range(s) are
   invalid or unsatisfiable, the server SHOULD send a 416 (Range Not
   Satisfiable) response.

---

 If-Range = entity-tag / HTTP-date

---

If a single part is being transferred, the server generating the 206
   response MUST generate a Content-Range header field, describing what
   range of the selected representation is enclosed, and a payload
   consisting of the range.  For example:

     HTTP/1.1 206 Partial Content
     Date: Wed, 15 Nov 1995 06:25:24 GMT
     Last-Modified: Wed, 15 Nov 1995 04:58:08 GMT
     Content-Range: bytes 21010-47021/47022
     Content-Length: 26012
     Content-Type: image/gif

     ... 26012 bytes of partial image data ...

---

 If multiple parts are being transferred, the server generating the
   206 response MUST generate a "multipart/byteranges" payload, as
   defined in Appendix A, and a Content-Type header field containing the
   multipart/byteranges media type and its required boundary parameter.
   To avoid confusion with single-part responses, a server MUST NOT
   generate a Content-Range header field in the HTTP header section of a
   multiple part response (this field will be sent in each part
   instead).

---

When a 206 response is generated, the server MUST generate the
   following header fields, in addition to those required above, if the
   field would have been sent in a 200 (OK) response to the same
   request: Date, Cache-Control, ETag, Expires, Content-Location, and
   Vary.

-}
withRange :: (FilterMonad Response m, MonadPlus m, WebMonad Response m, ServerMonad m) =>
               m Response -> m Response
withRange part =
    do composeFilter $ \res -> addHeaderBS (original hAcceptRanges) (C.pack "bytes") res
       response <- part
       req <- askRq
       case getHeaderBS (original hRange) req of
         Nothing -> pure response -- no Range header in the Request, so we are done
         (Just rangeBS) ->
             case () of
               -- only pay attention to Range header for a GET/HEAD Request
               () | not ((rqMethod req == GET) || (rqMethod req /= HEAD)) ->
                      pure response
                  | otherwise ->
                       case parseByteRanges rangeBS of
                         Nothing -> pure response -- FIXME: or error?
                         (Just brs) -> do
                            composeFilter $ \response ->
                                case rsCode response of
                                   200 -> -- would have return 200 (OK) otherwise
                                     case brs of
                                       [br] -> do -- only one byte range
                                         case response of
                                             Response {} ->
                                               case (fmap fst (C.readInteger =<< getHeaderUnsafe (C.pack "content-length") (rsHeaders response))) of
                                                 Nothing -> response -- does not seem like we can return a ranged response on content of unknown length
                                                       -- The Content-Range response header we are supposed to add requires a start and end byte?
                                                 (Just contentLength) ->
                                                   case br of -- FIXME: perhaps use mContentLen when it is available?
                                                     (ByteRangeFromTo f t) -> -- ^ single ByteRange
                                                       setHeaderBS (original hContentLength) (C.pack $ show (1 + t - f)) $
                                                        setHeaderBS (original hContentRange) (renderByteRanges [br] <> (C.pack "/") <> (C.pack $ show contentLength)) $
                                                         response { rsCode = 206
                                                                  , rsBody = Lazy.take (fromInteger (1 + (t - f))) $ Lazy.drop (fromInteger f) $ rsBody response
                                                                  }
                                                     (ByteRangeSuffix count)
                                                               | count > contentLength -> -- ^ requested more data than available, return 200 instead
                                                                                -- "If the selected representation is shorter than the specified suffix-length, the entire representation is used."
                                                                   response
                                                               | otherwise ->
                                                                  setHeaderBS (original hContentLength) (C.pack $ show count) $
                                                                   setHeaderBS (original hContentRange)
                                                                               (renderByteRanges [ByteRangeFromTo (contentLength - count) (contentLength - 1)] <> (C.pack "/") <> (C.pack $ show contentLength)) $
                                                                     response { rsCode = 206
                                                                              , rsBody = Lazy.take (fromInteger count) $ Lazy.drop (fromInteger (contentLength - count)) $ rsBody response
                                                                              }
                                                     (ByteRangeFrom f)
                                                         | f < contentLength ->
                                                                  setHeaderBS (original hContentLength) (C.pack $ show (contentLength - f)) $
                                                                   setHeaderBS (original hContentRange)
                                                                               ((C.pack "bytes ") <> (C.pack $ show f) <> (C.pack "-") <> (C.pack $ show (contentLength - 1)) <> (C.pack "/") <> (C.pack $ show contentLength)) $
                                                                      response { rsCode = 206
                                                                               , rsBody = Lazy.drop (fromInteger f) $ rsBody response
                                                                               }
                                                         | otherwise ->
                                                             setHeaderBS (original hContentRange) (C.pack "*/" <> (C.pack $ show contentLength)) $
                                                                             ((toResponse ()) { rsCode = 416 })
                                             SendFile {} ->
                                                 case sfSendFile response of
                                                   (SinglePart filePath offset count)
                                                         | (offset == 0) -> -- I am not sure what circumstances would result in offset being non-zero at this point, so I am reluctant to
                                                                            -- to send a partial response
                                                             case br of
                                                               (ByteRangeFromTo f t) | t <= count ->
                                                                setHeaderBS (original hContentLength) (C.pack $ show (1 + t - f)) $
                                                                 setHeaderBS (original hContentRange) (renderByteRanges [br] <> (C.pack "/") <> (C.pack (show (count - offset)))) $
                                                                    response { rsCode = 206
                                                                             , sfSendFile = SinglePart filePath f (1 + t - f)
                                                                             }
                                                               (ByteRangeSuffix suffix)
                                                                   | suffix > count ->
                                                                       response
                                                                   | otherwise ->
                                                                     setHeaderBS (original hContentLength) (C.pack $ show suffix) $
                                                                      setHeaderBS (original hContentRange) (renderByteRanges [ByteRangeFromTo (count - suffix) (count - 1)] <> (C.pack "/") <> (C.pack (show (count - offset)))) $
                                                                       response { rsCode = 206
                                                                                , sfSendFile = SinglePart filePath (count - suffix) suffix
                                                                                }
                                                               (ByteRangeFrom f)
                                                                   | f < count ->
                                                                       setHeaderBS (original hContentLength) (C.pack $ show (count - f)) $
                                                                        setHeaderBS (original hContentRange) (renderByteRanges [ByteRangeFromTo f (count - 1)] <> (C.pack "/") <> (C.pack (show (count - offset)))) $
                                                                       response { rsCode = 206
                                                                                , sfSendFile = SinglePart filePath f (count - f)
                                                                                }

                                                                   | otherwise ->
                                                                       setHeaderBS (original hContentRange) (C.pack "*/" <> (C.pack $ show count)) $
                                                                          ((toResponse ()) { rsCode = 416 })

                                                   _ -> response
                                       brs ->
                                                   -- FIXME: this only checks the case where the Content-Length header was explicitly generated by the handler
                                                   -- but does not handle the case where the Response rfsLength flag is set to 'ContentLength'. Also not sure
                                                   -- what happens if Content-Length header exists bu rsfFlags Chunked is set.
                                         case response of
                                             Response {} ->
                                               case (fmap fst (C.readInteger =<< getHeaderUnsafe (C.pack "content-length") (rsHeaders response))) of
                                                 Nothing -> response -- does not seem like we can return a ranged response on content of unknown length
                                                       -- The Content-Range response header we are supposed to add requires a start and end byte?
                                                 (Just contentLength) ->
                                                   case isInOrderNonOverlapping contentLength brs of
                                                     Nothing -> ((toResponse ()) { rsCode = 416 }) -- FIXME: add headers
                                                     Just [] -> ((toResponse ()) { rsCode = 416 }) -- FIXME: add headers
                                                     Just ranges ->
                                                         let boundary =  "multipart_byteranges_boundary_XXX"
                                                             ct = case getHeaderBS (original hContentType) (rsHeaders response) of
                                                                    Nothing -> "application/octet-stream"
                                                                    (Just ct') -> C.unpack ct'
                                                         in setHeaderBS (original hContentType) (C.pack $ "multipart/byteranges; boundary=" ++ boundary) $
                                                            response { rsCode = 206
                                                                     , rsBody = showMultipartBody boundary (MultiPart.MultiPart (map (mkPart contentLength ct (rsBody response)) ranges)) -- FIXME: probably a space leak
                                                                     }
                                             SendFile {} ->
                                                 case sfSendFile response of
                                                   (SinglePart filePath offset count)
                                                         | (offset == 0) -> -- I am not sure what circumstances would result in offset being non-zero at this point, so I am reluctant to
                                                                            -- to send a partial response
                                                           case isInOrderNonOverlapping count brs of
                                                             Nothing -> ((toResponse ()) { rsCode = 416 }) -- FIXME: add headers
                                                             Just [] -> ((toResponse ()) { rsCode = 416 }) -- FIXME: add headers
                                                             Just ranges ->
                                                                 let boundary =  C.pack "multipart_byteranges_boundary_XXX"
                                                                     ct = case getHeaderBS (original hContentType) (rsHeaders response) of
                                                                            Nothing -> C.pack "application/octet-stream"
                                                                            (Just ct') -> ct'
                                                                 in setHeaderBS (original hContentType) ((C.pack "multipart/byteranges; boundary=") <> boundary) $
                                                                    response { rsCode = 206
                                                                             , sfSendFile = Happstack.MultiPart filePath (count - offset) ct boundary ranges
                                                                             }

                                                   _ -> response
                                   _ -> response -- rsCode was not 200
                            pure response
{-
                         (Just br@[ByteRangeFromTo f t]) -> do -- only one byte range
                             composeFilter $ \response ->
                                 case rsCode response of
                                   200 -> -- partial content only returned for 200 [1]
                                       case response of
                                         Response {} ->
                                             setHeaderBS (original hContentRange) (renderByteRanges br) $
                                              response { rsCode = 206
                                                       , rsBody = Lazy.take (fromInteger (t - f)) $ Lazy.drop (fromInteger f) $ rsBody response
                                                       }
                                         SendFile {} -> response -- TODO: implement
                                   _ -> response
                             pure (Just br)

                         (Just brs) -> do -- multi byte ranges
                             let boundary =  "multipart_byteranges_boundary"
                             composeFilter $ \response ->
                                 case rsCode response of
                                   200 -> -- partial content only returned for 200 [1]
                                      let ct = case getHeaderBS (original hContentType) (rsHeaders response) of
                                                 Nothing -> "application/octet-stream"
                                                 (Just ct') -> C.unpack ct'
                                      in
                                       case response of
                                         Response {} ->
                                             setHeaderBS (original hContentType) (C.pack $ "multipart/byteranges; boundary=" ++ boundary) $
                                              response { rsCode = 206
                                                       , rsBody = showMultipartBody boundary (MultiPart.MultiPart (map (mkPart ct (rsBody response)) brs)) -- FIXME: probably a space leak
                                                       }
                                         SendFile {} ->
                                             case sfSendFile response of -- NOTE: if already MultiPart, then what?
                                               (SinglePart filepath 0 _count) -> -- FIXME: what if count was less than f
                                                 let ct = case getHeaderBS (original hContentType) (rsHeaders response) of
                                                            Nothing -> C.pack "application/octet-stream"
                                                            (Just ct') -> ct'
                                                 in
                                                   setHeaderBS (original hContentType) (C.pack $ "multipart/byteranges; boundary=" ++ boundary) $
                                                               response { rsCode = 206
                                                                        , sfSendFile = Happstack.MultiPart
                                                                                       { sfFilePath = filepath
                                                                                       , mpBoundary = C.pack boundary
                                                                                       , mpContentType = ct
                                                                                       , mpByteRanges = brs
                                                                                       }
                                                                        }
                                               _ -> response
                                   _ -> response
                             pure (Just brs)
-}
    where
      ifRangeCheck req response cont = cont -- FIXME: implement
      mkPart :: Integer -> String -> Lazy.ByteString -> (Integer, Integer) -> MultiPart.BodyPart
      mkPart contentLength ct body (f, t) = -- FIXME: other byteranges
          MultiPart.BodyPart
                       [ (HeaderName "Content-Type", ct)
                       , (HeaderName "Content-Range", (C.unpack $ renderByteRanges [ByteRangeFromTo f t]) ++ "/" ++ show contentLength)
                       ]
              (Lazy.take (fromInteger (1 + t - f)) $ Lazy.drop (fromInteger f) $ body)

{-
      mkFilePart :: String -> FilePath -> ByteRange -> FilePart
      mkFilePart ct filepath br@(ByteRangeFromTo f t) =
          FilePart filepath (fromIntegral f) (fromIntegral (t - f)) (Just $ mkHeaderPairs [("Content-Type", ct), ("Content-Range", (C.unpack $ renderByteRange br) ++ "/*")])
-}
      mkHeaderPairs hdrs = [ HeaderPair (C.pack key) [C.pack value] | (key, value) <- hdrs]

-- Lazy.take (fromInteger (t - f)) $ Lazy.drop (fromInteger f) $ rsBody response


-- 'Accept-Ranges: bytes' header to all Responses RFC 2616
-- suggests that a PUT could have a Content-Range, but it
-- recommended here that the server should reject it with a 501.
-- http://tools.ietf.org/html/draft-ietf-httpbis-p2-semantics-23#section-4.3.4
--
-- https://tools.ietf.org/html/rfc7233 only talks about GET
--
-- We will be conservative an return 501 for any request which includes
-- a 'Content-Range'



{-

 In general, HTTP treats a multipart message-body no differently than
   any other media type: strictly as payload. The one exception is the
   "multipart/byteranges" type (appendix 19.2) when it appears in a 206
   (Partial Content) response, which will be interpreted by some HTTP
   caching mechanisms as described in sections 13.5.4 and 14.16. In all
   other cases, an HTTP user agent SHOULD follow the same or similar
   behavior as a MIME user agent would upon receipt of a multipart type.
   The MIME header fields within each body-part of a multipart message-
   body do not have any significance to HTTP beyond that defined by
   their MIME semantics.

Entity tags are used for comparing two or more entities from the same
   requested resource. HTTP/1.1 uses entity tags in the ETag (section
   14.19), If-Match (section 14.24), If-None-Match (section 14.26), and
   If-Range (section 14.27) header fields. The definition of how they
   are used and compared as cache validators is in section 13.3.3. An
   entity tag consists of an opaque quoted string, possibly prefixed by
   a weakness indicator.

3.12 Range Units

   HTTP/1.1 allows a client to request that only part (a range of) the
   response entity be included within the response. HTTP/1.1 uses range
   units in the Range (section 14.35) and Content-Range (section 14.16)
   header fields. An entity can be broken down into subranges according
   to various structural units.

      range-unit       = bytes-unit | other-range-unit
      bytes-unit       = "bytes"
      other-range-unit = token

   The only range unit defined by HTTP/1.1 is "bytes". HTTP/1.1
   implementations MAY ignore ranges specified using other units.

   HTTP/1.1 has been designed to allow implementations of applications
   that do not depend on knowledge of ranges.

---

 4.If the message uses the media type "multipart/byteranges", and the
     ransfer-length is not otherwise specified, then this self-
     elimiting media type defines the transfer-length. This media type
     UST NOT be used unless the sender knows that the recipient can arse
     it; the presence in a request of a Range header with ultiple byte-
     range specifiers from a 1.1 client implies that the lient can parse
     multipart/byteranges responses.

       A range header might be forwarded by a 1.0 proxy that does not
       understand multipart/byteranges; in this case the server MUST
       delimit the message using methods defined in items 1,3 or 5 of
       this section.

---

The semantics of the GET method change to a "conditional GET" if the
   request message includes an If-Modified-Since, If-Unmodified-Since,
   If-Match, If-None-Match, or If-Range header field. A conditional GET
   method requests that the entity be transferred only under the
   circumstances described by the conditional header field(s). The
   conditional GET method is intended to reduce unnecessary network
   usage by allowing cached entities to be refreshed without requiring
   multiple requests or transferring data already held by the client.

   The semantics of the GET method change to a "partial GET" if the
   request message includes a Range header field. A partial GET requests
   that only part of the entity be transferred, as described in section
   14.35. The partial GET method is intended to reduce unnecessary
   network usage by allowing partially-retrieved entities to be
   completed without transferring data already held by the client.
---
  PUT

The recipient of the entity MUST NOT ignore any Content-*
   (e.g. Content-Range) headers that it does not understand or implement
   and MUST return a 501 (Not Implemented) response in such cases.

---

10.2.7 206 Partial Content

   The server has fulfilled the partial GET request for the resource.
   The request MUST have included a Range header field (section 14.35)
   indicating the desired range, and MAY have included an If-Range
   header field (section 14.27) to make the request conditional.

   The response MUST include the following header fields:

      - Either a Content-Range header field (section 14.16) indicating
        the range included with this response, or a multipart/byteranges
        Content-Type including Content-Range fields for each part. If a
        Content-Length header field is present in the response, its
        value MUST match the actual number of OCTETs transmitted in the
        message-body.

      - Date

      - ETag and/or Content-Location, if the header would have been sent
        in a 200 response to the same request

      - Expires, Cache-Control, and/or Vary, if the field-value might
        differ from that sent in any previous response for the same
        variant

   If the 206 response is the result of an If-Range request that used a
   strong cache validator (see section 13.3.3), the response SHOULD NOT
   include other entity-headers. If the response is the result of an
   If-Range request that used a weak validator, the response MUST NOT
   include other entity-headers; this prevents inconsistencies between
   cached entity-bodies and updated headers. Otherwise, the response
   MUST include all of the entity-headers that would have been returned
   with a 200 (OK) response to the same request.

   A cache MUST NOT combine a 206 response with other previously cached
   content if the ETag or Last-Modified headers do not match exactly,
   see 13.5.4.

   A cache that does not support the Range and Content-Range headers
   MUST NOT cache 206 (Partial) responses.

---

10.4.17 416 Requested Range Not Satisfiable

   A server SHOULD return a response with this status code if a request
   included a Range request-header field (section 14.35), and none of
   the range-specifier values in this field overlap the current extent
   of the selected resource, and the request did not include an If-Range
   request-header field. (For byte-ranges, this means that the first-
   byte-pos of all of the byte-range-spec values were greater than the
   current length of the selected resource.)

   When this status code is returned for a byte-range request, the
   response SHOULD include a Content-Range entity-header field
   specifying the current length of the selected resource (see section
   14.16). This response MUST NOT use the multipart/byteranges content-
   type.

--

13.5.4 Combining Byte Ranges

   A response might transfer only a subrange of the bytes of an entity-
   body, either because the request included one or more Range
   specifications, or because a connection was broken prematurely. After
   several such transfers, a cache might have received several ranges of
   the same entity-body.

   If a cache has a stored non-empty set of subranges for an entity, and
   an incoming response transfers another subrange, the cache MAY
   combine the new subrange with the existing set if both the following
   conditions are met:

      - Both the incoming response and the cache entry have a cache
        validator.

      - The two cache validators match using the strong comparison
        function (see section 13.3.3).

   If either requirement is not met, the cache MUST use only the most
   recent partial response (based on the Date values transmitted with
   every response, and using the incoming response if these values are
   equal or missing), and MUST discard the other partial information.

---

14.5 Accept-Ranges

      The Accept-Ranges response-header field allows the server to
      indicate its acceptance of range requests for a resource:

          Accept-Ranges     = "Accept-Ranges" ":" acceptable-ranges
          acceptable-ranges = 1#range-unit | "none"

      Origin servers that accept byte-range requests MAY send

          Accept-Ranges: bytes

      but are not required to do so. Clients MAY generate byte-range
      requests without having received this header for the resource
      involved. Range units are defined in section 3.12.

      Servers that do not accept any kind of range request for a
      resource MAY send

          Accept-Ranges: none

      to advise the client not to attempt a range request.
---

14.16 Content-Range

   The Content-Range entity-header is sent with a partial entity-body to
   specify where in the full entity-body the partial body should be
   applied. Range units are defined in section 3.12.

       Content-Range = "Content-Range" ":" content-range-spec

       content-range-spec      = byte-content-range-spec
       byte-content-range-spec = bytes-unit SP
                                 byte-range-resp-spec "/"
                                 ( instance-length | "*" )

       byte-range-resp-spec = (first-byte-pos "-" last-byte-pos)
                                      | "*"
       instance-length           = 1*DIGIT

   The header SHOULD indicate the total length of the full entity-body,
   unless this length is unknown or difficult to determine. The asterisk
   "*" character means that the instance-length is unknown at the time
   when the response was generated.

   Unlike byte-ranges-specifier values (see section 14.35.1), a byte-
   range-resp-spec MUST only specify one range, and MUST contain
   absolute byte positions for both the first and last byte of the
   range.

   A byte-content-range-spec with a byte-range-resp-spec whose last-
   byte-pos value is less than its first-byte-pos value, or whose
   instance-length value is less than or equal to its last-byte-pos
   value, is invalid. The recipient of an invalid byte-content-range-
   spec MUST ignore it and any content transferred along with it.

   A server sending a response with status code 416 (Requested range not
   satisfiable) SHOULD include a Content-Range field with a byte-range-
   resp-spec of "*". The instance-length specifies the current length of



Fielding, et al.            Standards Track                   [Page 122]

RFC 2616                        HTTP/1.1                       June 1999


   the selected resource. A response with status code 206 (Partial
   Content) MUST NOT include a Content-Range field with a byte-range-
   resp-spec of "*".

   Examples of byte-content-range-spec values, assuming that the entity
   contains a total of 1234 bytes:

      . The first 500 bytes:
       bytes 0-499/1234

      . The second 500 bytes:
       bytes 500-999/1234

      . All except for the first 500 bytes:
       bytes 500-1233/1234

      . The last 500 bytes:
       bytes 734-1233/1234

   When an HTTP message includes the content of a single range (for
   example, a response to a request for a single range, or to a request
   for a set of ranges that overlap without any holes), this content is
   transmitted with a Content-Range header, and a Content-Length header
   showing the number of bytes actually transferred. For example,

       HTTP/1.1 206 Partial content
       Date: Wed, 15 Nov 1995 06:25:24 GMT
       Last-Modified: Wed, 15 Nov 1995 04:58:08 GMT
       Content-Range: bytes 21010-47021/47022
       Content-Length: 26012
       Content-Type: image/gif

   When an HTTP message includes the content of multiple ranges (for
   example, a response to a request for multiple non-overlapping
   ranges), these are transmitted as a multipart message. The multipart
   media type used for this purpose is "multipart/byteranges" as defined
   in appendix 19.2. See appendix 19.6.3 for a compatibility issue.

   A response to a request for a single range MUST NOT be sent using the
   multipart/byteranges media type.  A response to a request for
   multiple ranges, whose result is a single range, MAY be sent as a
   multipart/byteranges media type with one part. A client that cannot
   decode a multipart/byteranges message MUST NOT ask for multiple
   byte-ranges in a single request.

   When a client requests multiple byte-ranges in one request, the
   server SHOULD return them in the order that they appeared in the
   request.

   If the server ignores a byte-range-spec because it is syntactically
   invalid, the server SHOULD treat the request as if the invalid Range
   header field did not exist. (Normally, this means return a 200
   response containing the full entity).

   If the server receives a request (other than one including an If-
   Range request-header field) with an unsatisfiable Range request-
   header field (that is, all of whose byte-range-spec values have a
   first-byte-pos value greater than the current length of the selected
   resource), it SHOULD return a response code of 416 (Requested range
   not satisfiable) (section 10.4.17).

      Note: clients cannot depend on servers to send a 416 (Requested
      range not satisfiable) response instead of a 200 (OK) response for
      an unsatisfiable Range request-header, since not all servers
      implement this request-header.
---
14.27 If-Range

   If a client has a partial copy of an entity in its cache, and wishes
   to have an up-to-date copy of the entire entity in its cache, it
   could use the Range request-header with a conditional GET (using
   either or both of If-Unmodified-Since and If-Match.) However, if the
   condition fails because the entity has been modified, the client
   would then have to make a second request to obtain the entire current
   entity-body.

   The If-Range header allows a client to "short-circuit" the second
   request. Informally, its meaning is `if the entity is unchanged, send
   me the part(s) that I am missing; otherwise, send me the entire new
   entity'.

        If-Range = "If-Range" ":" ( entity-tag | HTTP-date )

   If the client has no entity tag for an entity, but does have a Last-
   Modified date, it MAY use that date in an If-Range header. (The
   server can distinguish between a valid HTTP-date and any form of
   entity-tag by examining no more than two characters.) The If-Range
   header SHOULD only be used together with a Range header, and MUST be
   ignored if the request does not include a Range header, or if the
   server does not support the sub-range operation.

   If the entity tag given in the If-Range header matches the current
   entity tag for the entity, then the server SHOULD provide the
   specified sub-range of the entity using a 206 (Partial content)
   response. If the entity tag does not match, then the server SHOULD
   return the entire entity using a 200 (OK) response.

---


14.35 Range

14.35.1 Byte Ranges

   Since all HTTP entities are represented in HTTP messages as sequences
   of bytes, the concept of a byte range is meaningful for any HTTP
   entity. (However, not all clients and servers need to support byte-
   range operations.)

   Byte range specifications in HTTP apply to the sequence of bytes in
   the entity-body (not necessarily the same as the message-body).

   A byte range operation MAY specify a single range of bytes, or a set
   of ranges within a single entity.

       ranges-specifier = byte-ranges-specifier
       byte-ranges-specifier = bytes-unit "=" byte-range-set
       byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
       byte-range-spec = first-byte-pos "-" [last-byte-pos]
       first-byte-pos  = 1*DIGIT
       last-byte-pos   = 1*DIGIT

   The first-byte-pos value in a byte-range-spec gives the byte-offset
   of the first byte in a range. The last-byte-pos value gives the
   byte-offset of the last byte in the range; that is, the byte
   positions specified are inclusive. Byte offsets start at zero.

   If the last-byte-pos value is present, it MUST be greater than or
   equal to the first-byte-pos in that byte-range-spec, or the byte-
   range-spec is syntactically invalid. The recipient of a byte-range-
   set that includes one or more syntactically invalid byte-range-spec
   values MUST ignore the header field that includes that byte-range-
   set.

   If the last-byte-pos value is absent, or if the value is greater than
   or equal to the current length of the entity-body, last-byte-pos is
   taken to be equal to one less than the current length of the entity-
   body in bytes.

   By its choice of last-byte-pos, a client can limit the number of
   bytes retrieved without knowing the size of the entity.




Fielding, et al.            Standards Track                   [Page 138]

RFC 2616                        HTTP/1.1                       June 1999


       suffix-byte-range-spec = "-" suffix-length
       suffix-length = 1*DIGIT

   A suffix-byte-range-spec is used to specify the suffix of the
   entity-body, of a length given by the suffix-length value. (That is,
   this form specifies the last N bytes of an entity-body.) If the
   entity is shorter than the specified suffix-length, the entire
   entity-body is used.

   If a syntactically valid byte-range-set includes at least one byte-
   range-spec whose first-byte-pos is less than the current length of
   the entity-body, or at least one suffix-byte-range-spec with a non-
   zero suffix-length, then the byte-range-set is satisfiable.
   Otherwise, the byte-range-set is unsatisfiable. If the byte-range-set
   is unsatisfiable, the server SHOULD return a response with a status
   of 416 (Requested range not satisfiable). Otherwise, the server
   SHOULD return a response with a status of 206 (Partial Content)
   containing the satisfiable ranges of the entity-body.

   Examples of byte-ranges-specifier values (assuming an entity-body of
   length 10000):

      - The first 500 bytes (byte offsets 0-499, inclusive):  bytes=0-
        499

      - The second 500 bytes (byte offsets 500-999, inclusive):
        bytes=500-999

      - The final 500 bytes (byte offsets 9500-9999, inclusive):
        bytes=-500

      - Or bytes=9500-

      - The first and last bytes only (bytes 0 and 9999):  bytes=0-0,-1

      - Several legal but not canonical specifications of the second 500
        bytes (byte offsets 500-999, inclusive):
         bytes=500-600,601-999
         bytes=500-700,601-999

14.35.2 Range Retrieval Requests

   HTTP retrieval requests using conditional or unconditional GET
   methods MAY request one or more sub-ranges of the entity, instead of
   the entire entity, using the Range request header, which applies to
   the entity returned as the result of the request:

      Range = "Range" ":" ranges-specifier

   A server MAY ignore the Range header. However, HTTP/1.1 origin
   servers and intermediate caches ought to support byte ranges when
   possible, since Range supports efficient recovery from partially
   failed transfers, and supports efficient partial retrieval of large
   entities.

   If the server supports the Range header and the specified range or
   ranges are appropriate for the entity:

      - The presence of a Range header in an unconditional GET modifies
        what is returned if the GET is otherwise successful. In other
        words, the response carries a status code of 206 (Partial
        Content) instead of 200 (OK).

      - The presence of a Range header in a conditional GET (a request
        using one or both of If-Modified-Since and If-None-Match, or
        one or both of If-Unmodified-Since and If-Match) modifies what
        is returned if the GET is otherwise successful and the
        condition is true. It does not affect the 304 (Not Modified)
        response returned if the conditional is false.

   In some cases, it might be more appropriate to use the If-Range
   header (see section 14.27) in addition to the Range header.

   If a proxy that supports ranges receives a Range request, forwards
   the request to an inbound server, and receives an entire entity in
   reply, it SHOULD only return the requested range to its client. It
   SHOULD store the entire received response in its cache if that is
   consistent with its cache allocation policies.

---

19.2 Internet Media Type multipart/byteranges

   When an HTTP 206 (Partial Content) response message includes the
   content of multiple ranges (a response to a request for multiple
   non-overlapping ranges), these are transmitted as a multipart
   message-body. The media type for this purpose is called
   "multipart/byteranges".

   The multipart/byteranges media type includes two or more parts, each
   with its own Content-Type and Content-Range fields. The required
   boundary parameter specifies the boundary string used to separate
   each body-part.

       Media Type name:         multipart
       Media subtype name:      byteranges
       Required parameters:     boundary
       Optional parameters:     none
       Encoding considerations: only "7bit", "8bit", or "binary" are
                                permitted
       Security considerations: none


   For example:

   HTTP/1.1 206 Partial Content
   Date: Wed, 15 Nov 1995 06:25:24 GMT
   Last-Modified: Wed, 15 Nov 1995 04:58:08 GMT
   Content-type: multipart/byteranges; boundary=THIS_STRING_SEPARATES

   --THIS_STRING_SEPARATES
   Content-type: application/pdf
   Content-range: bytes 500-999/8000

   ...the first range...
   --THIS_STRING_SEPARATES
   Content-type: application/pdf
   Content-range: bytes 7000-7999/8000

   ...the second range
   --THIS_STRING_SEPARATES--

      Notes:

      1) Additional CRLFs may precede the first boundary string in the
         entity.

      2) Although RFC 2046 [40] permits the boundary string to be
         quoted, some existing implementations handle a quoted boundary
         string incorrectly.

      3) A number of browsers and servers were coded to an early draft
         of the byteranges specification to use a media type of
         multipart/x-byteranges, which is almost, but not quite
         compatible with the version documented in HTTP/1.1.

---
A response received with a status code of 200, 203, 206, 300, 301 or
   410 MAY be stored by a cache and used in reply to a subsequent
   request, subject to the expiration mechanism, unless a cache-control
   directive prohibits caching. However, a cache that does not support
   the Range and Content-Range headers MUST NOT cache 206 (Partial
   Content) responses.
-}
-}
