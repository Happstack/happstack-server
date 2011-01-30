-- |HUnit tests and QuickQuick properties for Happstack.Server.*
module Happstack.Server.Tests (allTests) where

import qualified Codec.Compression.GZip as GZ
import qualified Codec.Compression.Zlib as Z
import Control.Arrow ((&&&))
import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Monad (msum)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8     (pack, unpack)
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Lazy  as L
import qualified Data.Map              as Map
import Happstack.Server                      ( Request(..), Method(..), Response(..), ServerPart(..), Headers(..), RqBody(Body), HttpVersion(..)
                                             , ToMessage(..), HeaderPair(..), ok, dir, simpleHTTP'', composeFilter, noContentLength)
import Happstack.Server.FileServe.BuildingBlocks (sendFileResponse)
import Happstack.Server.Cookie
import Happstack.Server.Internal.Compression
import Happstack.Server.Internal.Cookie
import Happstack.Server.Internal.Multipart
import Happstack.Server.Internal.MessageWrap
import Happstack.Server.SURI(ToSURI(..), SURI(..),path,query)
import Test.HUnit as HU (Test(..),(~:),(~?),(@?=),(@=?), assertEqual, assertFailure)
import Text.ParserCombinators.Parsec

-- |All of the tests for happstack-util should be listed here. 
allTests :: Test
allTests = 
    "happstack-server tests" ~: [ cookieParserTest
                                , acceptEncodingParserTest
                                , splitMultipart 
                                , compressFilterResponseTest
                                ]

cookieParserTest :: Test
cookieParserTest = 
    "cookieParserTest" ~:
    [parseCookies "$Version=1;Cookie1=value1;$Path=\"/testpath\";$Domain=example.com;cookie2=value2"
        @?= (Right [
            Cookie "1" "/testpath" "example.com" "cookie1" "value1" False
          , Cookie "1" "" "" "cookie2" "value2" False
          ])
    ,parseCookies "  \t $Version = \"1\" ; cookie1 = \"randomcrap!@#%^&*()-_+={}[]:;'<>,.?/\\|\" , $Path=/  "
        @?= (Right [
            Cookie "1" "/" "" "cookie1" "randomcrap!@#%^&*()-_+={}[]:;'<>,.?/\\|" False
          ])
    ,parseCookies " cookie1 = value1  "
        @?= (Right [
            Cookie "" "" "" "cookie1" "value1" False
          ])
    ,parseCookies " $Version=\"1\";buggygooglecookie = valuewith=whereitshouldnotbe  "
        @?= (Right [
            Cookie "1" "" "" "buggygooglecookie" "valuewith=whereitshouldnotbe" False
          ])
    ]

acceptEncodingParserTest :: Test
acceptEncodingParserTest =
    "acceptEncodingParserTest" ~:
    map (\(string, result) -> either (Left . show) Right (parse encodings "" string) @?= (Right result)) acceptEncodings
    where
      acceptEncodings =
       [ (" gzip;q=1,*, compress ; q = 0.5 ", [("gzip", Just 1),("*", Nothing),("compress", Just 0.5)])
       , (" compress , gzip", [ ("compress", Nothing), ("gzip", Nothing)])
       , (" ", [])
       , (" *", [("*", Nothing)])
       , (" compress;q=0.5, gzip;q=1.0", [("compress", Just 0.5), ("gzip", Just 1.0)])
       , (" gzip;q=1.0, identity; q=0.5, *;q=0", [("gzip", Just 1.0), ("identity",Just 0.5), ("*", Just 0)])
       , (" x-gzip",[("x-gzip", Nothing)])
       ]

splitMultipart :: Test
splitMultipart =
    "split multipart" ~: assertFailure "update splitMultipart test"
{-
    [ Just [pack "1"] @=? 
           splitParts (pack "boundary")
                      (pack "beg\r\n--boundary\r\n1\r\n--boundary--\r\nend")
    , Just [pack "1\n"] @=? 
           splitParts (pack "boundary")
                      (pack "beg\r\n--boundary\r\n1\n\r\n--boundary--\r\nend")
    , Just [pack "1\r\n"] @=? 
           splitParts (pack "boundary")
                      (pack "beg\r\n--boundary\r\n1\r\n\r\n--boundary--\r\nend")
    , Just [pack "1\n\r"] @=? 
           splitParts (pack "boundary")
                      (pack "beg\r\n--boundary\r\n1\n\r\r\n--boundary--\r\nend")
    , Just [pack "\r\n1\n\r"] @=? 
           splitParts (pack "boundary")
                      (pack "beg\r\n--boundary\r\n\r\n1\n\r\r\n--boundary--\r\nend")
    ]
-}
compressFilterResponseTest :: Test
compressFilterResponseTest =
    "compressFilterResponseTest" ~: 
     [ uncompressedResponse 
     , uncompressedSendFile
     , compressedResponseGZ
     , compressedResponseZ
     , compressedSendFile
     , compressedSendFileNoIdentity
     ]

mkRequest :: Method -> String -> [(String, Cookie)] -> Headers -> L.ByteString -> IO Request
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
                        }

compressPart :: ServerPart Response
compressPart =
    do compressedResponseFilter
       composeFilter noContentLength
       msum [ dir "response" $ ok (toResponse "compress Response")
            , dir "sendfile" $ ok (sendFileResponse "text/plain" "/dev/null" Nothing 0 100)
            ]

uncompressedResponse :: Test
uncompressedResponse =
    "uncompressedResponse" ~:
      do req <- mkRequest GET "/response" [] Map.empty L.empty
         res <- simpleHTTP'' compressPart req
         assertEqual "respone code"     (rsCode res) 200
         assertEqual "body"             (unpack (rsBody res)) "compress Response"
         assertEqual "Content-Encoding" ((hName &&& hValue) <$> Map.lookup (B.pack "content-encoding") (rsHeaders res)) Nothing

uncompressedSendFile :: Test
uncompressedSendFile =
    "uncompressedSendFile" ~:
      do req <- mkRequest GET "/sendfile" [] Map.empty L.empty
         res <- simpleHTTP'' compressPart req
         assertEqual "respone code"     (rsCode res) 200
         assertEqual "filepath"         (sfFilePath res) "/dev/null"
         assertEqual "Content-Encoding" ((hName &&& hValue) <$> Map.lookup (B.pack "content-encoding") (rsHeaders res)) Nothing

compressedResponseGZ :: Test
compressedResponseGZ =
    "compressedResponseGZ" ~:
      do req <- mkRequest GET "/response" [] (Map.singleton (B.pack "accept-encoding") (HeaderPair (B.pack "Accept-Encoding") [B.pack " gzip;q=1"])) L.empty
         res <- simpleHTTP'' compressPart req
         assertEqual "respone code"     (rsCode res) 200
         assertEqual "body"             (unpack (GZ.decompress (rsBody res))) ("compress Response")
         assertEqual "Content-Encoding" ((hName &&& hValue) <$> Map.lookup (B.pack "content-encoding") (rsHeaders res)) (Just (B.pack "Content-Encoding", [B.pack "gzip"]))

compressedResponseZ :: Test
compressedResponseZ =
    "compressedResponseZ" ~:
      do req <- mkRequest GET "/response" [] (Map.singleton (B.pack "accept-encoding") (HeaderPair (B.pack "Accept-Encoding") [B.pack " deflate;q=1"])) L.empty
         res <- simpleHTTP'' compressPart req
         assertEqual "respone code"     (rsCode res) 200
         assertEqual "body"             (unpack (Z.decompress (rsBody res))) ("compress Response")
         assertEqual "Content-Encoding" ((hName &&& hValue) <$> Map.lookup (B.pack "content-encoding") (rsHeaders res)) (Just (B.pack "Content-Encoding", [B.pack "deflate"]))

compressedSendFile :: Test
compressedSendFile =
    "compressedSendfile" ~:
      do req <- mkRequest GET "/sendfile" [] (Map.singleton (B.pack "accept-encoding") (HeaderPair (B.pack "Accept-Encoding") [B.pack " gzip;q=1"])) L.empty
         res <- simpleHTTP'' compressPart req
         assertEqual "respone code"     (rsCode res) 200
         assertEqual "filepath"         (sfFilePath res) "/dev/null"
         assertEqual "Content-Encoding" ((hName &&& hValue) <$> Map.lookup (B.pack "content-encoding") (rsHeaders res)) Nothing

compressedSendFileNoIdentity :: Test
compressedSendFileNoIdentity =
    "compressedSendFileNoIdentity" ~:
      do req <- mkRequest GET "/sendfile" [] (Map.singleton (B.pack "accept-encoding") (HeaderPair (B.pack "Accept-Encoding") [B.pack " gzip;q=1, identity: q=0.0"])) L.empty
         res <- simpleHTTP'' compressPart req
         assertEqual "respone code"     (rsCode res) 406
         assertEqual "body"             (unpack (rsBody res)) ""
         assertEqual "Content-Encoding" ((hName &&& hValue) <$> Map.lookup (B.pack "content-encoding") (rsHeaders res)) Nothing
