{-# LANGUAGE ScopedTypeVariables, PatternSignatures #-}

module Happstack.Server.HTTP.Handler(request-- version,required
  ,parseResponse,putRequest
-- ,unchunkBody,val,testChunk,pack
) where
--    ,fsepC,crlfC,pversion
import qualified Paths_happstack_server as Paths
import qualified Data.Version as DV
import Control.Exception.Extensible as E
import Control.Monad
import Data.List(elemIndex)
import Data.Char(toLower)
import Data.Maybe ( fromMaybe, fromJust, isJust, isNothing )
import Prelude hiding (last)
import qualified Data.List as List
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString as S
import System.IO.Unsafe
import qualified Data.Map as M
import System.IO
import Numeric
import Data.Int (Int64)
import Happstack.Server.Cookie
import Happstack.Server.HTTP.Clock
import Happstack.Server.HTTP.Types
import Happstack.Server.HTTP.Multipart
import Happstack.Server.HTTP.RFC822Headers
import Happstack.Server.MessageWrap
import Happstack.Server.SURI(SURI(..),path,query)
import Happstack.Server.SURI.ParseURI
import Happstack.Util.TimeOut
import Happstack.Util.LogFormat (formatRequestCombined)
import Data.Time.Clock (getCurrentTime)
import Network.Socket.SendFile (unsafeSendFile')
import System.Log.Logger (Priority(..), logM)


hGetContentsN :: Int -> Handle -> IO L.ByteString
hGetContentsN k h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetNonBlocking h k
        if S.null c
          then do eof <- hIsEOF h
                  if eof then hClose h >> return L.Empty
                         else hWaitForInput h (-1)
                            >> loop

          --then hClose h >> return Empty
          else do cs <- lazyRead
                  return (L.Chunk c cs)

hGetContents' :: Handle -> IO L.ByteString
hGetContents' = hGetContentsN L.defaultChunkSize

request :: Conf -> Handle -> Host -> (Request -> IO Response) -> IO ()
request conf h host handler = rloop conf h host handler =<< hGetContents' h

required :: String -> Maybe a -> Either String a
required err Nothing  = Left err
required _   (Just a) = Right a

transferEncodingC :: [Char]
transferEncodingC = "transfer-encoding"
rloop :: t
         -> Handle
         -> Host
         -> (Request -> IO Response)
         -> L.ByteString
         -> IO ()
rloop conf h host handler inputStr
    | L.null inputStr = return ()
    | otherwise
    = join $ withTimeOut (30 * second) $
      do let parseRequest
                 = do
                      (topStr, restStr) <- required "failed to separate request" $ splitAtEmptyLine inputStr
                      (rql, headerStr) <- required "failed to separate headers/body" $ splitAtCRLF topStr
                      let (m,u,v) = requestLine rql
                      headers' <- parseHeaders "host" (L.unpack headerStr)
                      let headers = mkHeaders headers'
                      let contentLength = fromMaybe 0 $ fmap fst (P.readInt =<< getHeaderUnsafe contentlengthC headers)
                      (body, nextRequest) <- case () of
                          () | contentLength < 0               -> fail "negative content-length"
                             | isJust $ getHeader transferEncodingC headers ->
                                 return $ consumeChunks restStr
                             | otherwise                       -> return (L.splitAt (fromIntegral contentLength) restStr)
                      let cookies = [ (cookieName c, c) | cl <- fromMaybe [] (fmap getCookies (getHeader "Cookie" headers)), c <- cl ] -- Ugle
                          rqTmp = Request m (pathEls (path u)) (path u) (query u)
                                  [] cookies v headers (Body body) host
                          rq = rqTmp{rqInputs = queryInput u ++ bodyInput rqTmp}
                      return (rq, nextRequest)
         case parseRequest of
           Left err -> error $ "failed to parse HTTP request: " ++ err
           Right (req, rest)
               -> return $
                  do let ioseq act = act >>= \x -> x `seq` return x
                     res <- ioseq (handler req) `E.catch` \(e::E.SomeException) -> return $ result 500 $ "Server error: " ++ show e

                     -- combined log format
                     time <- getCurrentTime
                     let host' = fst host
                         user = "-"
                         requestLn = unwords [show $ rqMethod req, rqUri req, show $ rqVersion req]
                         responseCode = rsCode res
                         size = maybe (-1) (read . B.unpack) (getHeader "Content-Length" res) -- -1 indicates unknown size
                         referer = B.unpack $ fromMaybe (B.pack "") $ getHeader "Referer" req
                         userAgent = B.unpack $ fromMaybe (B.pack "") $ getHeader "User-Agent" req
                     logM "Happstack.Server.AccessLog.Combined" INFO $ formatRequestCombined host' user time requestLn responseCode size referer userAgent

                     putAugmentedResult h req res
                     when (continueHTTP req res) $ rloop conf h host handler rest

-- | Unserializes the bytestring into a response.  If there is an
-- error it will return @Left msg@.
parseResponse :: L.ByteString -> Either String Response
parseResponse inputStr =
    do (topStr,restStr) <- required "failed to separate response" $
                           splitAtEmptyLine inputStr
       (rsl,headerStr) <- required "failed to separate headers/body" $
                          splitAtCRLF topStr
       let (_,code) = responseLine rsl
       headers' <- parseHeaders "host" (L.unpack headerStr)
       let headers = mkHeaders headers'
       let mbCL = fmap fst (B.readInt =<< getHeader "content-length" headers)
       (body,_) <-
           maybe (if (isNothing $ getHeader "transfer-encoding" headers)
                       then  return (restStr,L.pack "")
                       else  return $ consumeChunks restStr)
                 (\cl->return (L.splitAt (fromIntegral cl) restStr))
                 mbCL
       return $ Response {rsCode=code,rsHeaders=headers,rsBody=body,rsFlags=RsFlags True,rsValidator=Nothing}

-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html
-- note this does NOT handle extenions
consumeChunks::L.ByteString->(L.ByteString,L.ByteString)
consumeChunks str = let (parts,tr,rest) = consumeChunksImpl str in (L.concat . (++ [tr]) .map snd $ parts,rest)

consumeChunksImpl :: L.ByteString -> ([(Int64, L.ByteString)], L.ByteString, L.ByteString)
consumeChunksImpl str
    | L.null str = ([],L.empty,str)
    | chunkLen == 0 = let (last,rest') = L.splitAt lenLine1 str
                          (tr',rest'') = getTrailer rest'
                      in ([(0,last)],tr',rest'')
    | otherwise = ((chunkLen,part):crest,tr,rest2)
    where
      line1 = head $ lazylines str
      lenLine1 = (L.length line1) + 1 -- endchar
      chunkLen = (fst $ head $ readHex $ L.unpack line1)
      len = chunkLen + lenLine1 + 2
      (part,rest) = L.splitAt len str
      (crest,tr,rest2) = consumeChunksImpl rest
      getTrailer s = L.splitAt index s
          where index | crlfLC `L.isPrefixOf` s = 2
                      | otherwise = let iscrlf = L.zipWith (\a b -> a == '\r' && b == '\n') s . L.tail $ s
                                        Just i = elemIndex True $ zipWith (&&) iscrlf (tail (tail iscrlf))
                                    in fromIntegral $ i+4

crlfLC :: L.ByteString
crlfLC = L.pack "\r\n"

-- Properly lazy version of 'lines' for lazy bytestrings
lazylines           :: L.ByteString -> [L.ByteString]
lazylines s
    | L.null s  = []
    | otherwise =
        let (l,s') = L.break ((==) '\n') s
        in l : if L.null s' then []
                            else lazylines (L.tail s')

requestLine :: L.ByteString -> (Method, SURI, Version)
requestLine l = case P.words ((P.concat . L.toChunks) l) of
                  [rq,uri,ver] -> (method rq, SURI $ parseURIRef uri, version ver)
                  [rq,uri] -> (method rq, SURI $ parseURIRef uri,Version 0 9)
                  x -> error $ "requestLine cannot handle input:  " ++ (show x)

responseLine :: L.ByteString -> (B.ByteString, Int)
responseLine l = case B.words ((B.concat . L.toChunks) l) of
                   (v:c:_) -> version v `seq` (v,fst (fromJust (B.readInt c)))
                   x -> error $ "responseLine cannot handle input: " ++ (show x)


method :: B.ByteString -> Method
method r = fj $ lookup r mtable
    where fj (Just x) = x
          fj Nothing  = error "invalid request method"
          mtable = [(P.pack "GET",     GET),
                    (P.pack "HEAD",    HEAD),
                    (P.pack "POST",    POST),
                    (P.pack "PUT",     PUT),
                    (P.pack "DELETE",  DELETE),
                    (P.pack "TRACE",   TRACE),
                    (P.pack "OPTIONS", OPTIONS),
                    (P.pack "CONNECT", CONNECT)]

-- Result side

staticHeaders :: Headers
staticHeaders =
    foldr (uncurry setHeaderBS) (mkHeaders [])
    [ (serverC, happsC), (contentTypeC, textHtmlC) ]

-- FIXME: we should not be controlling the response headers in mysterious ways in this low level code
-- headers should be set by application code and the core http engine should be very lean.
putAugmentedResult :: Handle -> Request -> Response -> IO ()
putAugmentedResult outp req res = do
    case res of
        -- standard bytestring response
        Response {} -> do
            sendTop (fromIntegral (L.length (rsBody res)))
            when (rqMethod req /= HEAD) (L.hPut outp $ rsBody res)
        -- zero-copy sendfile response
        -- the handle *should* be closed by the garbage collector
        SendFile {} -> do
            let infp = sfFilePath res
                off = sfOffset res
                count = sfCount res
            sendTop count
            unsafeSendFile' outp infp off count
    hFlush outp
    where ph (HeaderPair k vs) = map (\v -> P.concat [k, fsepC, v, crlfC]) vs
          sendTop cl = do
              allHeaders <- augmentHeaders req res cl
              mapM_ (P.hPut outp) $ concat
                [ (pversion $ rqVersion req)          -- Print HTTP version
                , [responseMessage $ rsCode res]      -- Print responseCode
                , concatMap ph (M.elems allHeaders)   -- Print all headers
                , [crlfC]
                ]

augmentHeaders :: Request -> Response -> Integer -> IO Headers
augmentHeaders req res cl = do
    -- TODO: Hoist static headers to the toplevel.
    raw <- getApproximateTime
    let stdHeaders = staticHeaders `M.union`
          M.fromList ( [ (dateCLower,       HeaderPair dateC [raw])
                       , (connectionCLower, HeaderPair connectionC [if continueHTTP req res then keepAliveC else closeC])
                       ] ++ if rsfContentLength (rsFlags res)
                              then [(contentlengthC, HeaderPair contentLengthC [P.pack (show cl)])]
                              else [] )
    return (rsHeaders res `M.union` stdHeaders) -- 'union' prefers 'headers res' when duplicate keys are encountered.

-- | Serializes the request to the given handle
putRequest :: Handle -> Request -> IO ()
putRequest h rq = do
    let put = B.hPut h
        ph (HeaderPair k vs) = map (\v -> B.concat [k, fsepC, v, crlfC]) vs
        sp = [B.pack " "]
    mapM_ put $ concat
      [[B.pack $ show $ rqMethod rq],sp
      ,[B.pack $ rqURL rq],sp
      ,(pversion $ rqVersion rq), [crlfC]
      ,concatMap ph (M.elems $ rqHeaders rq)
      ,[crlfC]
      ]
    let Body body = rqBody rq
    L.hPut h  body
    hFlush h



-- Version

pversion :: Version -> [B.ByteString]
pversion (Version 1 1) = [http11]
pversion (Version 1 0) = [http10]
pversion (Version x y) = [P.pack "HTTP/", P.pack (show x), P.pack ".", P.pack (show y)]

version :: B.ByteString -> Version
version x | x == http09 = Version 0 9
          | x == http10 = Version 1 0
          | x == http11 = Version 1 1
          | otherwise   = error "Invalid HTTP version"

http09 :: B.ByteString
http09 = P.pack "HTTP/0.9"
http10 :: B.ByteString
http10 = P.pack "HTTP/1.0"
http11 :: B.ByteString
http11 = P.pack "HTTP/1.1"

-- Constants

connectionC :: B.ByteString
connectionC      = P.pack "Connection"
connectionCLower :: B.ByteString
connectionCLower = P.map toLower connectionC
closeC :: B.ByteString
closeC           = P.pack "close"
keepAliveC :: B.ByteString
keepAliveC       = P.pack "Keep-Alive"
crlfC :: B.ByteString
crlfC            = P.pack "\r\n"
fsepC :: B.ByteString
fsepC            = P.pack ": "
contentTypeC :: B.ByteString
contentTypeC     = P.pack "Content-Type"
contentLengthC :: B.ByteString
contentLengthC   = P.pack "Content-Length"
contentlengthC :: B.ByteString
contentlengthC   = P.pack "content-length"
dateC :: B.ByteString
dateC            = P.pack "Date"
dateCLower :: B.ByteString
dateCLower       = P.map toLower dateC
serverC :: B.ByteString
serverC          = P.pack "Server"
happsC :: B.ByteString
happsC           = P.pack $ "Happstack/" ++ DV.showVersion Paths.version
textHtmlC :: B.ByteString
textHtmlC        = P.pack "text/html; charset=utf-8"

-- Response code names

responseMessage :: (Num t) => t -> B.ByteString
responseMessage 100 = P.pack " 100 Continue\r\n"
responseMessage 101 = P.pack " 101 Switching Protocols\r\n"
responseMessage 200 = P.pack " 200 OK\r\n"
responseMessage 201 = P.pack " 201 Created\r\n"
responseMessage 202 = P.pack " 202 Accepted\r\n"
responseMessage 203 = P.pack " 203 Non-Authoritative Information\r\n"
responseMessage 204 = P.pack " 204 No Content\r\n"
responseMessage 205 = P.pack " 205 Reset Content\r\n"
responseMessage 206 = P.pack " 206 Partial Content\r\n"
responseMessage 300 = P.pack " 300 Multiple Choices\r\n"
responseMessage 301 = P.pack " 301 Moved Permanently\r\n"
responseMessage 302 = P.pack " 302 Found\r\n"
responseMessage 303 = P.pack " 303 See Other\r\n"
responseMessage 304 = P.pack " 304 Not Modified\r\n"
responseMessage 305 = P.pack " 305 Use Proxy\r\n"
responseMessage 307 = P.pack " 307 Temporary Redirect\r\n"
responseMessage 400 = P.pack " 400 Bad Request\r\n"
responseMessage 401 = P.pack " 401 Unauthorized\r\n"
responseMessage 402 = P.pack " 402 Payment Required\r\n"
responseMessage 403 = P.pack " 403 Forbidden\r\n"
responseMessage 404 = P.pack " 404 Not Found\r\n"
responseMessage 405 = P.pack " 405 Method Not Allowed\r\n"
responseMessage 406 = P.pack " 406 Not Acceptable\r\n"
responseMessage 407 = P.pack " 407 Proxy Authentication Required\r\n"
responseMessage 408 = P.pack " 408 Request Time-out\r\n"
responseMessage 409 = P.pack " 409 Conflict\r\n"
responseMessage 410 = P.pack " 410 Gone\r\n"
responseMessage 411 = P.pack " 411 Length Required\r\n"
responseMessage 412 = P.pack " 412 Precondition Failed\r\n"
responseMessage 413 = P.pack " 413 Request Entity Too Large\r\n"
responseMessage 414 = P.pack " 414 Request-URI Too Large\r\n"
responseMessage 415 = P.pack " 415 Unsupported Media Type\r\n"
responseMessage 416 = P.pack " 416 Requested range not satisfiable\r\n"
responseMessage 417 = P.pack " 417 Expectation Failed\r\n"
responseMessage 500 = P.pack " 500 Internal Server Error\r\n"
responseMessage 501 = P.pack " 501 Not Implemented\r\n"
responseMessage 502 = P.pack " 502 Bad Gateway\r\n"
responseMessage 503 = P.pack " 503 Service Unavailable\r\n"
responseMessage 504 = P.pack " 504 Gateway Time-out\r\n"
responseMessage 505 = P.pack " 505 HTTP Version not supported\r\n"
responseMessage x   = P.pack (show x ++ "\r\n")

