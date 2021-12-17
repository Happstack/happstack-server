{-# LANGUAGE ScopedTypeVariables, ScopedTypeVariables, TupleSections #-}

module Happstack.Server.Internal.Handler
    ( request
    , parseResponse
    , putRequest
    ) where

import qualified Paths_happstack_server as Paths
import qualified Data.Version as DV
import Control.Applicative (pure)
import Control.Concurrent (newMVar, newEmptyMVar, tryTakeMVar)
import Control.Exception.Extensible as E
import Control.Monad
import Data.List(elemIndex)
import Data.Char(toLower)
import Data.Maybe ( fromMaybe, fromJust, isJust, isNothing )
import Data.Time      (UTCTime)
import Prelude hiding (last)
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Lazy.Internal (ByteString(Chunk, Empty))
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map as M
import Data.Int (Int64)
import Happstack.Server.Internal.Cookie
import Happstack.Server.Internal.Clock
import Happstack.Server.Internal.Types
import Happstack.Server.Internal.Multipart
import Happstack.Server.Internal.RFC822Headers
import Happstack.Server.Internal.MessageWrap
import Happstack.Server.SURI(SURI(..),path,query)
import Happstack.Server.SURI.ParseURI
import Happstack.Server.Internal.TimeoutIO (TimeoutIO(..))
import Happstack.Server.Internal.Monads (failResponse)
import qualified Happstack.Server.Internal.TimeoutManager as TM
import Network.Sendfile (FileRange(PartOfFile), rangeOffset, rangeLength)
import Numeric
import System.Directory (removeFile)
import System.IO
import System.IO.Error (isDoesNotExistError)

request :: TimeoutIO -> Maybe (LogAccess UTCTime) -> Host -> (Request -> IO Response) -> IO ()
request timeoutIO mlog host handler =
    rloop timeoutIO mlog host handler =<< toGetContents timeoutIO

required :: String -> Maybe a -> Either String a
required err Nothing  = Left err
required _   (Just a) = Right a

rloop :: TimeoutIO
         -> Maybe (LogAccess UTCTime)
         -> Host
         -> (Request -> IO Response)
         -> L.ByteString
         -> IO ()
rloop timeoutIO mlog host handler inputStr
    | L.null inputStr = return ()
    | otherwise
    = (join $
      do let parseRequest
                 = do
                      (topStr, restStr) <- required "failed to separate request" $ splitAtEmptyLine inputStr
                      (rql, headerStr)  <- required "failed to separate headers/body" $ splitAtCRLF topStr
                      let (m,u,v) = requestLine rql
                      headers' <- case parseHeaders "host" (L.unpack headerStr) of
                        Nothing -> Left "failed to parse host header"
                        Just x -> Right x
                      let headers = mkHeaders headers'
                      let contentLen = fromMaybe 0 $ fmap fst (P.readInt =<< getHeaderUnsafe contentlengthC headers)
                      (body, nextRequest) <- case () of
                          () | contentLen < 0               -> Left "negative content-length"
                             | isJust $ getHeaderBS transferEncodingC headers ->
                                 return $ consumeChunks restStr
                             | otherwise                       -> return (L.splitAt (fromIntegral contentLen) restStr)
                      let cookies = [ (cookieName c, c) | cl <- fromMaybe [] (fmap getCookies (getHeader "Cookie" headers)), c <- cl ] -- Ugle
                      return (m, u, cookies, v, headers, body, nextRequest)

         case parseRequest of
           Left err -> error $ "failed to parse HTTP request: " ++ err
           Right (m, u, cookies, v, headers, body, nextRequest)
              -> pure $
                  do bodyRef        <- newMVar (Body body)
                     bodyInputRef   <- newEmptyMVar
                     let req = Request (toSecure timeoutIO) m (pathEls (path u)) (path u) (query u)
                                  (queryInput u) bodyInputRef cookies v headers bodyRef host

                     let ioseq act = act >>= \x -> x `seq` return x

                     (res, handlerKilled) <- ((, False) `liftM` ioseq (handler req))
                         `E.catches` [ Handler $ \(e::EscapeHTTP)      -> throwIO e -- need to handle this higher up
                                     , Handler $ \(e::E.SomeException) -> pure (failResponse (show e), fromException e == Just ThreadKilled)
                                     ]

                     case mlog of
                       Nothing -> return ()
                       (Just logger) ->
                           do time <- getApproximateUTCTime
                              let host'        = fst host
                                  user         = "-"
                                  requestLn    = unwords [show $ rqMethod req, rqUri req, show $ rqVersion req]
                                  responseCode = rsCode res
                                  size         = maybe (-1) (readDec' . B.unpack) (getHeader "Content-Length" res) -- -1 indicates unknown size
                                  referer      = B.unpack $ fromMaybe (B.pack "") $ getHeader "Referer" req
                                  userAgent    = B.unpack $ fromMaybe (B.pack "") $ getHeader "User-Agent" req
                              logger host' user time requestLn responseCode size referer userAgent

                     -- withNoPush sock $ putAugmentedResult thandle sock req res
                     putAugmentedResult timeoutIO req res
                     -- clean up tmp files
                     cleanupTempFiles req
                     -- do not continue if handler was killed
                     when (not handlerKilled && continueHTTP req res) $
                         rloop timeoutIO mlog host handler nextRequest) `E.catch` (escapeHttpHandler timeoutIO)

escapeHttpHandler :: TimeoutIO
                  -> EscapeHTTP
                  -> IO ()
escapeHttpHandler tio (EscapeHTTP f) = f tio

-- NOTE: if someone took the inputs and never put them back, then they are responsible for the cleanup
cleanupTempFiles :: Request -> IO ()
cleanupTempFiles req =
    do mInputs <- tryTakeMVar (rqInputsBody req)
       case mInputs of
         Nothing -> return ()
         (Just inputs) -> mapM_ deleteTmpFile inputs
    where
      deleteTmpFile :: (String, Input) -> IO ()
      deleteTmpFile (_, input) =
          case inputValue input of
            (Left fp) -> E.catchJust (guard . isDoesNotExistError) (removeFile fp)  (const $ return ())
            _         -> return ()

-- | Unserializes the bytestring into a response.  If there is an
-- error it will return @Left msg@.
parseResponse :: L.ByteString -> Either String Response
parseResponse inputStr =
    do (topStr,restStr) <- required "failed to separate response" $
                           splitAtEmptyLine inputStr
       (rsl,headerStr) <- required "failed to separate headers/body" $
                          splitAtCRLF topStr
       let (_,code) = responseLine rsl
       headers' <- case parseHeaders "host" (L.unpack headerStr) of
         Nothing -> Left "failed to parse host header"
         Just x -> Right x
       let headers = mkHeaders headers'
       let mbCL = fmap fst (B.readInt =<< getHeader "content-length" headers)
       (body,_) <-
           maybe (if (isNothing $ getHeader "transfer-encoding" headers)
                       then  return (restStr,L.pack "")
                       else  return $ consumeChunks restStr)
                 (\cl->return (L.splitAt (fromIntegral cl) restStr))
                 mbCL
       return $ Response {rsCode=code,rsHeaders=headers,rsBody=body,rsFlags=RsFlags ContentLength,rsValidator=Nothing}

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

requestLine :: L.ByteString -> (Method, SURI, HttpVersion)
requestLine l = case P.words ((P.concat . L.toChunks) l) of
                  [rq,uri,ver] -> (method rq, SURI $ parseURIRef uri, version ver)
                  [rq,uri] -> (method rq, SURI $ parseURIRef uri,HttpVersion 0 9)
                  x -> error $ "requestLine cannot handle input:  " ++ (show x)

responseLine :: L.ByteString -> (B.ByteString, Int)
responseLine l = case B.words ((B.concat . L.toChunks) l) of
                   (v:c:_) -> version v `seq` (v,fst (fromJust (B.readInt c)))
                   x -> error $ "responseLine cannot handle input: " ++ (show x)


method :: B.ByteString -> Method
method r = fj $ lookup r mtable
    where fj (Just x) = x
          fj Nothing  = EXTENSION r
          mtable = [ (P.pack "GET",     GET)
                   , (P.pack "HEAD",    HEAD)
                   , (P.pack "POST",    POST)
                   , (P.pack "PUT",     PUT)
                   , (P.pack "DELETE",  DELETE)
                   , (P.pack "TRACE",   TRACE)
                   , (P.pack "OPTIONS", OPTIONS)
                   , (P.pack "CONNECT", CONNECT)
                   , (P.pack "PATCH",   PATCH)
                   ]

-- Result side

staticHeaders :: Headers
staticHeaders =
    foldr (uncurry setHeaderBS) (mkHeaders [])
    [ (serverC, happstackC) ]

-- FIXME: we should not be controlling the response headers in mysterious ways in this low level code
-- headers should be set by application code and the core http engine should be very lean.
putAugmentedResult :: TimeoutIO -> Request -> Response -> IO ()
putAugmentedResult timeoutIO req res = do
    case res of
        -- standard bytestring response
        Response {} -> do
            let isChunked = rsfLength (rsFlags res) == TransferEncodingChunked && isHTTP1_1 req
            sendTop (if isChunked then Nothing else (Just (fromIntegral (L.length (rsBody res))))) isChunked
            when (rqMethod req /= HEAD)
                     (let body = if isChunked
                                 then chunk (rsBody res)
                                 else rsBody res
                      in toPutLazy timeoutIO body)
        -- zero-copy sendfile response
        -- the handle *should* be closed by the garbage collector

        SendFile {} -> do
            let infp = sfFilePath res
                off = sfOffset res
                count = sfCount res
            sendTop (Just count) False
            TM.tickle (toHandle timeoutIO)
            toSendFile timeoutIO infp PartOfFile { rangeOffset=off, rangeLength=count }

    where ph (HeaderPair k vs) = map (\v -> P.concat [k, fsepC, v, crlfC]) vs
          sendTop cl isChunked = do
              allHeaders <- augmentHeaders req res cl isChunked
              toPut timeoutIO $ B.concat $ concat
                 [ (pversion $ rqVersion req)          -- Print HTTP version
                 , [responseMessage $ rsCode res]      -- Print responseCode
                 , concatMap ph (M.elems allHeaders)   -- Print all headers
                 , [crlfC]
                 ]
              TM.tickle (toHandle timeoutIO)
          chunk :: L.ByteString -> L.ByteString
          chunk Empty        = LC.pack "0\r\n\r\n"
          chunk (Chunk c cs) = Chunk (B.pack $ showHex (B.length c) "\r\n") (Chunk c (Chunk (B.pack "\r\n") (chunk cs)))

augmentHeaders :: Request -> Response -> Maybe Integer -> Bool -> IO Headers
augmentHeaders req res mcl isChunked = do
    -- TODO: Hoist static headers to the toplevel.
    raw <- getApproximateTime
    let stdHeaders = staticHeaders `M.union`
          M.fromList ( [ (dateCLower,       HeaderPair dateC [raw])
                       , (connectionCLower, HeaderPair connectionC [if continueHTTP req res then keepAliveC else closeC])
                       ] ++ case rsfLength (rsFlags res) of
                              NoContentLength -> []
                              ContentLength | not (hasHeader "Content-Length" res) ->
                                                case mcl of
                                                  (Just cl) -> [(contentlengthC, HeaderPair contentLengthC [P.pack (show cl)])]
                                                  _ -> []
                                            | otherwise -> []
                              TransferEncodingChunked
                                  -- we check 'chunked' because we might not use this mode if the client is http 1.0
                                  | isChunked -> [(transferEncodingC, HeaderPair transferEncodingC [chunkedC])]
                                  | otherwise -> []

                     )
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
    mBody <- takeRequestBody rq -- tryTakeMVar (rqBody rq)
    L.hPut h (maybe L.empty unBody mBody) -- FIXME: should this actually be an error if the body is null?
    hFlush h

-- HttpVersion

pversion :: HttpVersion -> [B.ByteString]
pversion (HttpVersion 1 1) = [http11]
pversion (HttpVersion 1 0) = [http10]
pversion (HttpVersion x y) = [P.pack "HTTP/", P.pack (show x), P.pack ".", P.pack (show y)]

version :: B.ByteString -> HttpVersion
version x | x == http09 = HttpVersion 0 9
          | x == http10 = HttpVersion 1 0
          | x == http11 = HttpVersion 1 1
          | otherwise   = error "Invalid HTTP version"

http09 :: B.ByteString
http09 = P.pack "HTTP/0.9"
http10 :: B.ByteString
http10 = P.pack "HTTP/1.0"
http11 :: B.ByteString
http11 = P.pack "HTTP/1.1"

-- * ByteString Constants

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
-- contentTypeC :: B.ByteString
-- contentTypeC     = P.pack "Content-Type"
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
happstackC :: B.ByteString
happstackC           = P.pack $ "Happstack/" ++ DV.showVersion Paths.version
-- textHtmlC :: B.ByteString
-- textHtmlC        = P.pack "text/html; charset=utf-8"
transferEncodingC :: B.ByteString
transferEncodingC = P.pack "Transfer-Encoding"
chunkedC :: B.ByteString
chunkedC = P.pack "chunked"

-- Response code names

responseMessage :: (Num t, Show t, Eq t) => t -> B.ByteString
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
responseMessage x   = P.pack (" " ++ show x ++ " \r\n")
