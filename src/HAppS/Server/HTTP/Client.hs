module HAppS.Server.HTTP.Client where


import HAppS.Server.HTTP.Handler
import HAppS.Server.HTTP.Types
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L 

import System.IO
import qualified Data.ByteString.Char8 as B 
import Network

getResponse :: Request -> IO (Either String Response)
getResponse rq = withSocketsDo $ do
  let (hostName,p) = span (/=':') $ fromJust $ fmap B.unpack $ getHeader "host" rq 
      portInt = if null p then 80 else read $ tail p
      portId = PortNumber $ toEnum $ portInt
  h <- connectTo hostName portId 
  hSetBuffering h NoBuffering

  putRequest h rq
  hFlush h

  inputStr <- L.hGetContents h
  return $ parseResponse inputStr

unproxify :: Request -> Request
unproxify rq = rq {rqPaths = tail $ rqPaths rq,
                   rqHeaders = 
                       forwardedFor $ forwardedHost $ 
                       setHeader "host" (head $ rqPaths rq) $
                   rqHeaders rq}
  where
  appendInfo hdr val x = setHeader hdr (csv val $
                                        maybe "" B.unpack $
                                        getHeader hdr rq) x
  forwardedFor = appendInfo "X-Forwarded-For" (fst $ rqPeer rq)
  forwardedHost = appendInfo "X-Forwarded-Host" 
                  (B.unpack $ fromJust $ getHeader "host" rq)
  csv v "" = v
  csv v x = x++", " ++ v

unrproxify :: String -> [(String, String)] -> Request -> Request
unrproxify defaultHost list rq = unproxify rq {rqPaths = host: rqPaths rq}
  where
  host::String
  host = maybe defaultHost (f .B.unpack) $
         getHeader "host" rq
  f = maybe defaultHost id . flip lookup list

