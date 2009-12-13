module Happstack.Server.HTTP.Client where


import Happstack.Server.HTTP.Handler
import Happstack.Server.HTTP.Types
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L 

import System.IO
import qualified Data.ByteString.Char8 as B 
import Network

-- | Sends the serialized request to the host defined in the request
-- and attempts to parse response upon arrival.
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
  appendInfo hdr val = setHeader hdr (csv val $
                                        maybe "" B.unpack $
                                        getHeader hdr rq)
  forwardedFor = appendInfo "X-Forwarded-For" (fst $ rqPeer rq)
  forwardedHost = appendInfo "X-Forwarded-Host" 
                  (B.unpack $ fromJust $ getHeader "host" rq)
  csv v "" = v
  csv v x = x++", " ++ v

unrproxify :: String -> [(String, String)] -> Request -> Request
unrproxify defaultHost list rq = 
  let host::String
      host = fromMaybe defaultHost $ flip lookup list =<< B.unpack `liftM` getHeader "host" rq 
      newrq = rq {rqPaths = host: rqPaths rq}
  in  unproxify newrq


