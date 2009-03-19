{-# LANGUAGE OverlappingInstances, FlexibleInstances, DeriveDataTypeable #-}
module Happstack.Server.UDP
    ( UDPConfig(..)
    , Request(..)
    , udpServer
    , sendUDPMessage            -- :: HostAddress -> PortNumber -> BS.ByteString -> IO ()
    ) where

import Control.Concurrent
import Control.Exception as E
import Control.Monad (liftM,foldM)
import Data.Typeable
import Foreign
import Foreign.Marshal
import Happstack.Util.Common ( readM )
import Network.Socket hiding (listen)
import System.IO ( IOMode(WriteMode),hClose )
import System.Log.Logger

import qualified Data.ByteString as BS


udpServer :: (Request -> IO ()) -> UDPConfig -> Handler st
udpServer fn conf = IoH $ listen conf fn

-- | UDP configuration
data UDPConfig = UDPConfig
    { bodyLimit :: Int -- ^ Limit on the number of bytes accepted for Requests. Default 2k.
    , port      :: Int -- ^ Port for the server to listen on. Default 9000.
    }

nullUDPConfig = UDPConfig { bodyLimit = 2 * 1024
                          , port      = 9000
                          }


data Request = Request { udpMsg :: BS.ByteString, udpAddr :: SockAddr} deriving Typeable

instance Show Request where
    showsPrec n (Request bs (SockAddrInet port ip)) = showsPrec n (bs, port, ip)
    -- FIXME: support SockAddrUnix.
instance Read Request where
    readsPrec n str = do ((bs, port, ip),rest) <- readsPrec n str
                         return (Request bs (SockAddrInet (fromIntegral port) ip), rest)


listen :: UDPConfig -> (Request -> IO ()) -> IO ()
listen conf hand = do
  s <- socket AF_INET Datagram 0
  bindSocket s (SockAddrInet (fromIntegral $ port conf) iNADDR_ANY)
  let work (bs, addr) = hand $ Request bs addr -- hand . uncurry Request
      maxInput = 1024*2
      loop = recvBSFrom s maxInput >>= forkIO . work >> loop
      pe e = logM "Happstack.Server.UDP" ERROR ("ERROR in accept thread: "++show e)
      infi = loop `E.catch` pe >> infi
  infi

recvBSFrom :: Socket -> Int -> IO (BS.ByteString, SockAddr)
recvBSFrom sock maxLength
    = do ptr <- mallocBytes maxLength
         (len, sockAddr) <- recvBufFrom sock ptr maxLength
         bs <- BS.packCStringLen (ptr,len)
         return (bs, sockAddr)

sendUDPMessage :: HostAddress -> PortNumber -> BS.ByteString -> IO ()
sendUDPMessage ip port msg =
    do fd <- socket AF_INET Datagram 0
       let target = (SockAddrInet (fromIntegral port) ip)
       connect fd target
       handle <- socketToHandle fd WriteMode
       BS.hPut handle msg
       hClose handle

