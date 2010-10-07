{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables, ScopedTypeVariables #-}
module Happstack.Server.Internal.Listen(listen, listen',listenOn) where

import Happstack.Server.Internal.Types
import Happstack.Server.Internal.Handler
import Happstack.Server.Internal.Socket (acceptLite)
import Happstack.Server.Internal.Timeout (timeoutThread, cancelTimeout)
import Control.Exception.Extensible as E
import Control.Concurrent (forkIO, killThread, myThreadId)
import qualified Data.DList as D
import           Data.IORef (atomicModifyIORef, newIORef)
import qualified Data.PSQueue as PSQ
import Data.Time.Clock.POSIX(getPOSIXTime)
import Network.BSD (getProtocolNumber)
import Network(sClose, Socket)
import Network.Socket as Socket (SocketOption(KeepAlive), setSocketOption, 
                                 socket, Family(..), SockAddr, 
                                 SocketOption(..), SockAddr(..), 
                                 iNADDR_ANY, maxListenQueue, SocketType(..), 
                                 bindSocket)
import qualified Network.Socket as Socket (listen)
import System.IO
{-
#ifndef mingw32_HOST_OS
-}
import System.Posix.Signals
{-
#endif
-}
import System.Log.Logger (Priority(..), logM)
log':: Priority -> String -> IO ()
log' = logM "Happstack.Server.HTTP.Listen"


{-
   Network.listenOn binds randomly to IPv4 or IPv6 or both,
   depending on system and local settings.
   Lets make it use IPv4 only for now.
-}

listenOn :: Int -> IO Socket
listenOn portm = do
    proto <- getProtocolNumber "tcp"
    E.bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (fromIntegral portm) iNADDR_ANY)
            Socket.listen sock maxListenQueue
            return sock
        )


-- | Bind and listen port
listen :: Conf -> (Request -> IO Response) -> IO ()
listen conf hand = do
    let port' = port conf
    socketm <- listenOn port'
    setSocketOption socketm KeepAlive 1
    listen' socketm conf hand

-- | Use a previously bind port and listen
listen' :: Socket -> Conf -> (Request -> IO Response) -> IO ()
listen' s conf hand = do
{-
#ifndef mingw32_HOST_OS
-}
  installHandler openEndedPipe Ignore Nothing
{-
#endif
-}
  let port' = port conf
  log' NOTICE ("Listening on port " ++ show port')
  tedits <- newIORef D.empty
  ttid <- timeoutThread tedits PSQ.empty
  let work (h,hn,p) = do -- hSetBuffering h NoBuffering
                         let eh (x::SomeException) = log' ERROR ("HTTP request failed with: "++show x)
                         tid <- myThreadId
                         now <- getPOSIXTime
                         atomicModifyIORef tedits $ \es -> (D.snoc es (PSQ.insert tid now), ())
                         request tid tedits conf h (hn,fromIntegral p) hand `E.catch` eh
                         -- remove thread from timeout table
                         cancelTimeout tid tedits
                         hClose h
  let loop = do acceptLite s >>= forkIO . work
                loop
  let pe e = log' ERROR ("ERROR in accept thread: " ++ show e)
  let infi = loop `catchSome` pe >> infi -- loop `E.catch` pe >> infi
  infi `finally` (sClose s >> killThread ttid)
{--
#ifndef mingw32_HOST_OS
-}
  installHandler openEndedPipe Ignore Nothing
  return ()
{-
#endif
-}
  where  -- why are these handlers needed?

    catchSome op h = op `E.catches` [
            Handler $ \(e :: ArithException) -> h (toException e),
            Handler $ \(e :: ArrayException) -> h (toException e)
          ]
