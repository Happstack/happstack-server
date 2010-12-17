{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}
module Happstack.Server.Internal.Listen(listen, listen',listenOn) where

import Happstack.Server.Internal.Types
import Happstack.Server.Internal.Handler
import Happstack.Server.Internal.Socket (acceptLite)
import Happstack.Server.Internal.Timeout (TimeoutHandle(..), tickleTimeout, timeoutThread, cancelTimeout)
import qualified Happstack.Server.Internal.TimeoutTable as TT
import Control.Exception.Extensible as E
import Control.Concurrent (forkIO, killThread, myThreadId)
import Control.Monad (forever, when)
import Data.Concurrent.HashMap (hashString)
import Network.BSD (getProtocolNumber)
import Network(sClose, Socket)
import Network.Socket as Socket (SocketOption(KeepAlive), setSocketOption, 
                                 socket, Family(..), SockAddr, 
                                 SocketOption(..), SockAddr(..), 
                                 iNADDR_ANY, maxListenQueue, SocketType(..), 
                                 bindSocket)
import qualified Network.Socket as Socket (listen)
import System.IO
import System.IO.Error (isFullError)
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
  tt <- TT.new
  ttid <- timeoutThread tt
  let work (s,hn,p) = do -- hSetBuffering h NoBuffering
                         let eh (x::SomeException) = when ((fromException x) /= Just ThreadKilled) $ log' ERROR ("HTTP request failed with: " ++ show x)
                         tid <- myThreadId
                         let thandle = TimeoutHandle (hashString (show tid)) tid tt
                         tickleTimeout thandle
                         request thandle conf s (hn,fromIntegral p) hand `E.catch` eh
                         -- remove thread from timeout table
                         cancelTimeout thandle
                         sClose s
      loop = forever $ do w <- acceptLite s
                          forkIO $ work w
      pe e = log' ERROR ("ERROR in accept thread: " ++ show e)
      infi = loop `catchSome` pe >> infi
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
            Handler $ \(e :: ArrayException) -> h (toException e),
            Handler $ \(e :: IOException)    ->
                if isFullError e
                   then return () -- h (toException e) -- we could log the exception, but there could be thousands of them
                   else throw e
          ]
