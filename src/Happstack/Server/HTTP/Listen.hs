{-# LANGUAGE CPP, ScopedTypeVariables, PatternSignatures #-}
module Happstack.Server.HTTP.Listen(listen) where



import Happstack.Server.HTTP.Types
import Happstack.Server.HTTP.Handler

import Control.Exception.Extensible as E
import Control.Concurrent
import Network
import Network.Socket as Socket (
  PortNumber(..), SockAddr(SockAddrInet), accept, socketToHandle)
import System.IO
import Happstack.Util.HostAddress

{-
#ifndef mingw32_HOST_OS
-}
import System.Posix.Signals
{-
#endif
-}

import System.Log.Logger (Priority(..), logM)
log' = logM "Happstack.Server.HTTP.Listen"

-- alternative implementation of accept to work around EAI_AGAIN errors
acceptLite :: Socket -> IO (Handle, HostName, Socket.PortNumber)
acceptLite sock = do
  (sock', addr) <- Socket.accept sock
  h <- socketToHandle sock' ReadWriteMode
  (PortNumber p) <- Network.socketPort sock'
  let peer = case addr of
               (SockAddrInet _ ha) -> showHostAddress ha
               _                   -> error "Only IPV4 is supported"
  return (h, peer, p)

listen :: Conf -> (Request -> IO Response) -> IO ()
listen conf hand = do
{-
#ifndef mingw32_HOST_OS
-}
  installHandler openEndedPipe Ignore Nothing
{-
#endif
-}
  let port' = port conf
  log' NOTICE ("Listening on port " ++ show port')
  s <- listenOn $ PortNumber $ toEnum port'
  let work (h,hn,p) = do -- hSetBuffering h NoBuffering
                         let eh (x::SomeException) = log' ERROR ("HTTP request failed with: "++show x)
                         request conf h (hn,fromIntegral p) hand `E.catch` eh
                         hClose h
  let loop = do acceptLite s >>= forkIO . work
                loop
  let pe e = log' ERROR ("ERROR in accept thread: "++
                                                    show e)
  let infi = loop `catchSome` pe >> infi -- loop `E.catch` pe >> infi
  infi `finally` sClose s
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
