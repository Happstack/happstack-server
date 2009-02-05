{-# LANGUAGE CPP, ScopedTypeVariables, PatternSignatures #-}
module HAppS.Server.HTTP.Listen(listen) where

import System.Log.Logger

import HAppS.Server.HTTP.Types
import HAppS.Server.HTTP.Handler

import Control.Exception.Extensible as E
import Control.Concurrent
import Network
import Network.Socket as Socket hiding (listen)
import System.IO

{-
#ifndef mingw32_HOST_OS
-}
import System.Posix.Signals
{-
#endif
-}

-- alternative implementation of accept to work around EAI_AGAIN errors
acceptLite :: Socket -> IO (Handle, HostName, Socket.PortNumber)
acceptLite sock = do
  (sock', addr) <- Socket.accept sock
  (Just peer, _) <- getNameInfo [NI_NUMERICHOST] True False addr
  h <- socketToHandle sock' ReadWriteMode
  (PortNumber p) <- Network.socketPort sock'
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
  s <- listenOn $ PortNumber $ toEnum $ port conf
  let work (h,hn,p) = do -- hSetBuffering h NoBuffering
                         let eh (x::SomeException) = logM "HAppS.Server.HTTP.Listen" ERROR ("HTTP request failed with: "++show x)
                         request conf h (hn,fromIntegral p) hand `E.catch` eh
                         hClose h
  let loop = do acceptLite s >>= forkIO . work
                loop
  let pe e = logM "HAppS.Server.HTTP.Listen" ERROR ("ERROR in accept thread: "++
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
