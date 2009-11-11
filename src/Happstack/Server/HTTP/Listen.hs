{-# LANGUAGE CPP, ScopedTypeVariables, PatternSignatures #-}
module Happstack.Server.HTTP.Listen(listen, listen') where

import Happstack.Server.HTTP.Types
import Happstack.Server.HTTP.Handler
import Happstack.Server.HTTP.Socket (acceptLite)
import Control.Exception.Extensible as E
import Control.Concurrent
import Network(PortID(..), listenOn, sClose, Socket)
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

-- | Bind and listen port
listen :: Conf -> (Request -> IO Response) -> IO ()
listen conf hand = do
    let port' = port conf
    socket <- listenOn (PortNumber $ toEnum port')
    listen' socket conf hand

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

