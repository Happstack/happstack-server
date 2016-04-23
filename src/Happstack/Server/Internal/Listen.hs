{-# LANGUAGE BangPatterns, CPP, OverloadedStrings, ScopedTypeVariables #-}
module Happstack.Server.Internal.Listen(listen, listen',listenOn,listenOnIPv4) where

import Happstack.Server.Internal.Types          (Conf(..), Request, Response)
import Happstack.Server.Internal.Handler        (request)
import Happstack.Server.Internal.LogFormat      (__LOC__, logError, logNotice)
import Happstack.Server.Internal.Socket         (acceptLite)
import Happstack.Server.Internal.TimeoutManager (cancel, initialize, register, forceTimeoutAll)
import Happstack.Server.Internal.TimeoutSocket  as TS
import qualified Control.Concurrent.Thread.Group as TG
import Control.Exception.Extensible             as E
import Control.Concurrent                       (forkIO, killThread, myThreadId)
import Control.Monad
import Data.Monoid ((<>))
import Data.Text (pack, Text)
import Language.Haskell.TH (litE, stringL)
import Language.Haskell.TH.Syntax (location, loc_module)
import Network.BSD                              (getProtocolNumber)
import Network                                  (Socket)
import Network.Socket as Socket (SocketOption(KeepAlive), close, setSocketOption,
                                 socket, Family(..), SockAddr,
                                 SocketOption(..), SockAddr(..),
                                 iNADDR_ANY, maxListenQueue, SocketType(..),
                                 bindSocket)
import qualified Network.Socket                 as Socket (listen, inet_addr)
import System.IO.Error                          (isFullError)
{-
#ifndef mingw32_HOST_OS
-}
import System.Posix.Signals
{-
#endif
-}

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
        (close)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (fromIntegral portm) iNADDR_ANY)
            Socket.listen sock (max 1024 maxListenQueue)
            return sock
        )

listenOnIPv4 :: String  -- ^ IP address to listen on (must be an IP address not a host name)
             -> Int     -- ^ port number to listen on
             -> IO Socket
listenOnIPv4 ip portm = do
    proto <- getProtocolNumber "tcp"
    hostAddr <- Socket.inet_addr ip
    E.bracketOnError
        (socket AF_INET Stream proto)
        (close)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (fromIntegral portm) hostAddr)
            Socket.listen sock (max 1024 maxListenQueue)
            return sock
        )

-- | Bind and listen port
listen :: Conf -> (Request -> IO Response) -> IO ()
listen conf hand = do
    let port' = port conf
    lsocket <- listenOn port'
    setSocketOption lsocket KeepAlive 1
    listen' lsocket conf hand

-- | Use a previously bind port and listen
listen' :: Socket -> Conf -> (Request -> IO Response) -> IO ()
listen' s conf hand = do
{-
#ifndef mingw32_HOST_OS
-}
  void $ installHandler openEndedPipe Ignore Nothing
{-
#endif
-}
  let port' = port conf
      fork = case threadGroup conf of
               Nothing -> forkIO
               Just tg -> \m -> fst `liftM` TG.forkIO tg m
  tm <- initialize ((timeout conf) * (10^(6 :: Int)))
  -- http:// loop
  logNotice $__LOC__ ("Listening for http:// on port " <> pack (show port'))
  let eh (x::SomeException) = when ((fromException x) /= Just ThreadKilled) $ logError $__LOC__ ("HTTP request failed with: " <> pack (show x))
      work (sock, hn, p) =
          do tid <- myThreadId
             thandle <- register tm (killThread tid)
             let timeoutIO = TS.timeoutSocketIO thandle sock
             request timeoutIO (logAccess conf) (hn,fromIntegral p) hand `E.catch` eh
             -- remove thread from timeout table
             cancel thandle
             close sock
      loop = forever $ do w <- acceptLite s
                          fork $ work w
      pe e = logError $__LOC__ ("ERROR in http accept thread: " <> pack (show e))
      infi :: IO ()
      infi = loop `catchSome` pe >> infi

  infi `finally` (close s >> forceTimeoutAll tm)

{--
#ifndef mingw32_HOST_OS
-}
  void $ installHandler openEndedPipe Ignore Nothing
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
