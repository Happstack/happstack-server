{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}
module Happstack.Server.Internal.Listen(listen, listen',listenOn,listenOnIPv4) where

import Data.Maybe                               (isNothing)
import Happstack.Server.Internal.Types          (Conf(..), Request, Response)
import Happstack.Server.Internal.Handler        (request)
import Happstack.Server.Internal.Socket         (acceptLite)
import Happstack.Server.Internal.TimeoutManager (cancel, initialize, register, forceTimeoutAll)
import Happstack.Server.Internal.TimeoutSocket  as TS
import qualified Control.Concurrent.Thread.Group as TG
import Control.Exception.Extensible             as E
import Control.Concurrent                       (forkIO, killThread, myThreadId)
import Control.Monad
import qualified Data.Maybe as Maybe
import qualified Network.Socket                 as Socket
import System.IO.Error                          (isFullError)
import Foreign.C (CInt)
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

-- Meant to be TCP in practise.
-- See https://www.gnu.org/software/libc/manual/html_node/Creating-a-Socket.html
-- which says "zero is usually right".  It could theoretically be SCTP, but it
-- would be a bizarre system that defaults to SCTP over TCP.
proto :: CInt
proto = Socket.defaultProtocol

{-
   Network.listenOn binds randomly to IPv4 or IPv6 or both,
   depending on system and local settings.
   Lets make it use IPv4 only for now.
-}

listenOn :: Int -> IO Socket.Socket
listenOn portm = do
    E.bracketOnError
        (Socket.socket Socket.AF_INET Socket.Stream proto)
        (Socket.close)
        (\sock -> do
            Socket.setSocketOption sock Socket.ReuseAddr 1
            Socket.bind sock (Socket.SockAddrInet (fromIntegral portm) iNADDR_ANY)
            Socket.listen sock (max 1024 Socket.maxListenQueue)
            return sock
        )

listenOnIPv4 :: String  -- ^ IP address to listen on (must be an IP address not a host name)
             -> Int     -- ^ port number to listen on
             -> IO Socket.Socket
listenOnIPv4 ip portm = do
    hostAddr <- inet_addr ip
    E.bracketOnError
        (Socket.socket Socket.AF_INET Socket.Stream proto)
        (Socket.close)
        (\sock -> do
            Socket.setSocketOption sock Socket.ReuseAddr 1
            Socket.bind sock (Socket.SockAddrInet (fromIntegral portm) hostAddr)
            Socket.listen sock (max 1024 Socket.maxListenQueue)
            return sock
        )

inet_addr :: String -> IO Socket.HostAddress
inet_addr ip = do
  addrInfos <- Socket.getAddrInfo (Just Socket.defaultHints) (Just ip) Nothing
  let getHostAddress addrInfo = case Socket.addrAddress addrInfo of
        Socket.SockAddrInet _ hostAddress -> Just hostAddress
        _ -> Nothing
  maybe (fail "inet_addr: no HostAddress") pure
    . Maybe.listToMaybe
    $ Maybe.mapMaybe getHostAddress addrInfos

iNADDR_ANY :: Socket.HostAddress
iNADDR_ANY = 0

-- | Bind and listen port
listen :: Conf -> (Request -> IO Response) -> IO ()
listen conf hand = do
    let port' = port conf
    lsocket <- listenOn port'
    Socket.setSocketOption lsocket Socket.KeepAlive 1
    listen' lsocket conf hand

-- | Use a previously bind port and listen
listen' :: Socket.Socket -> Conf -> (Request -> IO Response) -> IO ()
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
  log' NOTICE ("Listening for http:// on port " ++ show port')
  let eh (x::SomeException) = when ((fromException x) /= Just ThreadKilled) $ log' ERROR ("HTTP request failed with: " ++ show x)
      work (sock, hn, p) =
          do tid <- myThreadId
             thandle <- register tm (killThread tid)
             let timeoutIO = TS.timeoutSocketIO thandle sock
             request timeoutIO (logAccess conf) (hn,fromIntegral p) hand `E.catch` eh
             -- remove thread from timeout table
             cancel thandle
             Socket.close sock
      loop = forever $ do w <- acceptLite s
                          fork $ work w
      pe e = log' ERROR ("ERROR in http accept thread: " ++ show e)
      infi :: IO ()
      infi = loop `catchSome` pe >> infi

  infi `finally` (Socket.close s >> when (isNothing $ threadGroup conf) (forceTimeoutAll tm))

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
