{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- | 
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.Internal.Timeout where

import           Control.Concurrent            (ThreadId, forkIO, killThread, threadDelay, threadWaitWrite)
import           Control.Exception             (catch, SomeException)
import           Control.Monad                 (liftM)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import qualified Network.Socket.ByteString as N
import           Data.DList (DList)
import qualified Data.DList                    as D
import           Data.Word
import           Data.IORef
import           Data.List (foldl')
import qualified Data.PSQueue as PSQ
import           Data.PSQueue (PSQ)
import           Data.Time.Clock.POSIX(POSIXTime, getPOSIXTime)
import           Happstack.Server.Internal.Clock (getApproximatePOSIXTime)
import qualified Happstack.Server.Internal.TimeoutTable as TT
import           Happstack.Server.Internal.TimeoutTable (TimeoutTable)
import           Network.Socket (Socket, ShutdownCmd(..), shutdown)
import           Network.Socket.SendFile (Iter(..), ByteCount, Offset, sendFileIterWith')
import           Network.Socket.ByteString (sendAll)
import           Prelude hiding (catch)
import           System.IO (Handle, hClose, hIsEOF, hWaitForInput)
import           System.IO.Unsafe (unsafeInterleaveIO)

data TimeoutHandle = TimeoutHandle
    { _threadHash      :: !Word
    , _threadId        :: !ThreadId
    , _timeoutTable    :: TimeoutTable
    }

timeoutThread :: TimeoutTable -> IO ThreadId
timeoutThread table = do
    forkIO $ loop `catch` (\(_::SomeException) -> killAll)

  where

    loop = do
--        debug "timeoutThread: waiting for activity on thread table"
        TT.waitForActivity table
--        debug "timeoutThread: woke up, killing old connections"
        killTooOld
        loop


    killTooOld = do
        now    <- getApproximatePOSIXTime
        TT.killOlderThan (now - tIMEOUT) table

    -- timeout = 30 seconds
    tIMEOUT = 30

    killAll = do
--        debug "Backend.timeoutThread: shutdown, killing all connections"
        TT.killAll table


tickleTimeout :: TimeoutHandle -> IO ()
tickleTimeout thandle = do
    now <- getApproximatePOSIXTime
    TT.insert thash tid now tt
        where
          thash = _threadHash   thandle
          tid   = _threadId     thandle
          tt    = _timeoutTable thandle
{-# INLINE tickleTimeout #-}

cancelTimeout :: TimeoutHandle -> IO ()
cancelTimeout thandle =
    TT.delete thash tid tt
        where
          thash = _threadHash   thandle
          tid   = _threadId     thandle
          tt    = _timeoutTable thandle
{-# INLINE cancelTimeout #-}

sPutTickle :: TimeoutHandle -> Socket -> L.ByteString -> IO ()
sPutTickle thandle sock cs =
    do L.foldrChunks (\c rest -> sendAll sock c >> tickleTimeout thandle >> rest) (return ()) cs
{-# INLINE sPutTickle #-}
{-
hGetContentsN :: Int -> TimeoutHandle -> Handle -> IO L.ByteString
hGetContentsN k thandle h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetNonBlocking h k
        if S.null c
          then do eof <- hIsEOF h
                  if eof then hClose h >> cancelTimeout thandle >> return L.Empty
                         else hWaitForInput h (-1)
                            >> loop

          --then hClose h >> return Empty
          else do tickleTimeout thandle
                  cs <- lazyRead
                  return (L.Chunk c cs)

hGetContents' :: TimeoutHandle -> Handle -> IO L.ByteString
hGetContents' thandle h = hGetContentsN L.defaultChunkSize thandle h
-}

sGetContents' :: TimeoutHandle -> Socket -> IO L.ByteString
sGetContents' thandle sock = sGetContents sock -- hGetContentsN L.defaultChunkSize thandle h

sGetContents :: Socket         -- ^ Connected socket
            -> IO L.ByteString  -- ^ Data received
sGetContents sock = loop where
  loop = unsafeInterleaveIO $ do
    s <- N.recv sock 65536
    if S.null s
      then shutdown sock ShutdownReceive >> return L.Empty
      else L.Chunk s `liftM` loop


sendFileTickle :: TimeoutHandle -> Socket -> FilePath -> Offset -> ByteCount -> IO ()
sendFileTickle thandle outs fp offset count =
    sendFileIterWith' (iterTickle thandle) outs fp 65536 offset count

iterTickle :: TimeoutHandle -> IO Iter -> IO ()
iterTickle thandle = 
    iterTickle' 
    where
      iterTickle' :: (IO Iter -> IO ())
      iterTickle' iter =
          do r <- iter
             tickleTimeout thandle
             case r of
               (Done _) ->
                      return ()
               (WouldBlock _ fd cont) ->
                   do threadWaitWrite fd
                      iterTickle' cont
               (Sent _ cont) ->
                   do iterTickle' cont
