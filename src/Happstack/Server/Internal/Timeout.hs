{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- | 
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.Internal.Timeout where

import           Control.Concurrent            (ThreadId, forkIO, killThread, threadDelay, threadWaitWrite)
import           Control.Exception             (catch, SomeException)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import           Data.DList (DList)
import qualified Data.DList                    as D
import           Data.Word
import           Data.IORef
import           Data.List (foldl')
import qualified Data.PSQueue as PSQ
import           Data.PSQueue (PSQ)
import           Data.Time.Clock.POSIX(POSIXTime, getPOSIXTime)
import qualified Happstack.Server.Internal.TimeoutTable as TT
import           Happstack.Server.Internal.TimeoutTable (TimeoutTable)
import           Network.Socket.SendFile (Iter(..), ByteCount, Offset, unsafeSendFileIterWith')
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
        now    <- getPOSIXTime
        TT.killOlderThan (now - tIMEOUT) table

    -- timeout = 30 seconds
    tIMEOUT = 30

    killAll = do
--        debug "Backend.timeoutThread: shutdown, killing all connections"
        TT.killAll table

{-
timeoutThread :: TimeoutEdits -> TimeoutTable -> IO ThreadId
timeoutThread tedits timeoutTable  = forkIO $ loop timeoutTable
  where
    loop tt = do
        tt' <- killTooOld tt
        threadDelay (1000000)
        loop tt'


    killTooOld table = do
        -- atomic swap edit list
        now <- getPOSIXTime
        edits <- atomicModifyIORef tedits $ \t -> (D.empty, D.toList t)

        let table' = foldl' (flip ($)) table edits
        !t'   <- killOlderThan now table'
        return t'

    -- timeout = 60 seconds
    tIMEOUT = 60

    killOlderThan now !table = do
        let mmin = PSQ.findMin table
        maybe (return table)
              (\m -> if now - PSQ.prio m > tIMEOUT
                       then do
                           killThread $ PSQ.key m
                           killOlderThan now $ PSQ.deleteMin table
                       else return table)
              mmin
-}
tickleTimeout :: TimeoutHandle -> IO ()
tickleTimeout thandle = do
    now <- getPOSIXTime
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

hPutTickle :: TimeoutHandle -> Handle -> L.ByteString -> IO ()
hPutTickle thandle h cs =
    do L.foldrChunks (\c rest -> B.hPut h c >> tickleTimeout thandle >> rest) (return ()) cs
{-# INLINE hPutTickle #-}

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

unsafeSendFileTickle :: TimeoutHandle -> Handle -> FilePath -> Offset -> ByteCount -> IO ()
unsafeSendFileTickle thandle outp fp offset count =
    unsafeSendFileIterWith' (iterTickle thandle) outp fp 65536 offset count

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
