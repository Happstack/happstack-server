{-# LANGUAGE BangPatterns #-}
{- | 
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.HTTP.Timeout where

import           Control.Concurrent            (ThreadId, forkIO, killThread, threadDelay, threadWaitWrite)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import           Data.DList (DList)
import qualified Data.DList                    as D
import           Data.IORef
import           Data.List (foldl')
import qualified Data.PSQueue as PSQ
import           Data.PSQueue (PSQ)
import           Data.Time.Clock.POSIX(POSIXTime, getPOSIXTime)
import           Network.Socket.SendFile (Iter(..), ByteCount, Offset, unsafeSendFileIterWith')
import           System.IO (Handle, hClose, hIsEOF, hWaitForInput)
import           System.IO.Unsafe (unsafeInterleaveIO)

type TimeoutTable = PSQ ThreadId POSIXTime
type TimeoutEdits = IORef (DList (TimeoutTable -> TimeoutTable))

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

tickleTimeout :: ThreadId -> TimeoutEdits -> IO ()
tickleTimeout tid tedits = do
    now <- getPOSIXTime
    atomicModifyIORef tedits $ \es -> (D.snoc es (PSQ.insert tid now), ())
{-# INLINE tickleTimeout #-}

cancelTimeout :: ThreadId -> TimeoutEdits -> IO ()
cancelTimeout tid tedits =
    atomicModifyIORef tedits $ \es -> (D.snoc es (PSQ.delete tid), ())
{-# INLINE cancelTimeout #-}

hPutTickle :: ThreadId -> TimeoutEdits -> Handle -> L.ByteString -> IO ()
hPutTickle tid tedits h cs =
    do L.foldrChunks (\c rest -> B.hPut h c >> tickleTimeout tid tedits >> rest) (return ()) cs
{-# INLINE hPutTickle #-}

hGetContentsN :: Int -> ThreadId -> TimeoutEdits -> Handle -> IO L.ByteString
hGetContentsN k tid tedits h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetNonBlocking h k
        if S.null c
          then do eof <- hIsEOF h
                  if eof then hClose h >> cancelTimeout tid tedits >> return L.Empty
                         else hWaitForInput h (-1)
                            >> loop

          --then hClose h >> return Empty
          else do tickleTimeout tid tedits
                  cs <- lazyRead
                  return (L.Chunk c cs)

hGetContents' :: ThreadId -> TimeoutEdits -> Handle -> IO L.ByteString
hGetContents' tid tedits h = hGetContentsN L.defaultChunkSize tid tedits h

unsafeSendFileTickle :: ThreadId -> TimeoutEdits -> Handle -> FilePath -> Offset -> ByteCount -> IO ()
unsafeSendFileTickle tid tedits outp fp offset count =
    unsafeSendFileIterWith' (iterTickle tid tedits) outp fp 65536 offset count

iterTickle :: ThreadId -> TimeoutEdits -> IO Iter -> IO ()
iterTickle tid tedits = 
    iterTickle' 
    where
      iterTickle' :: (IO Iter -> IO ())
      iterTickle' iter =
          do r <- iter
             tickleTimeout tid tedits
             case r of
               (Done _) ->
                      return ()
               (WouldBlock _ fd cont) ->
                   do threadWaitWrite fd
                      iterTickle' cont
               (Sent _ cont) ->
                   do iterTickle' cont
