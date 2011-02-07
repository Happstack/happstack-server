{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- | 
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.Internal.TimeoutSocket where

import           Control.Concurrent            (ThreadId, forkIO, killThread, threadDelay, threadWaitWrite)
import           Control.Exception             (SomeException, catch, throw)
import           Control.Monad                 (liftM, when)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import qualified Network.Socket.ByteString as N
import           Data.Word
import           Data.IORef
import           Data.List (foldl')
import           Data.Time.Clock.POSIX(POSIXTime, getPOSIXTime)
import           Happstack.Server.Internal.Clock (getApproximatePOSIXTime)
import qualified Happstack.Server.Internal.TimeoutManager as TM
import           Network.Socket (Socket, ShutdownCmd(..), shutdown)
import           Network.Socket.SendFile (Iter(..), ByteCount, Offset, sendFileIterWith')
import           Network.Socket.ByteString (sendAll)
import           Prelude hiding (catch)
import           System.IO (Handle, hClose, hIsEOF, hWaitForInput)
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Unsafe (unsafeInterleaveIO)

sPutTickle :: TM.Handle -> Socket -> L.ByteString -> IO ()
sPutTickle thandle sock cs =
    do L.foldrChunks (\c rest -> sendAll sock c >> TM.tickle thandle >> rest) (return ()) cs
{-# INLINE sPutTickle #-}

sGetContents :: TM.Handle 
             -> Socket         -- ^ Connected socket
             -> IO L.ByteString  -- ^ Data received
sGetContents handle sock = loop where
  loop = unsafeInterleaveIO $ do
    s <- N.recv sock 65536
    TM.tickle handle
    if S.null s
      then do shutdown sock ShutdownReceive `catch` (\e -> when (not $ isDoesNotExistError e) (throw e))
              return L.Empty
      else L.Chunk s `liftM` loop


sendFileTickle :: TM.Handle -> Socket -> FilePath -> Offset -> ByteCount -> IO ()
sendFileTickle thandle outs fp offset count =
    sendFileIterWith' (iterTickle thandle) outs fp 65536 offset count

iterTickle :: TM.Handle -> IO Iter -> IO ()
iterTickle thandle = 
    iterTickle' 
    where
      iterTickle' :: (IO Iter -> IO ())
      iterTickle' iter =
          do r <- iter
             TM.tickle thandle
             case r of
               (Done _) ->
                      return ()
               (WouldBlock _ fd cont) ->
                   do threadWaitWrite fd
                      iterTickle' cont
               (Sent _ cont) ->
                   do iterTickle' cont
