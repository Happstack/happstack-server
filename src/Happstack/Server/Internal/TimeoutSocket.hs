{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- | 
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.Internal.TimeoutSocket where

import           Control.Concurrent            (threadWaitWrite)
import           Control.Exception             (catch, throw)
import           Control.Monad                 (liftM, when)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import           Network.Socket                (sClose)
import qualified Network.Socket.ByteString     as N
import qualified Happstack.Server.Internal.TimeoutManager as TM
import           Happstack.Server.Internal.TimeoutIO (TimeoutIO(..))
import           Network.Socket (Socket, ShutdownCmd(..), shutdown)
import           Network.Socket.SendFile (Iter(..), ByteCount, Offset, sendFileIterWith')
import           Network.Socket.ByteString (sendAll)
import           Prelude hiding (catch)
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Unsafe (unsafeInterleaveIO)

sPutLazyTickle :: TM.Handle -> Socket -> L.ByteString -> IO ()
sPutLazyTickle thandle sock cs =
    do L.foldrChunks (\c rest -> sendAll sock c >> TM.tickle thandle >> rest) (return ()) cs
{-# INLINE sPutLazyTickle #-}

sPutTickle :: TM.Handle -> Socket -> B.ByteString -> IO ()
sPutTickle thandle sock cs =
    do sendAll sock cs
       TM.tickle thandle
       return ()
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

timeoutSocketIO :: TM.Handle -> Socket -> TimeoutIO
timeoutSocketIO handle socket =
    TimeoutIO { toHandle      = handle
              , toShutdown    = sClose socket 
              , toPutLazy     = sPutLazyTickle handle socket
              , toPut         = sPutTickle     handle socket
              , toGetContents = sGetContents   handle socket
              , toSendFile    = sendFileTickle handle socket
              , toSecure      = False
              }
