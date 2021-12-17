{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- |
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.Internal.TimeoutSocket where

import           Control.Applicative           (pure)
import           Control.Concurrent            (threadWaitWrite)
import           Control.Exception             as E (catch, throw)
import           Control.Monad                 (liftM, when)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import           Network.Socket                (close)
import qualified Network.Socket.ByteString     as N
import qualified Happstack.Server.Internal.TimeoutManager as TM
import           Happstack.Server.Internal.TimeoutIO (TimeoutIO(..))
import           Network.Socket (Socket, ShutdownCmd(..), shutdown)
import           Network.Sendfile (sendfile)
import           Network.Socket.ByteString (sendAll)
import           System.IO.Error (isDoesNotExistError, ioeGetErrorType)
import           System.IO.Unsafe (unsafeInterleaveIO)
import           GHC.IO.Exception (IOErrorType(InvalidArgument))

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

sGet :: TM.Handle
     -> Socket
     -> IO (Maybe B.ByteString)
sGet handle socket =
  do s <- N.recv socket 65536
     TM.tickle handle
     if S.null s
       then pure Nothing
       else pure (Just s)

sGetContents :: TM.Handle
             -> Socket         -- ^ Connected socket
             -> IO L.ByteString  -- ^ Data received
sGetContents handle sock = loop where
  loop = unsafeInterleaveIO $ do
    s <- N.recv sock 65536
    TM.tickle handle
    if S.null s
      then do
        -- 'InvalidArgument' is GHCs code for eNOTCONN (among other
        -- things). Sometimes the other end of socket is closed first
        -- and this end is already disconnected before we do
        -- 'shutdown'. Ignore this exception.
        shutdown sock ShutdownReceive `E.catch`
                    (\e -> when (not (isDoesNotExistError e || ioeGetErrorType e == InvalidArgument)) (throw e))
        return L.Empty
      else L.Chunk s `liftM` loop

timeoutSocketIO :: TM.Handle -> Socket -> TimeoutIO
timeoutSocketIO handle socket =
    TimeoutIO { toHandle      = handle
              , toShutdown    = close socket
              , toPutLazy     = sPutLazyTickle handle socket
              , toGet         = sGet           handle socket
              , toPut         = sPutTickle     handle socket
              , toGetContents = sGetContents   handle socket
              , toSendFile    = \filepath range -> sendfile socket filepath range (pure ())
              , toSecure      = False
              }
