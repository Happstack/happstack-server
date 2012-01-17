{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- | 
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.Internal.TimeoutSocketTLS where

import           Control.Monad                 (liftM)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import qualified Happstack.Server.Internal.TimeoutManager as TM
import           Happstack.Server.Internal.TimeoutIO (TimeoutIO(..))
import           Network.Socket.SendFile (ByteCount, Offset)
import           OpenSSL.Session               (SSL)
import qualified OpenSSL.Session               as SSL
import           Prelude hiding (catch)
import           System.IO (IOMode(ReadMode), SeekMode(AbsoluteSeek), hSeek, withBinaryFile)
import           System.IO.Unsafe (unsafeInterleaveIO)

sPutLazyTickle :: TM.Handle -> SSL -> L.ByteString -> IO ()
sPutLazyTickle thandle ssl cs =
    do L.foldrChunks (\c rest -> SSL.write ssl c >> TM.tickle thandle >> rest) (return ()) cs
{-# INLINE sPutLazyTickle #-}

sPutTickle :: TM.Handle -> SSL -> B.ByteString -> IO ()
sPutTickle thandle ssl cs =
    do SSL.write ssl cs
       TM.tickle thandle
{-# INLINE sPutTickle #-}

sGetContents :: TM.Handle 
             -> SSL              -- ^ Connected socket
             -> IO L.ByteString  -- ^ Data received
sGetContents handle ssl = loop where
  loop = unsafeInterleaveIO $ do
    s <- SSL.read ssl 65536
    TM.tickle handle
    if S.null s
      then do -- SSL.shutdown ssl SSL.Unidirectional `catch` (\e -> when (not $ isDoesNotExistError e) (throw e))
              return L.Empty
      else L.Chunk s `liftM` loop

timeoutSocketIO :: TM.Handle -> SSL -> TimeoutIO
timeoutSocketIO handle ssl =
    TimeoutIO { toHandle      = handle
              , toShutdown    = SSL.shutdown ssl SSL.Bidirectional
              , toPutLazy     = sPutLazyTickle handle ssl
              , toPut         = sPutTickle     handle ssl
              , toGetContents = sGetContents   handle ssl
              , toSendFile    = sendFileTickle handle ssl
              , toSecure      = True
              }

sendFileTickle :: TM.Handle -> SSL -> FilePath -> Offset -> ByteCount -> IO ()
sendFileTickle thandle ssl fp offset count =
    do withBinaryFile fp ReadMode $ \h -> do
         hSeek h AbsoluteSeek offset
         c <- L.hGetContents h
         sPutLazyTickle thandle ssl (L.take (fromIntegral count) c)
