module Happstack.Server.Internal.TimeoutIO 
    ( TimeoutIO(..)
    ) where

import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as L
import Happstack.Server.Internal.TimeoutManager (Handle)
import Network.Socket.SendFile                  (ByteCount, Offset)


-- |TimeoutIO is a record which abstracts out all the network IO
-- functions needed by the request handling loop. This allows use to
-- use the same event loop for handle both http:// and https://.
data TimeoutIO = TimeoutIO
    { toHandle      :: Handle
    , toPutLazy     :: L.ByteString -> IO ()
    , toPut         :: B.ByteString -> IO ()
    , toGetContents :: IO L.ByteString
    , toSendFile    :: FilePath -> Offset -> ByteCount -> IO ()
    , toShutdown    :: IO ()
    , toSecure      :: Bool
    }
