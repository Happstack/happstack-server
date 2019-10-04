-- | a very simple interface for acting as an HTTP client. This is mostly used for things like "Happstack.Server.Proxy". You are more likely to want a library like http-enumerator <http://hackage.haskell.org/package/http-enumerator>.
module Happstack.Server.Client where

import Happstack.Server.Internal.Handler    (parseResponse, putRequest)
import Happstack.Server.Internal.Types      (Response, Request, getHeader, readDec')
import Data.Maybe                           (fromJust)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L 
import System.IO                            (BufferMode(NoBuffering), hFlush, hSetBuffering)

-- | Sends the serialized request to the host defined in the request
-- and attempts to parse response upon arrival.
getResponse :: Request -> IO (Either String Response)
getResponse _ = error "Happstack.Server.Client.getResponse"
