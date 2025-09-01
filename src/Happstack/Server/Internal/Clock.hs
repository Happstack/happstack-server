{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-cse #-}
module Happstack.Server.Internal.Clock
    ( getApproximateTime
    , getApproximatePOSIXTime
    , getApproximateUTCTime
    , formatHttpDate
    ) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Time.Clock       (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO.Unsafe

data DateCache = DateCache {
      cachedPOSIXTime :: !(IORef POSIXTime)
    , cachedHttpDate  :: !(IORef B.ByteString)
    }

formatHttpDate :: UTCTime -> String
formatHttpDate = formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"
{-# INLINE formatHttpDate #-}

mkTime :: IO (POSIXTime, B.ByteString)
mkTime =
    do now <- getPOSIXTime
       return (now, B.pack $ formatHttpDate (posixSecondsToUTCTime now))

{-# NOINLINE clock #-}
clock :: DateCache
clock = unsafePerformIO $ do
  (now, httpDate) <- mkTime
  nowRef      <- newIORef now
  httpDateRef <- newIORef httpDate
  let dateCache = (DateCache nowRef httpDateRef)
  void $ forkIO $ updater dateCache
  return dateCache

updater :: DateCache -> IO ()
updater dateCache =
    do threadDelay (10^(6 :: Int)) -- Every second
       (now, httpDate) <- mkTime
       writeIORef (cachedPOSIXTime dateCache) now
       writeIORef (cachedHttpDate  dateCache) httpDate
       updater dateCache

getApproximateTime :: IO B.ByteString
getApproximateTime = readIORef (cachedHttpDate clock)

getApproximatePOSIXTime :: IO POSIXTime
getApproximatePOSIXTime = readIORef (cachedPOSIXTime clock)

getApproximateUTCTime :: IO UTCTime
getApproximateUTCTime = posixSecondsToUTCTime <$> getApproximatePOSIXTime
