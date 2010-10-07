{-# OPTIONS -fno-cse #-}
module Happstack.Server.Internal.Clock(getApproximateTime) where

import Control.Concurrent
import Data.IORef
import System.IO.Unsafe
import System.Time
import System.Locale

import qualified Data.ByteString.Char8 as B

mkTime :: IO B.ByteString
mkTime = do now <- getClockTime
            return $ B.pack (formatCalendarTime defaultTimeLocale "%a, %d %b %Y %X GMT" (toUTCTime now))


{-# NOINLINE clock #-}
clock :: IORef B.ByteString
clock = unsafePerformIO $ do
  ref <- newIORef =<< mkTime
  forkIO $ updater ref
  return ref

updater :: IORef B.ByteString -> IO ()
updater ref = do threadDelay (10^(6 :: Int)) -- Every second
                 writeIORef ref =<< mkTime
                 updater ref

getApproximateTime :: IO B.ByteString
getApproximateTime = readIORef clock
