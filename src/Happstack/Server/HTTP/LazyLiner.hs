{-# LANGUAGE ForeignFunctionInterface #-}
module Happstack.Server.HTTP.LazyLiner
    (Lazy, newLinerHandle, headerLines, getBytes, getBytesStrict, getRest, L.toChunks
    ) where

import Control.Concurrent.MVar
import System.IO
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L

newtype Lazy = Lazy (MVar L.ByteString)

newLinerHandle :: Handle -> IO Lazy
newLinerHandle h = fmap Lazy (newMVar =<< L.hGetContents h)

headerLines :: Lazy -> IO [P.ByteString]
headerLines (Lazy mv) = modifyMVar mv $ \l -> do
  let loop acc r0 = let (h,r) = L.break ((==) ch) r0
                        ph    = toStrict h
                        phl   = P.length ph
                        ph2   = if phl == 0 || P.last ph /= '\x0D' then ph else P.init ph
                        ch    = '\x0A'
                        r'    = if L.null r then r else L.tail r
                    in if P.length ph2 == 0 then (r', reverse acc) else loop (ph2:acc) r'
  return $ loop [] l

getBytesStrict :: Lazy -> Int -> IO P.ByteString
getBytesStrict (Lazy mv) len = modifyMVar mv $ \l -> do
  let (h,p) = L.splitAt (fromIntegral len) l
  return (p, toStrict h)

getBytes :: Lazy -> Int -> IO L.ByteString
getBytes (Lazy mv) len = modifyMVar mv $ \l -> do
  let (h,p) = L.splitAt (fromIntegral len) l
  return (p, h)

getRest :: Lazy -> IO L.ByteString
getRest (Lazy mv) = modifyMVar mv $ \l -> return (L.empty, l)

toStrict :: L.ByteString -> P.ByteString
toStrict = P.concat . L.toChunks
