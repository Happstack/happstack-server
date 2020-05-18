{-# LANGUAGE CPP #-}
module Happstack.Server.Internal.Socket
    ( acceptLite
    , sockAddrToPeer
    ) where

import Data.List (intersperse)
import Data.Word (Word32)
import qualified Network.Socket as S
  ( Socket
  , PortNumber()
  , SockAddr(..)
  , HostName
  , accept
  )
import Numeric (showHex)

type HostAddress = Word32
type HostAddress6 = (Word32, Word32, Word32, Word32)

-- | Converts a HostAddress to a String in dot-decimal notation
showHostAddress :: HostAddress -> String
showHostAddress num = concat [show q1, ".", show q2, ".", show q3, ".", show q4]
  where (num',q1)   = num `quotRem` 256
        (num'',q2)  = num' `quotRem` 256
        (num''',q3) = num'' `quotRem` 256
        (_,q4)      = num''' `quotRem` 256

-- | Converts a IPv6 HostAddress6 to standard hex notation
showHostAddress6 :: HostAddress6 -> String
showHostAddress6 (a,b,c,d) =
  (concat . intersperse ":" . map (flip showHex ""))
    [p1,p2,p3,p4,p5,p6,p7,p8]
  where (a',p2) = a `quotRem` 65536
        (_,p1)  = a' `quotRem` 65536
        (b',p4) = b `quotRem` 65536
        (_,p3)  = b' `quotRem` 65536
        (c',p6) = c `quotRem` 65536
        (_,p5)  = c' `quotRem` 65536
        (d',p8) = d `quotRem` 65536
        (_,p7)  = d' `quotRem` 65536

-- | alternative implementation of accept to work around EAI_AGAIN errors
acceptLite :: S.Socket -> IO (S.Socket, S.HostName, S.PortNumber)
acceptLite sock = do
  (sock', addr) <- S.accept sock
  let (peer, port) = sockAddrToPeer addr
  return (sock', peer, port)

sockAddrToPeer ::  S.SockAddr -> (S.HostName, S.PortNumber)
sockAddrToPeer addr =
  case addr of
    (S.SockAddrInet p ha)      -> (showHostAddress ha, p)
    (S.SockAddrInet6 p _ ha _) -> (showHostAddress6 ha, p)
    _                          -> error "sockAddrToPeer: Unsupported socket type"
