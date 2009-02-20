{-# LANGUAGE TemplateHaskell #-}
module Happstack.Server.HTTP.Socket(acceptLite) where

import Happstack.Server.HTTP.SocketTH(supportsIPv6)
import Happstack.Util.HostAddress
import qualified Network as N
  ( PortID(PortNumber)
  , socketPort
  )
import qualified Network.Socket as S
  ( Socket(..)
  , PortNumber(PortNum)
  , SockAddr(..)
  , HostName(..)
  , accept
  , socketToHandle
  )
import System.IO

-- alternative implementation of accept to work around EAI_AGAIN errors
acceptLite :: S.Socket -> IO (Handle, S.HostName, S.PortNumber)
acceptLite sock = do
  (sock', addr) <- S.accept sock
  h <- S.socketToHandle sock' ReadWriteMode
  (N.PortNumber p) <- N.socketPort sock'
  
  let peer = $(if supportsIPv6
                 then
                 [| case addr of
                      (S.SockAddrInet _ ha)      -> showHostAddress ha
                      (S.SockAddrInet6 _ _ ha _) -> showHostAddress6 ha
                      _                        -> error "Unsupported socket"
                 |]
                 else
                 [| case addr of
                      (S.SockAddrInet _ ha)      -> showHostAddress ha
                      _                        -> error "Unsupported socket"
                 |])
                     
  return (h, peer, p)

