{-# LANGUAGE TemplateHaskell #-}
module Happstack.Server.Internal.Socket(acceptLite) where

import Happstack.Server.Internal.SocketTH(supportsIPv6)
import Language.Haskell.TH.Syntax
import Happstack.Util.HostAddress
import qualified Network as N
  ( PortID(PortNumber)
  , socketPort
  )
import qualified Network.Socket as S
  ( Socket(..)
  , PortNumber()
  , SockAddr(..)
  , HostName
  , accept
  , socketToHandle
  )
import System.IO

-- | alternative implementation of accept to work around EAI_AGAIN errors
acceptLite :: S.Socket -> IO (S.Socket, S.HostName, S.PortNumber)
acceptLite sock = do
  (sock', addr) <- S.accept sock
  (N.PortNumber p) <- N.socketPort sock'
  
  let peer = $(if supportsIPv6
                 then
                 return $ CaseE (VarE (mkName "addr")) 
                            [Match 
                             (ConP (mkName "S.SockAddrInet") 
                              [WildP,VarP (mkName "ha")]) 
                             (NormalB (AppE (VarE (mkName "showHostAddress")) 
                                       (VarE (mkName "ha")))) []
                            ,Match (ConP (mkName "S.SockAddrInet6") [WildP,WildP,VarP (mkName "ha"),WildP])
                             (NormalB (AppE (VarE (mkName "showHostAddress6")) (VarE (mkName "ha")))) []
                            ,Match WildP (NormalB (AppE (VarE (mkName "error")) (LitE (StringL "Unsupported socket")))) []]
                 -- the above mess is the equivalent of this: 
                 {-[| case addr of
                       (S.SockAddrInet _ ha)      -> showHostAddress ha
                       (S.SockAddrInet6 _ _ ha _) -> showHostAddress6 ha
                       _                          -> error "Unsupported socket"
                   |]-}
                 else
                 [| case addr of
                      (S.SockAddrInet _ ha)      -> showHostAddress ha
                      _                          -> error "Unsupported socket"
                 |])
                     
  return (sock', peer, p)

