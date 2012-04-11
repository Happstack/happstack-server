{-# LANGUAGE CPP #-}
#ifdef TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Happstack.Server.Internal.SocketTH(supportsIPv6) where

#ifdef TEMPLATE_HASKELL
import Language.Haskell.TH
#endif

import Network.Socket(SockAddr(..))

-- find out at compile time if the SockAddr6 / HostAddress6 constructors are available
supportsIPv6 :: Bool
#ifdef TEMPLATE_HASKELL
supportsIPv6 = $(let c = ["Network.Socket.SockAddrInet6", "Network.Socket.Internal.SockAddrInet6"] ; d = ''SockAddr
                     isInet6 :: Con -> Bool
                     isInet6 (NormalC n _) = show n `elem` c
                     isInet6 _             = False
                 in
                 do info <- reify d
                    case info of
                      TyConI (DataD _ _ _ cs _) ->
                        if any isInet6 cs
                          then [| True |]
                          else [| False |]
                      _ -> error "supportsIPv6: SockAddr is no longer a TyConI ?!?! Giving up."
                )
#else
supportsIPv6 = False
#endif
