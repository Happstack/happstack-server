{-# LANGUAGE TemplateHaskell #-}
module Happstack.Server.Internal.SocketTH(supportsIPv6) where
import Language.Haskell.TH

import Data.List
import Data.Maybe
import Network.Socket(SockAddr(..))

-- find out at compile time if the SockAddr6 / HostAddress6 constructors are available
supportsIPv6 :: Bool
supportsIPv6 = $(let c = ["Network.Socket.SockAddrInet6", "Network.Socket.Internal.SockAddrInet6"] ; d = ''SockAddr in
                 do TyConI (DataD _ _ _ cs _) <- reify d
                    if isJust (find (\(NormalC n _) -> show n `elem` c) cs)
                       then [| True |]
                       else [| False |] )
