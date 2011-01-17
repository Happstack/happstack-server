{-# LANGUAGE FlexibleContexts #-}
-- | Support for basic access authentication <http://en.wikipedia.org/wiki/Basic_access_authentication>
module Happstack.Server.Auth where

import Control.Monad                             (MonadPlus(mzero, mplus))
import qualified Data.ByteString.Char8           as B
import qualified Data.Map                        as M
import qualified Happstack.Crypto.Base64         as Base64
import Happstack.Server.Monads                   (FilterMonad, ServerMonad, WebMonad, escape, getHeaderM, setHeaderM)
import Happstack.Server.Types                    (Response)
import Happstack.Server.Response                 (unauthorized, toResponse)

-- | A simple HTTP basic authentication guard.
--
-- If authentication fails, this part will call 'mzero'.
-- 
-- example:
--
-- > main = simpleHTTP nullConf $ 
-- >  msum [ basicAuth "127.0.0.1" (fromList [("happstack","rocks")]) $ ok "You are in the secret club"
-- >       , ok "You are not in the secret club." 
-- >       ]
-- 
basicAuth :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadPlus m) =>
   String -- ^ the realm name
   -> M.Map String String -- ^ the username password map
   -> m a -- ^ the part to guard
   -> m a
basicAuth realmName authMap xs = basicAuthImpl `mplus` xs
  where
    basicAuthImpl = do
        aHeader <- getHeaderM "authorization"
        case aHeader of
            Nothing -> err
            Just x -> case parseHeader x of
                (name, ':':password) | validLogin name password -> mzero
                                     | otherwise -> err
                _  -> err
    validLogin name password = M.lookup name authMap == Just password
    parseHeader = break (':'==) . Base64.decode . B.unpack . B.drop 6
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""
    err = escape $ do
            setHeaderM headerName headerValue
            unauthorized $ toResponse "Not authorized"
