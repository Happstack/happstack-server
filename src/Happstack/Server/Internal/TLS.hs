{-# LANGUAGE CPP #-}
{- | core functions and types for HTTPS support
-}
module Happstack.Server.Internal.TLS where

#ifdef DISABLE_HTTPS
import Network.Socket (Socket)
#else
import Control.Monad  (when)
import Happstack.Server.Internal.Socket (acceptLite)
import Network.Socket (HostName, PortNumber, Socket)
import OpenSSL.Session (SSL, SSLContext)
import qualified OpenSSL.Session as SSL
#endif

-- | configuration for using https:\/\/
data TLSConf = TLSConf {
      tlsPort :: Int        -- port (usually 443)
    , tlsCert :: FilePath   -- path to SSL certificate
    , tlsKey  :: FilePath   -- path to SSL private key
    }

#ifdef DISABLE_HTTPS
data HTTPS = HTTPS
httpsOnSocket :: FilePath  -- ^ path to ssl certificate
              -> FilePath  -- ^ path to ssl private key
              -> Socket    -- ^ listening socket (on which listen() has been called, but not accept())
              -> IO HTTPS
httpsOnSocket = error "happstack-server was compiled with disable_https."
#else
-- | record that holds the 'Socket' and 'SSLContext' needed to start
-- the https:\/\/ event loop. Used with 'simpleHTTPWithSocket''
--
-- see also: 'httpOnSocket'
data HTTPS = HTTPS
    { httpsSocket :: Socket
    , sslContext :: SSLContext
    }

-- | generate the 'HTTPS' record needed to start the https:\/\/ event loop
--
httpsOnSocket :: FilePath  -- ^ path to ssl certificate
              -> FilePath  -- ^ path to ssl private key
              -> Socket    -- ^ listening socket (on which listen() has been called, but not accept())
              -> IO HTTPS
httpsOnSocket cert key socket =
    do ctx <- SSL.context
       SSL.contextSetPrivateKeyFile  ctx key
       SSL.contextSetCertificateFile ctx cert
       SSL.contextSetDefaultCiphers  ctx

       b <- SSL.contextCheckPrivateKey ctx
       when (not b) $ error $ "OpenTLS certificate and key do not match."

       return (HTTPS socket ctx)

acceptTLS :: HTTPS -> IO (SSL, HostName, PortNumber)
acceptTLS (HTTPS sck' ctx) =
    do -- do normal accept
      (sck, peer, port) <- acceptLite sck'

      --  then TLS accept
      ssl <- SSL.connection ctx sck
      SSL.accept ssl

      return (ssl, peer, port)
#endif