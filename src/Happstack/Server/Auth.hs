{-# LANGUAGE FlexibleContexts #-}
-- | Support for basic access authentication <http://en.wikipedia.org/wiki/Basic_access_authentication>
module Happstack.Server.Auth where

import Data.Foldable (foldl')
import Data.Bits (xor, (.|.))
import Data.Maybe (fromMaybe)
import Control.Monad                             (MonadPlus(mzero, mplus))
import Data.ByteString.Base64                    as Base64
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as B
import qualified Data.Map                        as M
import Happstack.Server.Monads                   (Happstack, escape, getHeaderM, setHeaderM)
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
basicAuth :: (Happstack m) =>
   String -- ^ the realm name
   -> M.Map String String -- ^ the username password map
   -> m a -- ^ the part to guard
   -> m a
basicAuth realmName authMap = basicAuthBy (validLoginPlaintext authMap) realmName


-- | Generalized version of 'basicAuth'.
--
-- The function that checks the username password combination must be
-- supplied as first argument.
--
-- example:
--
-- > main = simpleHTTP nullConf $
-- >  msum [ basicAuth' (validLoginPlaintext (fromList [("happstack","rocks")])) "127.0.0.1" $ ok "You are in the secret club"
-- >       , ok "You are not in the secret club."
-- >       ]
--
basicAuthBy :: (Happstack m) =>
   (B.ByteString -> B.ByteString -> Bool) -- ^ function that returns true if the name password combination is valid
   -> String -- ^ the realm name
   -> m a -- ^ the part to guard
   -> m a
basicAuthBy validLogin realmName xs = basicAuthImpl `mplus` xs
  where
    basicAuthImpl = do
        aHeader <- getHeaderM "authorization"
        case aHeader of
            Nothing -> err
            Just x ->
                do (name, password) <- parseHeader x
                   if B.length password > 0
                      && B.head password == ':'
                      && validLogin name (B.tail password)
                     then mzero
                     else err
    parseHeader h =
      case Base64.decode . B.drop 6 $ h of
        (Left _)   -> err
        (Right bs) -> return (B.break (':'==) bs)
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""
    err :: (Happstack m) => m a
    err = escape $ do
            setHeaderM headerName headerValue
            unauthorized $ toResponse "Not authorized"


-- | Function that looks up the plain text password for username in a
-- Map and returns True if it matches with the given password.
--
-- Note: The implementation is hardened against timing attacks but not
-- completely safe. Ideally you should build your own predicate, using
-- a robust constant-time equality comparison from a cryptographic
-- library like sodium.
validLoginPlaintext ::
  M.Map String String -- ^ the username password map
  -> B.ByteString -- ^ the username
  -> B.ByteString -- ^ the password
  -> Bool
validLoginPlaintext authMap name password = M.lookup (B.unpack name) authMap == Just (B.unpack password)
validLoginPlaintext authMap name password = fromMaybe False $ do
    r <- M.lookup (B.unpack name) authMap
    pure (constTimeEq (B.pack r) password)
  where
    -- (Mostly) constant time equality of bytestrings to prevent timing attacks by testing out passwords. This still
    -- allows to extract the length of the configured password via timing attacks. This implementation is still brittle
    -- in the sense that it relies on GHC not unrolling or vectorizing the loop.
    {-# NOINLINE constTimeEq #-}
    constTimeEq :: BS.ByteString -> BS.ByteString -> Bool
    constTimeEq x y
      | BS.length x /= BS.length y
      = False

      | otherwise
      = foldl' (.|.) 0 (BS.zipWith xor x y) == 0
