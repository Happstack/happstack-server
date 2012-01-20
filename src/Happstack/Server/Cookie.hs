{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
-- | Functions for creating, adding, and expiring cookies. To lookup cookie values see "Happstack.Server.RqData".
module Happstack.Server.Cookie
    ( Cookie(..)
    , CookieLife(..)
    , mkCookie
    , addCookie
    , addCookies
    , expireCookie
    )
    where

import Control.Monad.Trans              (MonadIO(..))
import Happstack.Server.Internal.Monads (FilterMonad, composeFilter)
import Happstack.Server.Internal.Cookie (Cookie(..), CookieLife(..), calcLife, mkCookie, mkCookieHeader)
import Happstack.Server.Types           (Response, addHeader)

-- | Add the 'Cookie' to 'Response'.
--
-- example
-- 
-- > main = simpleHTTP nullConf $
-- >   do addCookie Session (mkCookie "name" "value")
-- >      ok $ "You now have a session cookie."
--
-- see also: 'addCookies'
addCookie :: (MonadIO m, FilterMonad Response m) => CookieLife -> Cookie -> m ()
addCookie life cookie =
    do l <- liftIO $ calcLife life
       (addHeaderM "Set-Cookie") $ mkCookieHeader l cookie
    where
      addHeaderM a v = composeFilter $ \res-> addHeader a v res

-- | Add the list 'Cookie' to the 'Response'.
-- 
-- see also: 'addCookie'
addCookies :: (MonadIO m, FilterMonad Response m) => [(CookieLife, Cookie)] -> m ()
addCookies = mapM_ (uncurry addCookie)

-- | Expire the named cookie immediately and set the cookie value to @\"\"@
--
-- > main = simpleHTTP nullConf $
-- >   do expireCookie "name"
-- >      ok $ "The cookie has been expired."

expireCookie :: (MonadIO m, FilterMonad Response m) => String -> m () 
expireCookie name = addCookie Expired (mkCookie name "")
