{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
-- | Functions for creating, adding, and expiring cookies. To lookup cookie values see "Happstack.Server.RqData".
module Happstack.Server.Cookie
    ( Cookie(..)
    , mkCookie
    , addCookie
    , addCookies
    , expireCookie
    )
    where

import Happstack.Server.Monads      (FilterMonad, composeFilter)
import Happstack.Server.HTTP.Cookie (Cookie(..), mkCookie, mkCookieHeader)
import Happstack.Server.HTTP.Types  (Response, addHeader)
import Happstack.Util.Common        (Seconds)

-- | Add the cookie with a timeout to the response.
addCookie :: (FilterMonad Response m) => Seconds -> Cookie -> m ()
addCookie sec = (addHeaderM "Set-Cookie") . mkCookieHeader sec
    where
      addHeaderM a v = composeFilter $ \res-> addHeader a v res

-- | Add the list of cookie timeout pairs to the response.
addCookies :: (FilterMonad Response m) => [(Seconds, Cookie)] -> m ()
addCookies = mapM_ (uncurry addCookie)

-- | Expire the cookie immediately.
expireCookie :: (FilterMonad Response m) => String -> m () 
expireCookie cookieName = addCookie 0 (mkCookie cookieName "")
