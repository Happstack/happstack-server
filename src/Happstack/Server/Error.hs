module Happstack.Server.Error where

import Control.Monad.Error          (Error, ErrorT(runErrorT))
import Happstack.Server.Monads      (ServerPartT)
import Happstack.Server.Scary       (WebT, UnWebT, withRequest, mkWebT, runServerPartT, ununWebT)
import Happstack.Server.Response    (ok, toResponse)
import Happstack.Server.HTTP.Types  (Request, Response)

--------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------

-- | This 'ServerPart' modifier enables the use of 'throwError' and
-- 'catchError' inside the 'WebT' actions, by adding the 'ErrorT'
-- monad transformer to the stack.
--
-- You can wrap the complete second argument to 'simpleHTTP' in this
-- function.
--
-- DEPRECATED: use 'spUnwrapErrorT' instead.
errorHandlerSP :: (Monad m, Error e) => (Request -> e -> WebT m a) -> ServerPartT (ErrorT e m) a -> ServerPartT m a
errorHandlerSP handler sps = withRequest $ \req -> mkWebT $ do
			eer <- runErrorT $ ununWebT $ runServerPartT sps req
			case eer of
				Left err -> ununWebT (handler req err)
				Right res -> return res
{-# DEPRECATED errorHandlerSP "Use spUnwrapErrorT" #-}

-- | An example error Handler to be used with 'spUnwrapErrorT', which
-- returns the error message as a plain text message to the browser.
--
-- Another possibility is to store the error message, e.g. as a
-- FlashMsg, and then redirect the user somewhere.
simpleErrorHandler :: (Monad m) => String -> ServerPartT m Response
simpleErrorHandler err = ok $ toResponse $ ("An error occured: " ++ err)

-- | This is a for use with 'mapServerPartT'' It it unwraps the
-- interior monad for use with 'simpleHTTP'.  If you have a
-- @'ServerPartT' ('ErrorT' e m) a@, this will convert that monad into
-- a @'ServerPartT' m a@.  Used with 'mapServerPartT'' to allow
-- 'throwError' and 'catchError' inside your monad.  Eg.
--
-- > simpleHTTP conf $ mapServerPartT' (spUnWrapErrorT failurePart)  $ myPart `catchError` errorPart
--
-- Note that @failurePart@ will only be run if @errorPart@ threw an
-- error so it doesn\'t have to be very complex.
spUnwrapErrorT:: Monad m => (e -> ServerPartT m a)
              -> Request
              -> UnWebT (ErrorT e m) a
              -> UnWebT m a
spUnwrapErrorT handler rq = \x -> do
    err <- runErrorT x
    case err of
        Left e -> ununWebT $ runServerPartT (handler e) rq
        Right a -> return a

