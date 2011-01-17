-- | Some useful functions if you want to wrap the 'ServerPartT' monad transformer around the 'ErrorT' monad transformer. e.g., @'ServerPartT' ('ErrorT' e m) a@. This allows you to use 'throwError' and 'catchError' inside your monad.  
module Happstack.Server.Error where

import Control.Monad.Error              (Error, ErrorT(runErrorT))
import Happstack.Server.Monads          (ServerPartT)
import Happstack.Server.Internal.Monads (WebT, UnWebT, withRequest, mkWebT, runServerPartT, ununWebT)
import Happstack.Server.Response        (ok, toResponse)
import Happstack.Server.Types           (Request, Response)

--------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------

-- | Flatten @'ServerPartT' ('ErrorT' e m) a@ into a @'ServerPartT' m
-- a@ so that it can be use with 'simpleHTTP'.  Used with
-- 'mapServerPartT'', e.g.,
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

-- | A simple error handler which can be used with 'spUnwrapErrorT'.
-- 
-- It returns the error message as a plain text message to the
-- browser. More sophisticated behaviour can be achieved by calling
-- your own custom error handler instead.
simpleErrorHandler :: (Monad m) => String -> ServerPartT m Response
simpleErrorHandler err = ok $ toResponse $ ("An error occured: " ++ err)

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
