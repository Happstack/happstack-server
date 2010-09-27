-- | Support for validating server output on-the-fly. Validators can be configured on a per content-type basis.
module Happstack.Server.Validation where

import Control.Concurrent                        (forkIO)
import Control.Exception                         (evaluate)
import Control.Monad.Trans                       (MonadIO(liftIO))
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as L
import Happstack.Server.HTTP.Types               (Conf(..), Response(..), getHeader, nullConf)
import Happstack.Server.Response                 (ToMessage, toResponse)
import System.Exit                               (ExitCode(ExitSuccess, ExitFailure))
import System.IO                                 (hGetContents, hClose)
import System.Process                            (runInteractiveProcess, waitForProcess)

-- | Set the validator which should be used for this particular
-- 'Response' when validation is enabled.
--
-- Calling this function does not enable validation. That can only be
-- done by enabling the validation in the 'Conf' that is passed to
-- 'simpleHTTP'.
--
-- You do not need to call this function if the validator set in
-- 'Conf' does what you want already.
--
-- Example: (use 'noopValidator' instead of the default supplied by
-- 'validateConf')
--
-- > simpleHTTP validateConf . anyRequest $ ok . setValidator noopValidator =<< htmlPage
--
-- See also: 'validateConf', 'wdgHTMLValidator', 'noopValidator',
-- 'lazyProcValidator'.
setValidator :: (Response -> IO Response) -> Response -> Response
setValidator v r = r { rsValidator = Just v }

-- | 'ServerPart' version of 'setValidator'.
--
-- Example: (Set validator to 'noopValidator')
--
-- >  simpleHTTP validateConf $ setValidatorSP noopValidator (dir "ajax" ... )
--
setValidatorSP :: (Monad m, ToMessage r) => (Response -> IO Response) -> m r -> m Response
setValidatorSP v sp = return . setValidator v . toResponse =<< sp

-- | Extend 'nullConf' by enabling validation and setting
-- 'wdgHTMLValidator' as the default validator for @text\/html@.
--
-- Example:
--
-- > simpleHTTP validateConf . anyRequest $ ok htmlPage
--
validateConf :: Conf
validateConf = nullConf { validator = Just wdgHTMLValidator }

-- | Actually perform the validation on a 'Response'.
--
-- Run the validator specified in the 'Response'. If none is provide
-- use the supplied default instead.
--
-- Note: This function will run validation unconditionally. You
-- probably want 'setValidator' or 'validateConf'.
runValidator :: (Response -> IO Response) -> Response -> IO Response
runValidator defaultValidator r =
    case rsValidator r of
      Nothing -> defaultValidator r
      (Just altValidator) -> altValidator r

-- | Validate @text\/html@ content with @WDG HTML Validator@.
--
-- This function expects the executable to be named @validate@ and it
-- must be in the default @PATH@.
--
-- See also: 'setValidator', 'validateConf', 'lazyProcValidator'.
wdgHTMLValidator :: (MonadIO m, ToMessage r) => r -> m Response
wdgHTMLValidator = liftIO . lazyProcValidator "validate" ["-w","--verbose","--charset=utf-8"] Nothing Nothing handledContentTypes . toResponse
    where
      handledContentTypes (Just ct) = elem (takeWhile (\c -> c /= ';' && c /= ' ') (B.unpack ct)) [ "text/html", "application/xhtml+xml" ]
      handledContentTypes Nothing = False

-- | A validator which always succeeds.
--
-- Useful for selectively disabling validation. For example, if you
-- are sending down HTML fragments to an AJAX application and the
-- default validator only understands complete documents.
noopValidator :: Response -> IO Response
noopValidator = return

-- | Validate the 'Response' using an external application.
--
-- If the external application returns 0, the original response is
-- returned unmodified. If the external application returns non-zero,
-- a 'Response' containing the error messages and original response
-- body is returned instead.
--
-- This function also takes a predicate filter which is applied to the
-- content-type of the response. The filter will only be applied if
-- the predicate returns true.
--
-- NOTE: This function requires the use of -threaded to avoid
-- blocking.  However, you probably need that for Happstack anyway.
--
-- See also: 'wdgHTMLValidator'.
lazyProcValidator :: FilePath -- ^ name of executable
               -> [String] -- ^ arguments to pass to the executable
               -> Maybe FilePath -- ^ optional path to working directory
               -> Maybe [(String, String)] -- ^ optional environment (otherwise inherit)
               -> (Maybe B.ByteString -> Bool) -- ^ content-type filter
               -> Response -- ^ Response to validate
               -> IO Response
lazyProcValidator exec args wd env mimeTypePred response
    | mimeTypePred (getHeader "content-type" response) =
        do (inh, outh, errh, ph) <- runInteractiveProcess exec args wd env
           out <- hGetContents outh
           err <- hGetContents errh
           forkIO $ do L.hPut inh (rsBody response)
                       hClose inh
           forkIO $ evaluate (length out) >> return ()
           forkIO $ evaluate (length err) >> return ()
           ec <- waitForProcess ph
           case ec of
             ExitSuccess     -> return response
             (ExitFailure _) ->
                 return $ toResponse (unlines ([ "ExitCode: " ++ show ec
                                               , "stdout:"
                                               , out
                                               , "stderr:"
                                               , err
                                               , "input:"
                                               ] ++
                                               showLines (rsBody response)))
    | otherwise = return response
    where
      column = "  " ++ (take 120 $ concatMap  (\n -> "         " ++ show n) (drop 1 $ cycle [0..9::Int]))
      showLines :: L.ByteString -> [String]
      showLines string = column : zipWith (\n -> \l  -> show n ++ " " ++ (L.unpack l)) [1::Integer ..] (L.lines string)
