{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
-- | Functions for extracting values from the query string, form data, cookies, etc. 
--
-- For in-depth documentation see the following section of the Happstack Crash Course:
--
-- <http://happstack.com/docs/crashcourse/RqData.html>
module Happstack.Server.RqData 
    ( -- * Looking up keys
      -- ** Form Values and Query Parameters
      look
    , looks
    , lookText
    , lookTexts
    , lookBS
    , lookBSs
    , lookRead
    , lookReads
    , lookFile
    , lookPairs
    , lookPairsBS
    -- ** Cookies
    , lookCookie
    , lookCookieValue
    , readCookieValue
    -- ** low-level
    , lookInput
    , lookInputs
    -- * Filters
    -- The look* functions normally search the QUERY_STRING and the Request
    -- body for matches keys. 
    , body
    , queryString
    -- * Validation and Parsing
    , checkRq
    , checkRqM        
    , readRq
    -- * Handling POST\/PUT Requests
    , decodeBody
    -- ** Body Policy
    , BodyPolicy(..)
    , defaultBodyPolicy
    -- * RqData Monad & Error Reporting
    , RqData
    , mapRqData
    , Errors(..)       
    -- ** Using RqData with ServerMonad
    , getDataFn
    , withDataFn
    , FromData(..)
    , getData
    , withData
    -- * HasRqData class
    , RqEnv
    , HasRqData(askRqEnv, localRqEnv,rqDataError)
    ) where

import Control.Applicative 			(Applicative((<*>), pure), Alternative((<|>), empty), WrappedMonad(WrapMonad, unwrapMonad), (<$>))
import Control.Concurrent.MVar                  (newMVar)
import Control.Monad 				(MonadPlus(mzero), liftM)
import Control.Monad.Reader 			(ReaderT(ReaderT, runReaderT), MonadReader(ask, local), mapReaderT)
import Control.Monad.Error 			(Error(noMsg, strMsg))
import Control.Monad.Trans                      (MonadIO(..))
import qualified Data.ByteString.Char8          as P
import qualified Data.ByteString.Lazy.Char8     as L
import qualified Data.ByteString.Lazy.UTF8      as LU
import Data.Char 				(toLower)
import Data.Either                              (partitionEithers)
import Data.Generics                            (Data, Typeable)
import Data.Maybe                               (fromMaybe, fromJust)
import Data.Monoid 				(Monoid(mempty, mappend, mconcat))
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy.Encoding        as Text
import Happstack.Server.Cookie 			(Cookie (cookieValue))
import Happstack.Server.Internal.Monads         (ServerMonad(askRq, localRq), FilterMonad, WebMonad, ServerPartT, escape)
import Happstack.Server.Internal.RFC822Headers  (parseContentType)
import Happstack.Server.Types                   (ContentType(..), Input(inputValue, inputFilename, inputContentType), Response, Request(rqInputsQuery, rqInputsBody, rqCookies, rqMethod), Method(POST,PUT), getHeader, readInputsBody)
import Happstack.Server.Internal.MessageWrap    (BodyPolicy(..), bodyInput, defaultBodyPolicy)
import Happstack.Server.Response                (internalServerError, requestEntityTooLarge, toResponse)

newtype ReaderError r e a = ReaderError { unReaderError :: ReaderT r (Either e) a }
    deriving (Functor, Monad, MonadPlus)

instance (Error e) => MonadReader r (ReaderError r e) where
    ask = ReaderError ask
    local f m = ReaderError $ local f (unReaderError m)

instance (Monoid e, Error e) => Applicative (ReaderError r e) where
    pure = return
    (ReaderError (ReaderT f)) <*> (ReaderError (ReaderT a)) 
        = ReaderError $ ReaderT $ \env -> (f env) `apEither` (a env)

instance (Monoid e, Error e) => Alternative (ReaderError r e) where
    empty = unwrapMonad empty
    f <|> g = unwrapMonad $ (WrapMonad f) <|> (WrapMonad g)

apEither :: (Monoid e) => Either e (a -> b) -> Either e a -> Either e b
apEither (Left errs1) (Left errs2) = Left (errs1 `mappend` errs2)
apEither (Left errs)  _            = Left errs
apEither _            (Left errs)  = Left errs
apEither (Right f)    (Right a)    = Right (f a)

-- | a list of errors
newtype Errors a = Errors { unErrors :: [a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Monoid (Errors a) where
    mempty = Errors []
    (Errors x) `mappend` (Errors y) = Errors (x ++ y)
    mconcat errs = Errors $ concatMap unErrors errs

instance Error (Errors String) where
    noMsg = Errors []
    strMsg str = Errors [str]

mapReaderErrorT :: (Either e a -> Either e' b) -> (ReaderError r e a) -> (ReaderError r e' b)
mapReaderErrorT f m = ReaderError $ mapReaderT f (unReaderError m)

readerError :: (Monoid e, Error e) => e -> ReaderError r e b
readerError e = mapReaderErrorT ((Left e) `apEither`) (return ())

runReaderError :: ReaderError r e a -> r -> Either e a
runReaderError = runReaderT . unReaderError

-- | the environment used to lookup query parameters. It consists of
-- the triple: (query string inputs, body inputs, cookie inputs)
type RqEnv = ([(String, Input)], Maybe [(String, Input)], [(String, Cookie)])

-- | An applicative functor and monad for looking up key/value pairs
-- in the QUERY_STRING, Request body, and cookies.
newtype RqData a = RqData { unRqData :: ReaderError RqEnv (Errors String) a }
    deriving (Functor, Monad, MonadPlus, Applicative, Alternative, MonadReader RqEnv )

-- | A class for monads which contain a 'RqEnv'
class HasRqData m where
    askRqEnv :: m RqEnv
    localRqEnv :: (RqEnv -> RqEnv) -> m a -> m a
    -- | lift some 'Errors' into 'RqData'
    rqDataError :: Errors String -> m a 

instance HasRqData RqData where
    askRqEnv    = RqData ask
    localRqEnv f (RqData re) = RqData $ local f re
    rqDataError e = mapRqData ((Left e) `apEither`) (return ())

-- instance (MonadPlus m, MonadIO m, ServerMonad m) => (HasRqData m) where
instance (MonadIO m) => HasRqData (ServerPartT m) where
    askRqEnv =
        do rq  <- askRq
           mbi <- liftIO $ if ((rqMethod rq == POST) || (rqMethod rq == PUT)) && (isDecodable (ctype rq))
                           then readInputsBody rq
                           else return (Just [])
           return (rqInputsQuery rq, mbi, rqCookies rq)
        where
          ctype :: Request -> Maybe ContentType
          ctype req = parseContentType . P.unpack =<< getHeader "content-type" req
          isDecodable :: Maybe ContentType -> Bool
          isDecodable Nothing                                                      = True -- assume it is application/x-www-form-urlencoded
          isDecodable (Just (ContentType "application" "x-www-form-urlencoded" _)) = True
          isDecodable (Just (ContentType "multipart" "form-data" ps))              = True
          isDecodable (Just _)                                                     = False

    rqDataError e = mzero
    localRqEnv f m =
        do rq <- askRq
           b  <- liftIO $ readInputsBody rq
           let (q', b', c') = f (rqInputsQuery rq, b, rqCookies rq)
           bv <- liftIO $ newMVar (fromMaybe [] b')
           let rq' = rq { rqInputsQuery = q'
                        , rqInputsBody = bv
                        , rqCookies = c'
                        }
           localRq (const rq') m

-- | apply 'RqData a' to a 'RqEnv'
--
-- see also: 'getData', 'getDataFn', 'withData', 'withDataFn', 'RqData', 'getDataFn'
runRqData :: RqData a -> RqEnv -> Either [String] a
runRqData rqData rqEnv =
    either (Left . unErrors) Right $ runReaderError (unRqData rqData) rqEnv

-- | transform the result of 'RqData a'.
--
-- This is similar to 'fmap' except it also allows you to modify the
-- 'Errors' not just 'a'.
mapRqData :: (Either (Errors String) a -> Either (Errors String) b) -> RqData a -> RqData b
mapRqData f m = RqData $ ReaderError $ mapReaderT f (unReaderError (unRqData m))

-- | use 'read' to convert a 'String' to a value of type 'a'
--
-- > look "key" `checkRq` (readRq "key")
-- 
-- use with 'checkRq'
readRq :: (Read a) => 
          String -- ^ name of key (only used for error reporting)
       -> String -- ^ 'String' to 'read'
       -> Either String a -- ^ 'Left' on error, 'Right' on success
readRq key val =
    case reads val of
      [(a,[])] -> Right a
      _        -> Left $ "readRq failed while parsing key: " ++ key ++ " which has the value: " ++ val

-- | convert or validate a value
--
-- This is similar to 'fmap' except that the function can fail by
-- returning Left and an error message. The error will be propagated
-- by calling 'rqDataError'.
--
-- This function is useful for a number of things including:
-- 
--  (1) Parsing a 'String' into another type
--
--  (2) Checking that a value meets some requirements (for example, that is an Int between 1 and 10).
--
-- Example usage at:
--
-- <http://happstack.com/docs/crashcourse/RqData.html#rqdatacheckrq>
checkRq :: (Monad m, HasRqData m) => m a -> (a -> Either String b) -> m b
checkRq rq f =
    do a <- rq
       case f a of
         (Left e)  -> rqDataError (strMsg e)
         (Right b) -> return b

-- | like 'checkRq' but the check function can be monadic
checkRqM :: (Monad m, HasRqData m) => m a -> (a -> m (Either String b)) -> m b
checkRqM rq f =
    do a <- rq
       b <- f a
       case b of
         (Left e)  -> rqDataError (strMsg e)
         (Right b) -> return b

-- | Used by 'withData' and 'getData'. Make your preferred data
-- type an instance of 'FromData' to use those functions.
class FromData a where
    fromData :: RqData a
{-
instance (Eq a,Show a,Xml a,G.Data a) => FromData a where
    fromData = do mbA <- lookPairs >>= return . normalize . fromPairs
                  case mbA of
                    Just a -> return a
                    Nothing -> fail "FromData G.Data failure"
--    fromData = lookPairs >>= return . normalize . fromPairs
-}
instance (FromData a, FromData b) => FromData (a,b) where
    fromData = (,)   <$> fromData <*> fromData

instance (FromData a, FromData b, FromData c) => FromData (a,b,c) where
    fromData = (,,)  <$> fromData <*> fromData <*> fromData

instance (FromData a, FromData b, FromData c, FromData d) => FromData (a,b,c,d) where
    fromData = (,,,) <$> fromData <*> fromData <*> fromData <*> fromData

instance FromData a => FromData (Maybe a) where
    fromData = (Just <$> fromData) <|> (pure Nothing)

-- | similar to 'Data.List.lookup' but returns all matches not just the first
lookups :: (Eq a) => a -> [(a, b)] -> [b]
lookups a = map snd . filter ((a ==) . fst)

fromMaybeBody :: String -> String -> Maybe [(String, Input)] -> [(String, Input)]
fromMaybeBody funName fieldName mBody =
    case mBody of
      Nothing -> error $ funName ++ " " ++ fieldName ++ " failed because the request body has not been decoded yet. Try using 'decodeBody' to decode the body. Or the 'queryString' filter to ignore the body."
      (Just body) -> body

-- | Gets the first matching named input parameter
-- 
-- Searches the QUERY_STRING followed by the Request body.
--
-- see also: 'lookInputs'
lookInput :: (Monad m, HasRqData m) => String -> m Input
lookInput name
    = do (query, mBody, _cookies) <- askRqEnv
         let body = fromMaybeBody "lookInput" name mBody
         case lookup name (query ++ body) of
           Just i  -> return $ i
           Nothing -> rqDataError (strMsg $ "Parameter not found: " ++ name)

-- | Gets all matches for the named input parameter
-- 
-- Searches the QUERY_STRING followed by the Request body.
--
-- see also: 'lookInput'
lookInputs :: (Monad m, HasRqData m) => String -> m [Input]
lookInputs name
    = do (query, mBody, _cookies) <- askRqEnv
         let body = fromMaybeBody "lookInputs" name mBody
         return $ lookups name (query ++ body)

-- | Gets the first matching named input parameter as a lazy 'ByteString'
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- see also: 'lookBSs'
lookBS :: (Functor m, Monad m, HasRqData m) => String -> m L.ByteString
lookBS n = 
    do i <- fmap inputValue (lookInput n)
       case i of
         (Left fp)  -> rqDataError $ (strMsg $ "lookBS: " ++ n ++ " is a file.")
         (Right bs) -> return bs

-- | Gets all matches for the named input parameter as lazy 'ByteString's
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- see also: 'lookBS'
lookBSs :: (Functor m, Monad m, HasRqData m) => String -> m [L.ByteString]
lookBSs n = 
    do is <- fmap (map inputValue) (lookInputs n)
       case partitionEithers is of
         ([], bs) -> return bs
         (fp, _)  -> rqDataError (strMsg $ "lookBSs: " ++ n ++ " is a file.")

-- | Gets the first matching named input parameter as a 'String'
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- Example:
--
-- > handler :: ServerPart Response
-- > handler =
-- >      do foo <- look "foo"
-- >         ok $ toResponse $ "foo = " ++ foo
--
-- see also: 'looks', 'lookBS', and 'lookBSs'
look :: (Functor m, Monad m, HasRqData m) => String -> m String
look = fmap LU.toString . lookBS

-- | Gets all matches for the named input parameter as 'String's
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- see also: 'look' and 'lookBSs'
looks :: (Functor m, Monad m, HasRqData m) => String -> m [String]
looks = fmap (map LU.toString) . lookBSs

-- | Gets the first matching named input parameter as a lazy 'Text'
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- see also: 'lookTexts', 'look', 'looks', 'lookBS', and 'lookBSs'
lookText :: (Functor m, Monad m, HasRqData m) => String -> m Text
lookText = fmap Text.decodeUtf8 . lookBS

-- | Gets all matches for the named input parameter as lazy 'Text's
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- see also: 'lookText', 'looks' and 'lookBSs'
lookTexts :: (Functor m, Monad m, HasRqData m) => String -> m [Text]
lookTexts = fmap (map Text.decodeUtf8) . lookBSs

-- | Gets the named cookie
-- the cookie name is case insensitive
lookCookie :: (Monad m, HasRqData m) => String -> m Cookie
lookCookie name
    = do (_query,_body, cookies) <- askRqEnv
         case lookup (map toLower name) cookies of -- keys are lowercased
           Nothing -> rqDataError $ strMsg $ "lookCookie: cookie not found: " ++ name
           Just c  -> return c

-- | gets the named cookie as a string
lookCookieValue :: (Functor m, Monad m, HasRqData m) => String -> m String
lookCookieValue = fmap cookieValue . lookCookie

-- | gets the named cookie as the requested Read type
readCookieValue :: (Functor m, Monad m, HasRqData m, Read a) => String -> m a
readCookieValue name = fmap cookieValue (lookCookie name) `checkRq` (readRq name)

-- | Gets the first matching named input parameter and decodes it using 'Read'
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- see also: 'lookReads'
lookRead :: (Functor m, Monad m, HasRqData m, Read a) => String -> m a
lookRead name = look name `checkRq` (readRq name)

-- | Gets all matches for the named input parameter and decodes them using 'Read'
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- see also: 'lookReads'
lookReads :: (Functor m, Monad m, HasRqData m, Read a) => String -> m [a]
lookReads name = 
    do vals <- looks name
       mapM (\v -> (return v) `checkRq` (readRq name)) vals

-- | Gets the first matching named file
--
-- Files can only appear in the request body. Additionally, the form
-- must set enctype=\"multipart\/form-data\".
--
-- This function returns a tuple consisting of:
-- 
--  (1) The temporary location of the uploaded file
--
--  (2) The local filename supplied by the browser
--
--  (3) The content-type supplied by the browser
--
-- NOTE: You must move the file from the temporary location before the
-- 'Response' is sent. The temporary files are automatically removed
-- after the 'Response' is sent.
lookFile :: (Monad m, HasRqData m) =>
            String -- ^ name of input field to search for
         -> m (FilePath, FilePath, ContentType) -- ^ (temporary file location, uploaded file name, content-type)
lookFile n =
    do i <- lookInput n
       case inputValue i of
         (Right _) -> rqDataError $ (strMsg $ "lookFile: " ++ n ++ " was found but is not a file.")
         (Left fp) -> return (fp, fromJust $ inputFilename i, inputContentType i)

-- | gets all the input parameters, and converts them to a 'String'
--
-- The results will contain the QUERY_STRING followed by the Request
-- body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- see also: 'lookPairsBS'
lookPairs :: (Monad m, HasRqData m) => m [(String, Either FilePath String)]
lookPairs = 
    do (query, mBody, _cookies) <- askRqEnv
       let body = fromMaybeBody "lookPairs" "" mBody
       return $ map (\(n,vbs)->(n, (\e -> case e of Left fp -> Left fp ; Right bs -> Right (LU.toString bs)) $ inputValue vbs)) (query ++ body)

-- | gets all the input parameters
--
-- The results will contain the QUERY_STRING followed by the Request
-- body.
--
-- see also: 'lookPairs'
lookPairsBS :: (Monad m, HasRqData m) => m [(String, Either FilePath L.ByteString)]
lookPairsBS = 
    do (query, mBody, _cookies) <- askRqEnv
       let body = fromMaybeBody "lookPairsBS" "" mBody
       return $ map (\(n,vbs) -> (n, inputValue vbs)) (query ++ body)

-- | The POST\/PUT body of a Request is not received or decoded unless
-- this function is invoked. 
--
-- It is an error to try to use the look functions for a POST\/PUT
-- request with out first calling this function.
--
-- It is ok to call 'decodeBody' at the beginning of every request:
--
-- > main = simpleHTTP nullConf $ 
-- >           do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
-- >              handlers
--
-- You can achieve finer granularity quotas by calling 'decodeBody'
-- with different values in different handlers.
--
-- Only the first call to 'decodeBody' will have any effect. Calling
-- it a second time, even with different quota values, will do
-- nothing.
decodeBody :: (ServerMonad m, MonadPlus m, MonadIO m, FilterMonad Response m, WebMonad Response m) => BodyPolicy -> m ()
decodeBody bp =
    do rq <- askRq
       (_, me) <- bodyInput bp rq
       case me of
         Nothing -> return ()
         Just e  -> escape $ requestEntityTooLarge (toResponse e) -- FIXME: is this the best way to report the error

-- | run 'RqData' in a 'ServerMonad'.
--
-- Example: a simple @GET@ or @POST@ variable based authentication
-- guard.  It handles the request with 'errorHandler' if
-- authentication fails.
--
-- >  data AuthCredentials = AuthCredentials { username :: String,  password :: String }
-- >
-- >  isValid :: AuthCredentials -> Bool
-- >  isValid = const True
-- >
-- >  myRqData :: RqData AuthCredentials
-- >  myRqData = do
-- >     username <- look "username"
-- >     password <- look "password"
-- >     return (AuthCredentials username password)
-- >
-- >  checkAuth :: (String -> ServerPart Response) -> ServerPart Response
-- >  checkAuth errorHandler = do
-- >     d <- getDataFn myRqData
-- >     case d of
-- >         (Left e) -> errorHandler (unlines e)
-- >         (Right a) | isValid a -> mzero
-- >         (Right a) | otherwise -> errorHandler "invalid"
--
-- NOTE: you must call 'decodeBody' prior to calling this function if
-- the request method is POST or PUT.
getDataFn :: (HasRqData m, ServerMonad m, MonadIO m) => 
             RqData a -- ^ 'RqData' monad to evaluate
          -> m (Either [String] a) -- ^ return 'Left' errors or 'Right' a
getDataFn rqData =
    do rqEnv <- askRqEnv
       return (runRqData rqData rqEnv)

-- | similar to 'getDataFn', except it calls a sub-handler on success
-- or 'mzero' on failure.
-- 
-- NOTE: you must call 'decodeBody' prior to calling this function if
-- the request method is POST or PUT.
withDataFn :: (HasRqData m, MonadIO m, MonadPlus m, ServerMonad m) => RqData a -> (a -> m r) -> m r
withDataFn fn handle = getDataFn fn >>= either (const mzero) handle

-- | A variant of 'getDataFn' that uses 'FromData' to chose your
-- 'RqData' for you.  The example from 'getData' becomes:
-- 
-- >  data AuthCredentials = AuthCredentials { username :: String,  password :: String }
-- >
-- >  isValid :: AuthCredentials -> Bool
-- >  isValid = const True
-- >
-- >  myRqData :: RqData AuthCredentials
-- >  myRqData = do
-- >     username <- look "username"
-- >     password <- look "password"
-- >     return (AuthCredentials username password)
-- >
-- >  instance FromData AuthCredentials where
-- >     fromData = myRqData
-- >
-- >  checkAuth :: (String -> ServerPart Response) -> ServerPart Response
-- >  checkAuth errorHandler = do
-- >     d <- getData
-- >     case d of
-- >         (Left e) -> errorHandler (unlines e)
-- >         (Right a) | isValid a -> mzero
-- >         (Right a) | otherwise -> errorHandler "invalid"
--
-- NOTE: you must call 'decodeBody' prior to calling this function if
-- the request method is POST or PUT.
getData :: (HasRqData m, MonadIO m, ServerMonad m, FromData a) => m (Either [String] a)
getData = getDataFn fromData

-- | similar to 'getData' except it calls a subhandler on success or 'mzero' on failure.
--
-- NOTE: you must call 'decodeBody' prior to calling this function if
-- the request method is POST or PUT.
withData :: (HasRqData m, MonadIO m, FromData a, MonadPlus m, ServerMonad m) => (a -> m r) -> m r
withData = withDataFn fromData

-- | limit the scope to the Request body
--
-- > handler :: ServerPart Response
-- > handler =
-- >     do foo <- body $ look "foo"
-- >        ok $ toResponse $ "foo = " ++ foo
body :: (HasRqData m) => m a -> m a
body rqData = localRqEnv f rqData
    where
      f (_query, body, _cookies) = ([], body, [])

-- | limit the scope to the QUERY_STRING
--
-- > handler :: ServerPart Response
-- > handler =
-- >     do foo <- queryString $ look "foo"
-- >        ok $ toResponse $ "foo = " ++ foo
queryString ::  (HasRqData m) => m a -> m a
queryString rqData = localRqEnv f rqData
    where
      f (query, _body, _cookies) = (query, Just [], [])

right :: (MonadPlus m) => Either a b -> m b
right (Right a) = return a
right (Left e) = mzero

bytestring :: (HasRqData m) => m a -> m a
bytestring rqData = localRqEnv f rqData
    where
      f (query, body, cookies) = (filter bsf query, filter bsf <$> body, cookies)
      bsf (_, i) =
          case inputValue i of
            (Left  _fp) -> False
            (Right _bs) -> True
