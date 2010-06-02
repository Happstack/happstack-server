{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Happstack.Server.RqData where

import Control.Applicative 			(Applicative((<*>), pure), Alternative((<|>), empty), WrappedMonad(WrapMonad, unwrapMonad), (<$>))
import Control.Monad 				(MonadPlus(mzero))
import Control.Monad.Reader 			(ReaderT(ReaderT, runReaderT), MonadReader(ask, local), asks, mapReaderT)
import Control.Monad.Error 			(Error(noMsg, strMsg))
import qualified Data.ByteString.Lazy.Char8     as L
import qualified Data.ByteString.Lazy.UTF8      as LU
import Data.Char 				(toLower)
import Data.Generics                            (Data, Typeable)
import Data.Monoid 				(Monoid(mempty, mappend, mconcat))
import Happstack.Server.Cookie 			(Cookie (cookieValue))
import Happstack.Server 			(Input(inputValue), Request(rqInputs, rqCookies), ServerMonad, askRq)
import Happstack.Util.Common                    (readM)

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

newtype Errors a = Errors { unErrors :: [a] }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Monoid (Errors a) where
    mempty = Errors []
    (Errors x) `mappend` (Errors y) = Errors (x ++ y)
    mconcat errs = Errors $ concatMap unErrors errs

instance Error (Errors String) where
    noMsg = Errors []
    strMsg str = Errors [str]

type RqData = ReaderError ([(String, Input)], [(String, Cookie)]) (Errors String)

mapReaderErrorT :: (Either e a -> Either e' b) -> (ReaderError r e a) -> (ReaderError r e' b)
mapReaderErrorT f m = ReaderError $ mapReaderT f (unReaderError m)

readerError :: (Monoid e, Error e) => e -> ReaderError r e b
readerError e = mapReaderErrorT ((Left e) `apEither`) (return ())

runReaderError :: ReaderError r e a -> r -> Either e a
runReaderError = runReaderT . unReaderError


-- | Useful for 'withData' and 'getData''. Make your preferred data
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
    fromData = (,) <$> fromData <*> fromData

instance (FromData a, FromData b, FromData c) => FromData (a,b,c) where
    fromData = (,,) <$> fromData <*> fromData <*> fromData

instance (FromData a, FromData b, FromData c, FromData d) => FromData (a,b,c,d) where
    fromData = (,,,) <$>fromData <*> fromData <*> fromData <*> fromData

instance FromData a => FromData (Maybe a) where
    fromData = (Just <$> fromData) <|> (pure Nothing)

-- | similar to 'Data.List.lookup' but returns all matches not just the first
lookups :: (Eq a) => a -> [(a, b)] -> [b]
lookups a = map snd . filter ((a ==) . fst)

lookInput :: String -> RqData Input
lookInput name
    = do inputs <- asks fst
         case lookup name inputs of
           Just i  -> return $ i
           Nothing -> readerError (strMsg name)

lookInputs :: String -> RqData [Input]
lookInputs name
    = do inputs <- asks fst
         return $ lookups name inputs

-- | Gets the named input parameter as a lazy byte string
lookBS :: String -> RqData L.ByteString
lookBS = fmap inputValue . lookInput

-- | Gets the named input parameter as a lazy byte string
lookBSs :: String -> RqData [L.ByteString]
lookBSs = fmap (map inputValue) . lookInputs

look :: String -> RqData String
look = fmap LU.toString . lookBS

looks :: String -> RqData [String]
looks = fmap (map LU.toString) . lookBSs

-- | Gets the named cookie
-- the cookie name is case insensitive
lookCookie :: String -> RqData Cookie
lookCookie name
    = do cookies <- asks snd
         case lookup (map toLower name) cookies of -- keys are lowercased
           Nothing -> fail "cookie not found"
           Just c  -> return c

-- | gets the named cookie as a string
lookCookieValue :: String -> RqData String
lookCookieValue = fmap cookieValue . lookCookie

-- | gets the named cookie as the requested Read type
readCookieValue :: Read a => String -> RqData a
readCookieValue name = readM =<< fmap cookieValue (lookCookie name)

-- | like look, but Reads for you.n
lookRead :: Read a => String -> RqData a
lookRead name = readM =<< look name

-- | like look, but Reads for you.n
lookReads :: Read a => String -> RqData [a]
lookReads name = mapM readM =<< looks name

-- | gets all the input parameters, and converts them to a string
lookPairs :: RqData [(String,String)]
lookPairs = asks fst >>= return . map (\(n,vbs)->(n,LU.toString $ inputValue vbs))


-- | Parse your request with a 'RqData' (a 'ReaderT', basically) For
-- example here is a simple @GET@ or @POST@ variable based
-- authentication guard.  It handles the request with 'errorHandler'
-- if authentication fails.
--
-- > myRqData = do
-- >     username <- lookInput "username"
-- >     password <- lookInput "password"
-- >     return (username, password)
-- > checkAuth errorHandler = do
-- >     d <- getData myRqDataA
-- >     case d of
-- >         Nothing -> errorHandler
-- >         Just a | isValid a -> mzero
-- >         Just a | otherwise -> errorHandler
getDataFn :: (ServerMonad m) => RqData a -> m (Either [String] a)
getDataFn rqData = do
    rq <- askRq
    return $ either (Left . unErrors) Right $ runReaderError rqData (rqInputs rq, rqCookies rq)

-- | A variant of 'getData' that uses 'FromData' to chose your
-- 'RqData' for you.  The example from 'getData' becomes:
--
-- >  myRqData = do
-- >     username <- lookInput "username"
-- >     password <- lookInput "password"
-- >     return (username, password)
-- >  instance FromData (String,String) where
-- >     fromData = myRqData
-- >  checkAuth errorHandler = do
-- >     d <- getData'
-- >     case d of
-- >         Nothing -> errorHandler
-- >         Just a | isValid a -> mzero
-- >         Just a | otherwise -> errorHandler
--
withDataFn :: (MonadPlus m, ServerMonad m) => RqData a -> (a -> m r) -> m r
withDataFn fn handle = getDataFn fn >>= either (const mzero) handle

-- | A variant of 'getDataFn' that uses 'FromData' to chose your
-- 'RqData' for you.  The example from 'getData' becomes:
--
-- >  myRqData = do
-- >     username <- lookInput "username"
-- >     password <- lookInput "password"
-- >     return (username, password)
-- >  instance FromData (String,String) where
-- >     fromData = myRqData
-- >  checkAuth errorHandler = do
-- >     d <- getData'
-- >     case d of
-- >         Nothing -> errorHandler
-- >         Just a | isValid a -> mzero
-- >         Just a | otherwise -> errorHandler
--
getData :: (ServerMonad m, FromData a) => m (Either [String] a)
getData = getDataFn fromData

-- | Retrieve data from the input query or the cookies.
withData :: (FromData a, MonadPlus m, ServerMonad m) => (a -> m r) -> m r
withData = withDataFn fromData
