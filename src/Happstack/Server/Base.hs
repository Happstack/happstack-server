{-# LANGUAGE FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, UndecidableInstances  #-}
{- | This module defines the Monad stack used by Happstack. You mostly don't want to be looking in here. 
-}
module Happstack.Server.Base where

import Control.Applicative                       (Applicative, pure, (<*>), Alternative(empty,(<|>)))

import Control.Monad                             ( MonadPlus, mzero, mplus
                                                 , msum, ap, unless
                                                 , liftM, liftM2, liftM3, liftM4
                                                 )
import Control.Monad.Trans                       ( MonadTrans, lift
                                                 , MonadIO, liftIO
                                                 )
import Control.Monad.Reader                      ( ReaderT(ReaderT), runReaderT
                                                 , MonadReader, ask, local
                                                 , asks
                                                 )
import Control.Monad.Writer                      ( WriterT(WriterT), runWriterT
                                                 , MonadWriter, tell, pass
                                                 , listens
                                                 )
import qualified Control.Monad.Writer            as Writer (listen) -- So that we can disambiguate 'Listen.listen'
import Control.Monad.State                       (MonadState, get, put)
import Control.Monad.Error                       ( ErrorT(ErrorT), runErrorT
                                                 , Error, strMsg
                                                 , MonadError, throwError, catchError
                                                 , mapErrorT
                                                 )
import Control.Monad.Maybe                       (MaybeT(MaybeT), runMaybeT)
import qualified Data.ByteString.Char8           as B
import Data.Monoid
import Happstack.Server.HTTP.Types


-- | An alias for 'WebT' when using 'IO'.
type Web a = WebT IO a
-- | An alias for using 'ServerPartT' when using the 'IO'.
type ServerPart a = ServerPartT IO a

--------------------------------------
-- HERE BEGINS ServerPartT definitions

-- | 'ServerPartT' is a container for processing requests and returning results.
newtype ServerPartT m a = ServerPartT { unServerPartT :: ReaderT Request (WebT m) a }
    deriving (Monad, MonadPlus, Functor)

instance (MonadIO m) => MonadIO (ServerPartT m) where
    liftIO = ServerPartT . liftIO
    {-# INLINE liftIO #-}

-- | Particularly useful when combined with 'runWebT' to produce
-- a @m ('Maybe' 'Response')@ from a 'Request'.
runServerPartT :: ServerPartT m a -> Request -> WebT m a
runServerPartT = runReaderT . unServerPartT

withRequest :: (Request -> WebT m a) -> ServerPartT m a
withRequest = ServerPartT . ReaderT

-- | A constructor for a 'ServerPartT' when you don't care about the request.
anyRequest :: Monad m => WebT m a -> ServerPartT m a
anyRequest x = withRequest $ \_ -> x

-- | Used to manipulate the containing monad.  Very useful when
-- embedding a monad into a 'ServerPartT', since 'simpleHTTP' requires
-- a @'ServerPartT' 'IO' a@.  Refer to 'WebT' for an explanation of
-- the structure of the monad.
--
-- Here is an example.  Suppose you want to embed an 'ErrorT' into your
-- 'ServerPartT' to enable 'throwError' and 'catchError' in your 'Monad'.
--
-- > type MyServerPartT e m a = ServerPartT (ErrorT e m) a
--
-- Now suppose you want to pass @MyServerPartT@ into a function that
-- demands a @'ServerPartT' 'IO' a@ (e.g. 'simpleHTTP').  You can
-- provide the function:
--
-- >   unpackErrorT :: (Monad m, Show e) => UnWebT (ErrorT e m) a -> UnWebT m a
-- >   unpackErrorT et = do
-- >      eitherV <- runErrorT et
-- >      return $ case eitherV of
-- >          Left err -> Just (Left $ toResponse $ 
-- >                                   "Catastrophic failure " ++ show err
-- >                           , Set $ Dual $ Endo $ \r -> r{rsCode = 500})
-- >          Right x -> x
--
-- With @unpackErrorT@ you can now call 'simpleHTTP'. Just wrap your
-- 'ServerPartT' list.
--
-- >  simpleHTTP nullConf $ mapServerPartT unpackErrorT (myPart `catchError` myHandler)
--
-- Or alternatively:
--
-- >  simpleHTTP' unpackErrorT nullConf (myPart `catchError` myHandler)
--
-- Also see 'spUnwrapErrorT' for a more sophisticated version of this
-- function.
--
mapServerPartT :: (     UnWebT m a ->      UnWebT n b)
               -> (ServerPartT m a -> ServerPartT n b)
mapServerPartT f ma = withRequest $ \rq -> mapWebT f (runServerPartT ma rq)

-- | A variant of 'mapServerPartT' where the first argument also takes
-- a 'Request'.  Useful if you want to 'runServerPartT' on a different
-- 'ServerPartT' inside your monad (see 'spUnwrapErrorT').
mapServerPartT' :: (Request -> UnWebT m a ->      UnWebT n b)
                -> (      ServerPartT m a -> ServerPartT n b)
mapServerPartT' f ma = withRequest $ \rq -> mapWebT (f rq) (runServerPartT ma rq)

instance MonadTrans (ServerPartT) where
    lift m = withRequest (\_ -> lift m)

instance (Monad m) => Monoid (ServerPartT m a) where
    mempty  = mzero
    mappend = mplus

instance (Monad m, Functor m) => Applicative (ServerPartT m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (ServerPartT m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, MonadWriter w m) => MonadWriter w (ServerPartT m) where
    tell = lift . tell
    listen m = withRequest $ \rq ->  Writer.listen (runServerPartT m rq) >>= return
    pass m = withRequest $ \rq -> pass (runServerPartT m rq) >>= return

instance (Monad m, MonadError e m) => MonadError e (ServerPartT m) where
    throwError e = lift $ throwError e
    catchError action handler = withRequest $ \rq -> (runServerPartT action rq) `catchError` ((flip runServerPartT $ rq) . handler)

instance (Monad m, MonadReader r m) => MonadReader r (ServerPartT m) where
    ask = lift ask
    local fn m = withRequest $ \rq-> local fn (runServerPartT m rq)

instance Monad m => FilterMonad Response (ServerPartT m) where
    setFilter = anyRequest . setFilter
    composeFilter = anyRequest . composeFilter
    getFilter m = withRequest $ \rq -> getFilter (runServerPartT m rq)

instance Monad m => WebMonad Response (ServerPartT m) where
    finishWith r = anyRequest $ finishWith r

-- | Yes, this is exactly like 'ReaderT' with new names.  Why you ask?
-- Because 'ServerPartT' can lift up a 'ReaderT'.  If you did that, it
-- would shadow 'ServerPartT''s behavior as a 'ReaderT', thus meaning if
-- you lifted the 'ReaderT' you could no longer ask for the 'Request'.
-- This way you can add a 'ReaderT' to your monad stack without any
-- trouble.
class Monad m => ServerMonad m where
    askRq   :: m Request
    localRq :: (Request -> Request) -> m a -> m a

instance (Monad m) => ServerMonad (ServerPartT m) where
    askRq = ServerPartT $ ask
    localRq f m = ServerPartT $ local f (unServerPartT m)

instance (Error e, ServerMonad m) => ServerMonad (ErrorT e m) where
    askRq     = lift askRq
    localRq f = mapErrorT $ localRq f

-------------------------------
-- HERE BEGINS WebT definitions

-- | A monoid operation container.  If @a@ is a monoid, then
-- 'SetAppend' is a monoid with the following behaviors:
--
-- >  Set    x `mappend` Append y = Set    (x `mappend` y)
-- >  Append x `mappend` Append y = Append (x `mappend` y)
-- >  _        `mappend` Set y    = Set y
--
-- A simple way of summarizing this is, if the right side is 'Append',
-- then the right is appended to the left.  If the right side is
-- 'Set', then the left side is ignored.

data SetAppend a = Set a | Append a
    deriving (Eq, Show)

instance Monoid a => Monoid (SetAppend a) where
   mempty = Append mempty

   Set    x `mappend` Append y = Set    (x `mappend` y)
   Append x `mappend` Append y = Append (x `mappend` y)
   _        `mappend` Set y    = Set y

-- | Extract the value from a 'SetAppend'.
-- Note that a 'SetAppend' is actually a @CoPointed@ from:
-- <http://hackage.haskell.org/packages/archive/category-extras/latest/doc/html/Control-Functor-Pointed.html>
-- But lets not drag in that dependency. yet...
extract :: SetAppend t -> t
extract (Set    x) = x
extract (Append x) = x

instance Functor (SetAppend) where
    fmap f (Set    x) = Set    $ f x
    fmap f (Append x) = Append $ f x

-- | 'FilterFun' is a lot more fun to type than @'SetAppend' ('Dual'
-- ('Endo' a))@.
type FilterFun a = SetAppend (Dual (Endo a))

unFilterFun :: FilterFun a -> (a -> a)
unFilterFun = appEndo . getDual . extract

newtype FilterT a m b = FilterT { unFilterT :: WriterT (FilterFun a) m b }
   deriving (Monad, MonadTrans, Functor)

instance (MonadIO m) => MonadIO (FilterT a m) where
    liftIO = FilterT . liftIO
    {-# INLINE liftIO #-}

-- | A set of functions for manipulating filters.  A 'ServerPartT'
-- implements 'FilterMonad' 'Response' so these methods are the
-- fundamental ways of manipulating the response object, especially
-- before you've converted your monadic value to a 'Response'.
class Monad m => FilterMonad a m | m->a where
    -- | Ignores all previous alterations to your filter
    --
    -- As an example:
    --
    -- > do
    -- >   composeFilter f
    -- >   setFilter g
    -- >   return "Hello World"
    --
    -- The @'setFilter' g@ will cause the first @'composeFilter' f@ to
    -- be ignored.
    setFilter :: (a->a) -> m ()
    -- | Composes your filter function with the existing filter
    -- function.
    composeFilter :: (a->a) -> m ()
    -- | Retrieves the filter from the environment.
    getFilter :: m b -> m (b, a->a)

instance (Monad m) => FilterMonad a (FilterT a m) where
    setFilter     = FilterT . tell                . Set    . Dual . Endo
    composeFilter = FilterT . tell                . Append . Dual . Endo
    getFilter     = FilterT . listens unFilterFun . unFilterT

-- | The basic 'Response' building object.
newtype WebT m a = WebT { unWebT :: ErrorT Response (FilterT (Response) (MaybeT m)) a }
    deriving (Functor)

instance (MonadIO m) => MonadIO (WebT m) where
    liftIO = WebT . liftIO
    {-# INLINE liftIO #-}

-- | It is worth discussing the unpacked structure of 'WebT' a bit as
--  it's exposed in 'mapServerPartT' and 'mapWebT'.
--
--  A fully unpacked 'WebT' has a structure that looks like:
--
--  > ununWebT $ WebT m a :: m (Maybe (Either Response a, FilterFun Response))
--
--  So, ignoring @m@, as it is just the containing 'Monad', the
--  outermost layer is a 'Maybe'.  This is 'Nothing' if 'mzero' was
--  called or @'Just' ('Either' 'Response' a, 'SetAppend' ('Endo'
--  'Response'))@ if 'mzero' wasn't called.  Inside the 'Maybe', there
--  is a pair.  The second element of the pair is our filter function
--  @'FilterFun' 'Response'@.  @'FilterFun' 'Response'@ is a type
--  alias for @'SetAppend' ('Dual' ('Endo' 'Response'))@.  This is
--  just a wrapper for a @'Response' -> 'Response'@ function with a
--  particular 'Monoid' behavior.  The value
--
--  >  Append (Dual (Endo f))
--
--  Causes @f@ to be composed with the previous filter.
--
--  >  Set (Dual (Endo f))
--
--  Causes @f@ to not be composed with the previous filter.
--
--  Finally, the first element of the pair is either @'Left'
--  'Response'@ or @'Right' a@.
--
--  Another way of looking at all these pieces is from the behaviors
--  they control.  The 'Maybe' controls the 'mzero' behavior.  @'Set'
--  ('Endo' f)@ comes from the 'setFilter' behavior.  Likewise,
--  @'Append' ('Endo' f)@ is from 'composeFilter'.  @'Left'
--  'Response'@ is what you get when you call 'finishWith' and
--  @'Right' a@ is the normal exit.
--
--  An example case statement looks like:
--
--  >  ex1 webt = do
--  >    val <- ununWebT webt
--  >    case val of
--  >        Nothing -> Nothing  -- this is the interior value when mzero was used
--  >        Just (Left r, f) -> Just (Left r, f) -- r is the value that was passed into "finishWith"
--  >                                             -- f is our filter function
--  >        Just (Right a, f) -> Just (Right a, f) -- a is our normal monadic value
--  >                                               -- f is still our filter function
--
type UnWebT m a = m (Maybe (Either Response a, FilterFun Response))

instance Monad m => Monad (WebT m) where
    m >>= f = WebT $ unWebT m >>= unWebT . f
    {-# INLINE (>>=) #-}
    return a = WebT $ return a
    {-# INLINE return #-}
    fail s = finishWith (result 500 s)
--    fail s = outputTraceMessage s (mkFailMessage s) -- FIXME


newtype MyWebT m a = MyWebT { unMyWebT :: ErrorT Response (WriterT [Response] {- (MaybeT -} m {-) -}) a }
    deriving (Functor)

instance (MonadIO m) => MonadIO (MyWebT m) where
    liftIO = MyWebT . liftIO
    {-# INLINE liftIO #-}

instance Monad m => Monad (MyWebT m) where
    m >>= f = MyWebT $ unMyWebT m >>= unMyWebT . f
    {-# INLINE (>>=) #-}
    return a = MyWebT $ return a
    {-# INLINE return #-}

class Monad m => WebMonad a m | m->a where
    -- | A control structure.  It ends the computation and returns the
    -- 'Response' you passed into it immediately.  This provides an
    -- alternate escape route.  In particular it has a monadic value
    -- of any type.  And unless you call @'setFilter' id@ first your
    -- response filters will be applied normally.
    --
    -- Extremely useful when you're deep inside a monad and decide
    -- that you want to return a completely different content type,
    -- since it doesn't force you to convert all your return types to
    -- 'Response' early just to accommodate this.
    finishWith :: a -> m b

instance (Monad m) => WebMonad Response (WebT m) where
    finishWith r = WebT $ throwError r

instance MonadTrans WebT where
    lift = WebT . lift . lift . lift

instance (Monad m) => MonadPlus (WebT m) where
    -- | Aborts a computation.
    --
    -- This is primarily useful because 'msum' will take an array of
    -- 'MonadPlus' and return the first one that isn't 'mzero', which
    -- is exactly the semantics expected from objects that take lists
    -- of 'ServerPartT'.
    mzero = WebT $ lift $ lift $ mzero
    mplus x y =  WebT $ ErrorT $ FilterT $ (lower x) `mplus` (lower y)
        where lower = (unFilterT . runErrorT . unWebT)

-- | Deprecated: use 'mzero'.
noHandle :: (MonadPlus m) => m a
noHandle = mzero
{-# DEPRECATED noHandle "Use mzero" #-}

instance (Monad m) => FilterMonad Response (WebT m) where
    setFilter f = WebT $ lift $ setFilter $ f
    composeFilter f = WebT . lift . composeFilter $ f
    getFilter     m = WebT $ ErrorT $ fmap lft $ getFilter (runErrorT $ unWebT m)
        where
          lft (Left  r, _) = Left r
          lft (Right a, f) = Right (a, f)

instance (Monad m) => Monoid (WebT m a) where
    mempty = mzero
    mappend = mplus
{-

-}
-- | For when you really need to unpack a 'WebT' entirely (and not
-- just unwrap the first layer with 'unWebT').
ununWebT :: WebT m a -> UnWebT m a
ununWebT = runMaybeT . runWriterT . unFilterT . runErrorT . unWebT

-- | For wrapping a 'WebT' back up.  @'mkWebT' . 'ununWebT' = 'id'@
mkWebT :: UnWebT m a -> WebT m a
mkWebT = WebT . ErrorT . FilterT . WriterT . MaybeT

-- | See 'mapServerPartT' for a discussion of this function.
mapWebT :: (UnWebT m a -> UnWebT n b)
        -> (  WebT m a ->   WebT n b)
mapWebT f ma = mkWebT $ f (ununWebT ma)

instance (Monad m, Functor m) => Applicative (WebT m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (WebT m) where
    empty = mzero
    (<|>) = mplus

instance MonadReader r m => MonadReader r (WebT m) where
    ask = lift ask
    local fn m = mkWebT $ local fn (ununWebT m)

instance MonadState st m => MonadState st (WebT m) where
    get = lift get
    put = lift . put

instance MonadError e m => MonadError e (WebT m) where
	throwError err = lift $ throwError err
 	catchError action handler = mkWebT $ catchError (ununWebT action) (ununWebT . handler)

instance MonadWriter w m => MonadWriter w (WebT m) where
    tell = lift . tell
    listen m = mkWebT $ Writer.listen (ununWebT m) >>= (return . liftWebT)
        where liftWebT (Nothing, _) = Nothing
              liftWebT (Just (Left x,f), _) = Just (Left x,f)
              liftWebT (Just (Right x,f),w) = Just (Right (x,w),f)
    pass m = mkWebT $ ununWebT m >>= liftWebT
        where liftWebT Nothing = return Nothing
              liftWebT (Just (Left x,f)) = return $ Just (Left x, f)
              liftWebT (Just (Right x,f)) = pass (return x)>>= (\a -> return $ Just (Right a,f))

-- | An alias for @'setFilter' 'id'@. It resets all your filters.
ignoreFilters :: (FilterMonad a m) => m ()
ignoreFilters = setFilter id

-- | Used to ignore all your filters and immediately end the
-- computation.  A combination of 'ignoreFilters' and 'finishWith'.
escape :: (WebMonad a m, FilterMonad a m) => m a -> m b
escape gen = ignoreFilters >> gen >>= finishWith

-- | An alternate form of 'escape' that can be easily used within a do
-- block.
escape' :: (WebMonad a m, FilterMonad a m) => a -> m b
escape' a = ignoreFilters >> finishWith a
