{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeFamilies, UndecidableInstances  #-}
{-| This module defines the Monad stack used by Happstack. You mostly don't want to be looking in here. Look in "Happstack.Server.Monads" instead.
-}
module Happstack.Server.Internal.Monads where

import Control.Applicative                       (Applicative, pure, (<*>), Alternative(empty,(<|>)))

import Control.Monad                             ( MonadPlus(mzero, mplus), ap, liftM, msum
                                                 )
import Control.Monad.Base                        ( MonadBase, liftBase )
import Control.Monad.Catch                       ( MonadCatch(..), MonadThrow(..) )
import Control.Monad.Error                       ( ErrorT(ErrorT), runErrorT
                                                 , Error, MonadError, throwError
                                                 , catchError, mapErrorT
                                                 )
import Control.Monad.Reader                      ( ReaderT(ReaderT), runReaderT
                                                 , MonadReader, ask, local, mapReaderT
                                                 )
import qualified Control.Monad.RWS.Lazy as Lazy       ( RWST, mapRWST )
import qualified Control.Monad.RWS.Strict as Strict   ( RWST, mapRWST )

import Control.Monad.Trans.Except                ( ExceptT, mapExceptT )
import Control.Monad.State.Class                      ( MonadState, get, put )
import qualified Control.Monad.State.Lazy as Lazy     ( StateT, mapStateT )
import qualified Control.Monad.State.Strict as Strict ( StateT, mapStateT )
import Control.Monad.Trans                       ( MonadTrans, lift
                                                 , MonadIO, liftIO
                                                 )
import Control.Monad.Trans.Control               ( MonadTransControl(..)
                                                 , MonadBaseControl(..)
                                                 , ComposeSt, defaultLiftBaseWith, defaultRestoreM
                                                 )
import Control.Monad.Writer.Class                ( MonadWriter, tell, pass, listens )
import qualified Control.Monad.Writer.Lazy as Lazy     ( WriterT(WriterT), runWriterT, mapWriterT )
import qualified Control.Monad.Writer.Strict as Strict ( WriterT, mapWriterT )
import qualified Control.Monad.Writer.Class as Writer  ( listen )

import Control.Monad.Trans.Maybe                 (MaybeT(MaybeT), runMaybeT)
import qualified Data.ByteString.Lazy.UTF8       as LU (fromString)
import Data.Char                                 (ord)
import Data.List                                 (inits, isPrefixOf, stripPrefix, tails)
import Data.Monoid                               (Monoid(mempty, mappend), Dual(..), Endo(..))
import qualified Paths_happstack_server          as Cabal
import qualified Data.Version                    as DV
import Debug.Trace                               (trace)
import Happstack.Server.Types                    (Request, Response, resultBS, setHeader)

-- | An alias for 'WebT' when using 'IO'.
type Web a = WebT IO a

-- | An alias for @'ServerPartT' 'IO'@
type ServerPart a = ServerPartT IO a

--------------------------------------
-- HERE BEGINS ServerPartT definitions

-- | 'ServerPartT' is a rich, featureful monad for web development.
--
-- see also: 'simpleHTTP', 'ServerMonad', 'FilterMonad', 'WebMonad', and 'HasRqData'
newtype ServerPartT m a = ServerPartT { unServerPartT :: ReaderT Request (WebT m) a }
    deriving (Monad, MonadPlus, Functor)

instance MonadCatch m => MonadCatch (ServerPartT m) where
    catch action handle = ServerPartT $ catch (unServerPartT action) (unServerPartT . handle)

instance MonadThrow m => MonadThrow (ServerPartT m) where
    throwM = ServerPartT . throwM

instance MonadBase b m => MonadBase b (ServerPartT m) where
    liftBase = lift . liftBase

instance (MonadIO m) => MonadIO (ServerPartT m) where
    liftIO = ServerPartT . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_monad_control(1,0,0)
instance MonadTransControl ServerPartT where
    type StT ServerPartT a = StT WebT (StT (ReaderT Request) a)
    liftWith f = ServerPartT $ liftWith $ \runReader ->
                                 liftWith $ \runWeb ->
                                   f $ runWeb . runReader . unServerPartT
    restoreT = ServerPartT . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (ServerPartT m) where
    type StM (ServerPartT m) a = ComposeSt ServerPartT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
#else
instance MonadTransControl ServerPartT where
    newtype StT ServerPartT a = StSP {unStSP :: StT WebT (StT (ReaderT Request) a)}
    liftWith f = ServerPartT $ liftWith $ \runReader ->
                                 liftWith $ \runWeb ->
                                   f $ liftM StSP . runWeb . runReader . unServerPartT
    restoreT = ServerPartT . restoreT . restoreT . liftM unStSP

instance MonadBaseControl b m => MonadBaseControl b (ServerPartT m) where
    newtype StM (ServerPartT m) a = StMSP {unStMSP :: ComposeSt ServerPartT m a}
    liftBaseWith = defaultLiftBaseWith StMSP
    restoreM     = defaultRestoreM     unStMSP
#endif

-- | Particularly useful when combined with 'runWebT' to produce
-- a @m ('Maybe' 'Response')@ from a 'Request'.
runServerPartT :: ServerPartT m a -> Request -> WebT m a
runServerPartT = runReaderT . unServerPartT

-- | function for lifting WebT to ServerPartT
--
-- NOTE: This is mostly for internal use. If you want to access the
-- 'Request' in user-code see 'askRq' from 'ServerMonad'.
--
-- > do request <- askRq
-- >    ...
withRequest :: (Request -> WebT m a) -> ServerPartT m a
withRequest = ServerPartT . ReaderT

-- | A constructor for a 'ServerPartT' when you don't care about the request.
--
-- NOTE: This is mostly for internal use. If you think you need to use
-- it in your own code, you might consider asking on the mailing list
-- or IRC to find out if there is an alternative solution.
anyRequest :: Monad m => WebT m a -> ServerPartT m a
anyRequest x = withRequest $ \_ -> x

-- | Apply a function to transform the inner monad of
-- @'ServerPartT' m@.
--
-- Often used when transforming a monad with 'ServerPartT', since
-- 'simpleHTTP' requires a @'ServerPartT' 'IO' a@.  Refer to 'UnWebT'
-- for an explanation of the structure of the monad.
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
-- >                           , filterFun $ \r -> r{rsCode = 500})
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
-- Also see 'Happstack.Server.Error.spUnwrapErrorT' for a more sophisticated version of this
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

instance (Monad m, MonadState s m) => MonadState s (ServerPartT m) where
    get = lift get
    put = lift . put

instance Monad m => FilterMonad Response (ServerPartT m) where
    setFilter = anyRequest . setFilter
    composeFilter = anyRequest . composeFilter
    getFilter m = withRequest $ \rq -> getFilter (runServerPartT m rq)

instance Monad m => WebMonad Response (ServerPartT m) where
    finishWith r = anyRequest $ finishWith r

-- | The 'ServerMonad' class provides methods for reading or locally
-- modifying the 'Request'. It is essentially a specialized version of
-- the 'MonadReader' class. Providing the unique names, 'askRq' and
-- 'localRq' makes it easier to use 'ServerPartT' and 'ReaderT'
-- together.
class Monad m => ServerMonad m where
    askRq   :: m Request
    localRq :: (Request -> Request) -> m a -> m a

instance (Monad m) => ServerMonad (ServerPartT m) where
    askRq = ServerPartT $ ask
    localRq f m = ServerPartT $ local f (unServerPartT m)

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

-- | turn a function into a 'FilterFun'. Primarily used with 'mapServerPartT'
filterFun :: (a -> a) -> FilterFun a
filterFun = Set . Dual . Endo

newtype FilterT a m b = FilterT { unFilterT :: Lazy.WriterT (FilterFun a) m b }
   deriving (Functor, Applicative, Monad, MonadTrans)

instance MonadCatch m => MonadCatch (FilterT a m) where
    catch action handle = FilterT $ catch (unFilterT action) (unFilterT . handle)

instance MonadThrow m => MonadThrow (FilterT a m) where
    throwM = FilterT . throwM

instance MonadBase b m => MonadBase b (FilterT a m) where
    liftBase = lift . liftBase

instance (MonadIO m) => MonadIO (FilterT a m) where
    liftIO = FilterT . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_monad_control(1,0,0)
instance MonadTransControl (FilterT a) where
    type StT (FilterT a) b = StT (Lazy.WriterT (FilterFun a)) b
    liftWith f = FilterT $ liftWith $ \run -> f $ run . unFilterT
    restoreT = FilterT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (FilterT a m) where
    type StM (FilterT a m) c = ComposeSt (FilterT a) m c
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
#else
instance MonadTransControl (FilterT a) where
    newtype StT (FilterT a) b = StFilter {unStFilter :: StT (Lazy.WriterT (FilterFun a)) b}
    liftWith f = FilterT $ liftWith $ \run -> f $ liftM StFilter . run . unFilterT
    restoreT = FilterT . restoreT . liftM unStFilter

instance MonadBaseControl b m => MonadBaseControl b (FilterT a m) where
    newtype StM (FilterT a m) c = StMFilter {unStMFilter :: ComposeSt (FilterT a) m c}
    liftBaseWith = defaultLiftBaseWith StMFilter
    restoreM     = defaultRestoreM     unStMFilter
#endif

-- | A set of functions for manipulating filters.
--
-- 'ServerPartT' implements 'FilterMonad' 'Response' so these methods
-- are the fundamental ways of manipulating 'Response' values.
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

-- | Resets all your filters. An alias for @'setFilter' 'id'@.
ignoreFilters :: (FilterMonad a m) => m ()
ignoreFilters = setFilter id

instance (Monad m) => FilterMonad a (FilterT a m) where
    setFilter     = FilterT . tell                . Set    . Dual . Endo
    composeFilter = FilterT . tell                . Append . Dual . Endo
    getFilter     = FilterT . listens unFilterFun . unFilterT

-- | The basic 'Response' building object.
newtype WebT m a = WebT { unWebT :: ErrorT Response (FilterT (Response) (MaybeT m)) a }
    deriving (Functor)

instance MonadCatch m => MonadCatch (WebT m) where
    catch action handle = WebT $ catch (unWebT action) (unWebT . handle)

instance MonadThrow m => MonadThrow (WebT m) where
    throwM = WebT . throwM

instance MonadBase b m => MonadBase b (WebT m) where
    liftBase = lift . liftBase

instance (MonadIO m) => MonadIO (WebT m) where
    liftIO = WebT . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_monad_control(1,0,0)
instance MonadTransControl WebT where
    type StT WebT a = StT MaybeT
                       (StT (FilterT Response)
                        (StT (ErrorT Response) a))
    liftWith f = WebT $ liftWith $ \runError ->
                          liftWith $ \runFilter ->
                            liftWith $ \runMaybe ->
                              f $ runMaybe .
                                   runFilter .
                                    runError . unWebT
    restoreT = WebT . restoreT . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (WebT m) where
    type StM (WebT m) a = ComposeSt WebT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
#else
instance MonadTransControl WebT where
    newtype StT WebT a = StWeb {unStWeb :: StT MaybeT
                                             (StT (FilterT Response)
                                               (StT (ErrorT Response) a))}
    liftWith f = WebT $ liftWith $ \runError ->
                          liftWith $ \runFilter ->
                            liftWith $ \runMaybe ->
                              f $ liftM StWeb . runMaybe .
                                                  runFilter .
                                                    runError . unWebT
    restoreT = WebT . restoreT . restoreT . restoreT . liftM unStWeb

instance MonadBaseControl b m => MonadBaseControl b (WebT m) where
    newtype StM (WebT m) a = StMWeb {unStMWeb :: ComposeSt WebT m a}
    liftBaseWith = defaultLiftBaseWith StMWeb
    restoreM     = defaultRestoreM     unStMWeb
#endif
-- | 'UnWebT' is almost exclusively used with 'mapServerPartT'. If you
-- are not using 'mapServerPartT' then you do not need to wrap your
-- head around this type. If you are -- the type is not as complex as
-- it first appears.
--
-- It is worth discussing the unpacked structure of 'WebT' a bit as
-- it's exposed in 'mapServerPartT' and 'mapWebT'.
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
    fail s = lift (fail s)

-- | 'WebMonad' provides a means to end the current computation
-- and return a 'Response' immediately.  This provides an
-- alternate escape route.  In particular it has a monadic value
-- of any type.  And unless you call @'setFilter' 'id'@ first your
-- response filters will be applied normally.
--
-- Extremely useful when you're deep inside a monad and decide
-- that you want to return a completely different content type,
-- since it doesn't force you to convert all your return types to
-- 'Response' early just to accommodate this.
--
-- see also: 'escape' and 'escape''
class Monad m => WebMonad a m | m->a where
    -- abort the current computation and return a value
    finishWith :: a -- ^ value to return (For 'ServerPart', 'a' will always be the type 'Response')
               -> m b

-- | Used to ignore all your filters and immediately end the
-- computation.  A combination of 'ignoreFilters' and 'finishWith'.
escape :: (WebMonad a m, FilterMonad a m) => m a -> m b
escape gen = ignoreFilters >> gen >>= finishWith

-- | An alternate form of 'escape' that can be easily used within a do
-- block.
escape' :: (WebMonad a m, FilterMonad a m) => a -> m b
escape' a = ignoreFilters >> finishWith a


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

instance (Monad m) => FilterMonad Response (WebT m) where
    setFilter f = WebT $ lift $ setFilter $ f
    composeFilter f = WebT . lift . composeFilter $ f
    getFilter     m = WebT $ ErrorT $ liftM lft $ getFilter (runErrorT $ unWebT m)
        where
          lft (Left  r, _) = Left r
          lft (Right a, f) = Right (a, f)

instance (Monad m) => Monoid (WebT m a) where
    mempty = mzero
    mappend = mplus

-- | For when you really need to unpack a 'WebT' entirely (and not
-- just unwrap the first layer with 'unWebT').
ununWebT :: WebT m a -> UnWebT m a
ununWebT = runMaybeT . Lazy.runWriterT . unFilterT . runErrorT . unWebT

-- | For wrapping a 'WebT' back up.  @'mkWebT' . 'ununWebT' = 'id'@
mkWebT :: UnWebT m a -> WebT m a
mkWebT = WebT . ErrorT . FilterT . Lazy.WriterT . MaybeT

-- | See 'mapServerPartT' for a discussion of this function.
mapWebT :: (UnWebT m a -> UnWebT n b)
        -> (  WebT m a ->   WebT n b)
mapWebT f ma = mkWebT $ f (ununWebT ma)

-- | This is kinda like a very oddly shaped 'mapServerPartT' or 'mapWebT'.
-- You probably want one or the other of those.
localContext :: Monad m => (WebT m a -> WebT m' a) -> ServerPartT m a -> ServerPartT m' a
localContext fn hs
    = withRequest $ \rq -> fn (runServerPartT hs rq)

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

-- | Deprecated: use 'msum'.
multi :: Monad m => [ServerPartT m a] -> ServerPartT m a
multi = msum
{-# DEPRECATED multi "Use msum instead" #-}

-- | What is this for, exactly?  I don't understand why @Show a@ is
-- even in the context Deprecated: This function appears to do nothing
-- at all. If it use it, let us know why.
debugFilter :: (MonadIO m, Show a) => ServerPartT m a -> ServerPartT m a
debugFilter handle =
    withRequest $ \rq -> do
                    r <- runServerPartT handle rq
                    return r
{-# DEPRECATED debugFilter "This function appears to do nothing." #-}


-- "Pattern match failure in do expression at src\AppControl.hs:43:24"
-- is converted to:
-- "src\AppControl.hs:43:24: Pattern match failure in do expression"
-- Then we output this to stderr. Help debugging under Emacs console when using GHCi.
-- This is GHC specific, but you may add your favourite compiler here also.
outputTraceMessage :: String -> a -> a
outputTraceMessage s c | "Pattern match failure " `isPrefixOf` s =
    let w = [(k,p) | (i,p) <- zip (tails s) (inits s), Just k <- [stripPrefix " at " i]]
        v = concatMap (\(k,p) -> k ++ ": " ++ p) w
    in trace v c
outputTraceMessage s c = trace s c


mkFailMessage :: (FilterMonad Response m, WebMonad Response m) => String -> m b
mkFailMessage s = do
    ignoreFilters
    finishWith (failResponse s)

failResponse :: String -> Response
failResponse s =
    setHeader "Content-Type" "text/html; charset=UTF-8" $
     resultBS 500 (LU.fromString (failHtml s))

failHtml:: String->String
failHtml errString =
   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
    ++ "<html><head><title>Happstack "
    ++ ver ++ " Internal Server Error</title></head>"
    ++ "<body><h1>Happstack " ++ ver ++ "</h1>"
    ++ "<p>Something went wrong here<br>"
    ++ "Internal server error<br>"
    ++ "Everything has stopped</p>"
    ++ "<p>The error was \"" ++ (escapeString errString) ++ "\"</p></body></html>"
    where ver = DV.showVersion Cabal.version

escapeString :: String -> String
escapeString str = concatMap encodeEntity str
    where
      encodeEntity :: Char -> String
      encodeEntity '<' = "&lt;"
      encodeEntity '>' = "&gt;"
      encodeEntity '&' = "&amp;"
      encodeEntity '"' = "&quot;"
      encodeEntity c
          | ord c > 127 = "&#" ++ show (ord c) ++ ";"
          | otherwise = [c]

------------------------------------------------------------------------------
-- ServerMonad, FilterMonad, and WebMonad instances for ReaderT, StateT,
-- WriterT, RWST, and ErrorT
------------------------------------------------------------------------------

-- ReaderT

instance (ServerMonad m) => ServerMonad (ReaderT r m) where
    askRq         = lift askRq
    localRq f     = mapReaderT (localRq f)

instance (FilterMonad res m) => FilterMonad res (ReaderT r m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter     = mapReaderT getFilter

instance (WebMonad a m) => WebMonad a (ReaderT r m) where
    finishWith    = lift . finishWith

-- StateT

instance (ServerMonad m) => ServerMonad (Lazy.StateT s m) where
    askRq         = lift askRq
    localRq f     = Lazy.mapStateT (localRq f)

instance (ServerMonad m) => ServerMonad (Strict.StateT s m) where
    askRq         = lift askRq
    localRq f     = Strict.mapStateT (localRq f)

instance (FilterMonad res m) => FilterMonad res (Lazy.StateT s m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter   m = Lazy.mapStateT (\m' ->
                                   do ((b,s), f) <- getFilter m'
                                      return ((b, f), s)) m

instance (FilterMonad res m) => FilterMonad res (Strict.StateT s m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter   m = Strict.mapStateT (\m' ->
                                   do ((b,s), f) <- getFilter m'
                                      return ((b, f), s)) m

instance (WebMonad a m) => WebMonad a (Lazy.StateT s m) where
    finishWith    = lift . finishWith

instance (WebMonad a m) => WebMonad a (Strict.StateT s m) where
    finishWith    = lift . finishWith

-- WriterT

instance (ServerMonad m, Monoid w) => ServerMonad (Lazy.WriterT w m) where
    askRq         = lift askRq
    localRq f     = Lazy.mapWriterT (localRq f)

instance (ServerMonad m, Monoid w) => ServerMonad (Strict.WriterT w m) where
    askRq         = lift askRq
    localRq f     = Strict.mapWriterT (localRq f)

instance (FilterMonad res m, Monoid w) => FilterMonad res (Lazy.WriterT w m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter   m = Lazy.mapWriterT (\m' ->
                                   do ((b,w), f) <- getFilter m'
                                      return ((b, f), w)) m

instance (FilterMonad res m, Monoid w) => FilterMonad res (Strict.WriterT w m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter   m = Strict.mapWriterT (\m' ->
                                   do ((b,w), f) <- getFilter m'
                                      return ((b, f), w)) m

instance (WebMonad a m, Monoid w) => WebMonad a (Lazy.WriterT w m) where
    finishWith    = lift . finishWith

instance (WebMonad a m, Monoid w) => WebMonad a (Strict.WriterT w m) where
    finishWith    = lift . finishWith

-- RWST

instance (ServerMonad m, Monoid w) => ServerMonad (Lazy.RWST r w s m) where
    askRq         = lift askRq
    localRq f     = Lazy.mapRWST (localRq f)

instance (ServerMonad m, Monoid w) => ServerMonad (Strict.RWST r w s m) where
    askRq         = lift askRq
    localRq f     = Strict.mapRWST (localRq f)

instance (FilterMonad res m, Monoid w) => FilterMonad res (Lazy.RWST r w s m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter   m = Lazy.mapRWST (\m' ->
                                   do ((b,s,w), f) <- getFilter m'
                                      return ((b, f), s, w)) m

instance (FilterMonad res m, Monoid w) => FilterMonad res (Strict.RWST r w s m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter   m = Strict.mapRWST (\m' ->
                                   do ((b,s,w), f) <- getFilter m'
                                      return ((b, f), s, w)) m

instance (WebMonad a m, Monoid w) => WebMonad a (Lazy.RWST r w s m) where
    finishWith     = lift . finishWith

instance (WebMonad a m, Monoid w) => WebMonad a (Strict.RWST r w s m) where
    finishWith     = lift . finishWith

-- ErrorT

instance (Error e, ServerMonad m) => ServerMonad (ErrorT e m) where
    askRq     = lift askRq
    localRq f = mapErrorT $ localRq f

instance (Error e, FilterMonad a m) => FilterMonad a (ErrorT e m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter m = mapErrorT (\m' ->
                                 do (eb, f) <- getFilter m'
                                    case eb of
                                      (Left e)  -> return (Left e)
                                      (Right b) -> return $ Right (b, f)
                  ) m

instance (Error e, WebMonad a m) => WebMonad a (ErrorT e m) where
    finishWith    = lift . finishWith

-- ExceptT

instance ServerMonad m => ServerMonad (ExceptT e m) where
    askRq     = lift askRq
    localRq f = mapExceptT $ localRq f

instance (FilterMonad a m) => FilterMonad a (ExceptT e m) where
    setFilter f   = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter m = mapExceptT (\m' ->
                                 do (eb, f) <- getFilter m'
                                    case eb of
                                      (Left e)  -> return (Left e)
                                      (Right b) -> return $ Right (b, f)
                  ) m

instance WebMonad a m => WebMonad a (ExceptT e m) where
    finishWith    = lift . finishWith
