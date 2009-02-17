{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell , FlexibleInstances, UndecidableInstances, OverlappingInstances,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

import Happstack.Server

import Happstack.State
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State (modify,put,get,gets)
import Data.Generics hiding ((:+:))
import Happstack.Data
import Happstack.Data.IxSet

import qualified Data.Map as M

------------------------------------------------
-- Define a component of state 
--
-- Real examples are HelpReqs, FlashMsgs, and sessions
-- really you should put components in their own modules.
----------------------------------------------

-- State is global and composed of components that have component
-- specific methods.  The system generates special instance
-- declarations to access the component inside the global state.

-- Lets start with defining a simple state component: Session
type SesKey = Integer
type ETime = Integer
newtype OldSession val = OldSession {old_unsession::[(SesKey,(ETime,val))]} 
                   deriving (Typeable)

instance Version (OldSession val)
$(deriveSerialize ''OldSession)


newtype Session val = Session { unsession :: M.Map SesKey (ETime,val) }
                   deriving (Typeable)

instance Migrate (OldSession val) (Session val) where
    migrate (OldSession sess) = Session (M.fromList sess)

instance Serialize val => Version (Session val) where
    mode = extension 1 (Proxy :: Proxy (OldSession val))
$(deriveSerialize ''Session)



-- Note that we don't use the list directly because we may want this
-- list type for other purposes so we make it a newtype.  Now since
-- all methods are going to be inside our Update (aka State) or Query
-- (aka Reader) monads, it is useful to define some accessors.  the
-- typesig is necessary for askSession because we don't know the type
-- until the end.
askSession::MonadReader (Session val) m => m (M.Map SesKey (ETime,val))
askSession = return . unsession =<< ask
modSession f = modify (Session . f . unsession)



-- Now define some methods that will operate on Session state.
newSession val = do
                 key <- getRandom
                 t <- getTime
                 modSession $ M.insert key (t,val)
                 return key

getSession :: SesKey -> Query (Session val) (Maybe val)
getSession key = do val <- liftM (M.lookup key) askSession
                    return (liftM snd val)

setSession key val = do 
                     t <- getTime
                     modSession $ M.insert key (t,val)
                     return ()



-- Numsessions and cleansessions take a proxy type as an argument so
-- we know which session you want.  You may have sessions on more than
-- one type in state operating or sessions may be nested elsewhere.
-- You can only have one of each type in all of state.

--cleanSessions :: Proxy (Session key) -> ETime -> Update (Session key) ()
cleanSessions age = proxyUpdate $ do
                                  t <- getTime
                                  let minTime = t-age
                                  modSession $ M.filterWithKey (\k _ -> k>t)
                                  return ()


-- The type sig is required for reasons I don't understand
numSessions:: Proxy (Session val) -> Query (Session val) Int
numSessions = proxyQuery $ liftM M.size askSession

-- Declare these as methods. So you can access them from any IO via (query $
-- GetSession key) or (update $ setSession key val).  When we can have
-- Data for phantom types in 6.8.2 this will look nicer 

$(mkMethods ''Session 
  ['newSession,'setSession, 'cleanSessions,'numSessions ,'getSession]) 

-- Sometimes you want maintenance on your component that the user
-- doesn't want to worry about.

maintainSessions v = do update $ CleanSessions 3600000 v
                        threadDelay (10^6 * 10) -- Once every 10 seconds
                        maintainSessions v 

instance (Serialize a) => Component (Session a) where
    type Dependencies (Session a) = End
    initialValue = Session M.empty

-- All components need an atStart declaration though the list can be empty              

-- Now we repeat the above for a more trivial example so we have
-- multiple components in state.  But we'll use the more concise deriveAll syntax 
-- so you don't deal with the boilerplate of a zillion deriving declarations on each type.

data UserComponent key = UserComponent {unUserComponent :: key} deriving (Typeable)
data SingletonComponent = SingletonComponent {unSingleton :: String} deriving (Typeable)

instance Version (UserComponent key)
$(deriveSerialize ''UserComponent)
instance Version SingletonComponent
$(deriveSerialize ''SingletonComponent)


-- methods definition for these two components
setSingleton str = put (SingletonComponent str)
-- need an argument or to disable the monomorphism restriction or a type-sig
getSingleton () = liftM unSingleton ask
setComponent c = put (UserComponent c)
getComponent () = liftM unUserComponent ask

-- method declarations
$(mkMethods ''UserComponent ['getComponent,'setComponent])
$(mkMethods ''SingletonComponent ['setSingleton,'getSingleton])

-- now you can use (query GetComponent) and (update $ SetComponent c)
-- with any state that has one field of type Component

singletonIO Proxy
    = do putStrLn "Initializing singleton component"
         update $ SetSingleton "init"

-- this is complex because we want this to work even though the methods don't need proxies
-- we need userComponent to initialize against each different type inside state.
userComponentIO :: forall key. Serialize key => Proxy (UserComponent key) -> IO ()
userComponentIO proxy
    = do putStrLn $ "Initializing component of type: " ++ show (typeOf (unProxy proxy))
         query (GetComponent ()) :: IO key
         return ()

instance (Default key, Serialize key) => Component (UserComponent key) where
    type Dependencies (UserComponent key) = End
    onLoad = userComponentIO
    initialValue = UserComponent defaultValue

instance Component SingletonComponent where
    type Dependencies SingletonComponent = End
    onLoad = singletonIO
    initialValue = SingletonComponent ""

---------------------------------------------------------------------
-- Now lets define a state that has its own methods and uses some components.
------------------------------------------------------------------------
{-- This also works
$(deriveAll [''Show,''Default, ''Read]
    [d|
        data State = State { privateInt     :: Int
                           , privateString  :: String
                           , someComponent1 :: Component (UserComponent Int)
                           , someComponent2 :: Component (UserComponent String)
                           , singleton      :: Component SingletonComponent
                           , sessions       :: Component (Session String)
                           }
     |]
   )
--}

data State = State { privateInt     :: Int
                   , privateString  :: String
                   } deriving (Typeable)

instance Version State
$(deriveSerialize ''State)

-- Bind privateInt and privateString in a tuple.
getPrivateData () = liftM2 (,) (asks privateInt) (asks privateString)

setPrivateData int string = modify $ \s -> s{privateInt = int
                                            ,privateString = string}

-- notice that state is also a component with methods

$(mkMethods ''State ['getPrivateData, 'setPrivateData])

instance Component State where
    type Dependencies State = UserComponent Int :+:
                              UserComponent String :+:
                              SingletonComponent :+:
                              Session String :+:
                              End
    initialValue = State 0 ""


----------------------------------------------------
--  Now we define the HTTP interface to test stuff
----------------------------------------
impl = dir "setGet" $ msum
        [--return text/plain of the string inside component
         --you can return a type and have it convert automatically to XML (see below)
         --you can return Text.HTML and Text.XHTML and they will be handled properly too
         do
            methodM GET
            ok ()
            liftIO $ query $ GetComponent ()
         --method GET $ ok =<< (webQuery (GetComponent ()) :: Web Int)

         -- receive  urlencoded or mimemultipart of ?component=blah
         -- handle other encodings by defining your own FromData
        , do
             methodM POST
             mbComp <- getData
             comp <- maybe mzero return mbComp
             liftIO $ update $ SetComponent (comp :: Int)
             ok comp -- returned as <?xml v=1.0?><component>blah</component>. 
             -- add the xslt wrapper to style the xml
             -- or write your own ToMessage instance for your return types
        ]

-- and a test we can run from anywhere
ioTest = do print =<< query (GetPrivateData ())
            update $ SetPrivateData 10 "Hello world"
            print =<< query (GetPrivateData ())
            update $ SetComponent (10::Int)
            print =<< (query (GetComponent ()) :: IO Int)
            update $ SetSingleton "Hello HAppS from Haskell"
            putStrLn =<< query (GetSingleton ())

entryPoint :: Proxy State
entryPoint = Proxy

main = do control <- startSystemState entryPoint
          tid <- forkIO $ simpleHTTP nullConf impl
{-
          readEvent <- getEventStream
          forkIO $ forever $ do event <- readEvent
                                putStrLn $ "New event: " ++ show event
-}
          ioTest
          waitForTermination
          killThread tid
          shutdownSystem control
