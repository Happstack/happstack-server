{-# OPTIONS -fglasgow-exts -fth #-}
module Main (main) where

import Happstack.State
import Happstack.Server

import Data.Typeable          ( Typeable )
import System.Environment     ( getArgs, getProgName )
import System.Exit            ( exitWith, ExitCode(ExitFailure) )
import Control.Monad.State    ( put, get)
import Control.Monad.Reader   ( ask, liftM2, liftIO )
import Control.Exception      ( bracket )
import Data.List              ( intercalate )
import Data.Dynamic           ( fromDynamic )

type Nick = String
type Message = String
type MessageId = Int

data User = User { userNick     :: Nick
                 , userLastSeen :: MessageId }

data ChatState = ChatState MessageId [ (Nick, Message, MessageId) ] deriving (Typeable)
instance Version ChatState
$(deriveSerialize ''ChatState)

instance Component ChatState where
    type Dependencies ChatState = End
    initialValue = ChatState 2 [ ("System", "Welcome to the distributed chat system", 1) ]

listMessages :: Query ChatState [(Nick, Message, MessageId)]
listMessages = do ChatState _ msgs <- ask
                  return msgs

addMessage :: Nick -> Message -> Update ChatState ()
addMessage nick message
    = do ChatState mid msgs <- get
         put $ ChatState (mid+1) $ take 20 ((nick,message,mid):msgs)

$(mkMethods ''ChatState [ 'listMessages, 'addMessage ])

-- Wait for a new message to appear.
getMessages last
    = do stream <- getEventStream
         msgs <- query $ ListMessages
         case msgs of
           ((_,_,mid):_) | mid > last -> return (mid,msgs)
           _ -> do waitForAdd stream
                   getMessages last
    where waitForAdd s = do ev <- s
                            case fromDynamic (eventData ev) of
                              Nothing -> waitForAdd s
                              Just AddMessage{} -> return ()

rootState :: Proxy ChatState
rootState = Proxy

getUserFromCookie = liftM2 User (lookCookieValue "nick") (readCookieValue "last")

getPort = do args <- getArgs
             case args of
               [portStr] | [(port,"")] <- reads portStr -> return port
               _  -> do prog <- getProgName
                        putStrLn $ "Usage: " ++ prog ++ " port"
                        exitWith (ExitFailure 1)

main :: IO ()
main = bracket (startSystemStateMultimaster rootState) closeTxControl $ \ctl ->
       do port <- getPort
          simpleHTTP nullConf{port=port}
                         [ withDataFn getUserFromCookie $ \user ->
                            [ dir "send"
                              [ withDataFn (look "msg") $ \msg ->
                                [ anyRequest $ do update $ AddMessage (userNick user) msg
                                                  ok (toResponse "OK")]
                              ]
                            , dir "get"
                              [ anyRequest $ do (newLast, msgs) <- liftIO $ getMessages (userLastSeen user)
                                                addCookie (-1) (mkCookie "last" (show newLast))
                                                ok (toResponse (format msgs))
                              ]
                            , dir "clear"
                              [ anyRequest $ do addCookie (-1) (mkCookie "last" (show 0))
                                                ok (toResponse "")
                              ]
                            , fileServe [] "ChatRun.html"
                            ]
                         , dir "login"
                           [ withDataFn (look "nick") $ \nick ->
                             [ anyRequest $ do addCookie (-1) (mkCookie "nick" nick)
                                               addCookie (-1) (mkCookie "last" (show 0))
                                               seeOther "/" (toResponse "")
                             ]
                           ]
                         , fileServe [] "ChatLogin.html"
                         ]
          return ()

format = intercalate "<br/>" . map fn
    where fn (nick, msg, mid) = nick ++ ": " ++ msg
