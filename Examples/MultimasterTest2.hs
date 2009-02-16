{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Main where

import Happstack.Server
import Happstack.State

import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader

data MyState = MyState Int deriving (Typeable)
instance Version MyState
$(deriveSerialize ''MyState)

succVal :: Update MyState ()
succVal = modify (\(MyState n) -> MyState (succ n))
predVal :: Update MyState ()
predVal = modify (\(MyState n) -> MyState (pred n))

getVal :: Query MyState Int
getVal = do MyState n <- ask
            return n

$(mkMethods ''MyState [ 'succVal
                      , 'predVal
                      , 'getVal])

instance Component MyState where
    type Dependencies MyState = End
    initialValue = MyState 0

rootState :: Proxy MyState
rootState = Proxy

main :: IO ()
main = do ctl <- startSystemStateMultimaster rootState
          simpleHTTP nullConf{port=8001}
              [ dir "succ" [ anyRequest $ do update SuccVal
                                             seeOther "/" "" ]
              , dir "pred" [ anyRequest $ do update PredVal
                                             seeOther "/" "" ]
              , anyRequest $ do val <- query GetVal
                                ok $ "Value is: " ++ show val ]

