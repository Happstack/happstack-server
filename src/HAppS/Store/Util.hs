{-# LANGUAGE TemplateHaskell , FlexibleInstances, UndecidableInstances, CPP,
             OverlappingInstances, DeriveDataTypeable, MultiParamTypeClasses #-}

module HAppS.Store.Util where
import HAppS.Data
import GHC.Conc
import HAppS.State
import Control.Monad.State
import HAppS.Data.IxSet
import HAppS.Data.Atom
import Language.Haskell.TH

import Control.Monad.State 



--interface with State

$( deriveAll [''Show,''Default,''Read,''Eq,''Ord]
   [d|
       newtype Context = Context String  --this belongs elsewhere!
       newtype EpochTime = EpochTime Integer       
       data Wrap a = Wrap {unwrap::a}
       |])

type With st' st a = Ev (StateT st' STM) a -> Ev (StateT st STM) a


byTime::(Typeable a) => IxSet a -> [a]
byTime = concat . map (\(Published _,es)->es) . groupBy
byRevTime::(Typeable a) => IxSet a -> [a]
byRevTime = concat . map (\(Published _,es)->es) . rGroupBy


fun0_1 :: String -> String -> String -> Dec
fun0_1 name fun arg = 
    FunD (mkName name)  
             [Clause [] (NormalB (AppE (VarE $ mkName fun) 
                                           (ConE $ mkName arg))) 
              []
             ]
fun0_2 :: String -> String -> String -> String -> Dec
fun0_2 name fun arg1 arg2 = 
    FunD (mkName name)  
             [Clause [] (NormalB 
                         (AppE (AppE (VarE $ mkName fun) 
                                (ConE $ mkName arg1))
                                (ConE $ mkName arg2)))
              []
             ]
