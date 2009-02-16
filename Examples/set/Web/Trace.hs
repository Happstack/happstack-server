module Main where

import Happstack.Server
import System.Directory
import Control.Monad.Trans
import Control.Monad.Writer

{-
  interesting urls:
   /
   /special/
   /special/?query
-}

trace name local = do val <- local
                      anyRequest $ lift $ tell $ Endo (name:) -- Only append 'name' when 'local' was successful.
                      return val

transform :: WebT (Writer (Endo [String])) String -> WebT IO String
transform (WebT fn)
    = let (res,t) = runWriter fn
      in WebT $ return $ Ok id $ "Context:\n\n" ++ unlines (reverse $ appEndo t []) ++ "\n\n" ++ show res

main :: IO ()
main = do simpleHTTP nullConf
             [ localContext transform
               [ trace "special dir" $ dir "special" [ trace "query" $
                                                       withDataFn (look "query") $ \str -> [ anyRequest $ return str ]
                                                     , trace "otherwise" $
                                                       anyRequest $ return "special" ]
               , trace "default" $ anyRequest $ return "default"
               ]
             ]
