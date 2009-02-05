module Main where

import HAppS.Server
import System.Directory
import Control.Monad.Trans

{-
  interesting urls:
   /
-}
main :: IO ()
main = do simpleHTTP nullConf
             [ anyRequest $ do contents <- liftIO $ getDirectoryContents "."
                               ok $ unlines contents
             ]
