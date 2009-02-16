module Main where

import Happstack.Server
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
