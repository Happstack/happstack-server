module Main where

import Happstack.Server
import System.Directory
import Control.Monad.Trans

{-
  interesting urls:
   /
-}
main :: IO ()
main = simpleHTTP nullConf $
             do contents <- liftIO $ getDirectoryContents "."
                ok $ unlines contents
