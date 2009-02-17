{-# LANGUAGE FlexibleContexts #-}

module Main where

import Happstack.Server
import System.Directory
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad(msum)

{-
  interesting urls:
   /
   /special/
   /special/?query
-}
trace :: (Monad m, MonadWriter (Endo [String]) m) => String -> String -> m String
trace name result = (tell $ Endo (name:)) >> return result

transform :: (Monad m) => (WriterT (Endo [String]) m) (Maybe (Either Response String, SetAppend (Endo Response)))
             -> m (Maybe (Either Response String, SetAppend (Endo Response)))
transform wt = do
    (res, t) <- runWriterT wt
    case res of
        Just (Right r, f) -> return $ Just $ (Right $ context t r,f)
        _ -> return res
    where context t r = "Context:\n\n" ++ unlines (reverse $ appEndo t []) ++ "\n\n" ++ show r

main :: IO ()
main = do simpleHTTP' transform nullConf $ msum
               [ trace "special dir" =<< (dir "special" $ msum
                    [   
                        do mbStr <- getDataFn (look "query")
                           str <- maybe mzero return mbStr
                           trace "query" str
                        
                       ,trace "otherwise" =<< return "special"
                    ])
                    
                ,trace "default" "default"
               ]
