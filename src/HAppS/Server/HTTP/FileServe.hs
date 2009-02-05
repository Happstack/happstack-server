module HAppS.Server.HTTP.FileServe
    (
     MimeMap,fileServe, mimeTypes,isDot, blockDotFiles,doIndex,errorwrapper
    ) where

import Control.Exception.Extensible

import Control.Monad.Reader
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Data.Int
import HAppS.Server.SimpleHTTP hiding (path)
import System.Directory
import System.IO
import System.Locale(defaultTimeLocale)
import System.Log.Logger
import System.Time -- (formatCalendarTime, toUTCTime,TOD(..))
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as Map
import qualified HAppS.Server.SimpleHTTP as SH

ioErrors :: SomeException -> Maybe IOException
ioErrors = fromException

errorwrapper :: MonadIO m => String -> String -> ServerPartT m Response
errorwrapper binarylocation loglocation
    = require getErrorLog $ \errorLog ->
      --[method () $ SH.ok errorLog]
      [anyRequest $ liftIO $ return $ toResponse errorLog]
    where getErrorLog
              = liftIO $
                handleJust ioErrors (const (return Nothing)) $
                do bintime <- getModificationTime binarylocation
                   logtime <- getModificationTime loglocation
                   if (logtime > bintime)
                     then fmap Just $ readFile loglocation -- fileServe [loglocation] [] "./"
                     else return Nothing

type MimeMap = Map.Map String String

doIndex :: (MonadIO m) =>
           [String] -> Map.Map String String -> Request -> String -> WebT m Response
doIndex [] _mime _rq _fp = do setResponseCode 403
                              return $ toResponse "Directory index forbidden"
doIndex (index:rest) mime rq fp =
    do
    let path = fp++'/':index
    --print path
    fe <- liftIO $ doesFileExist path
    if fe then retFile path else doIndex rest mime rq fp
    where retFile = returnFile mime rq
defaultIxFiles :: [String]
defaultIxFiles= ["index.html","index.xml","index.gif"]

fileServe :: MonadIO m => [FilePath] -> FilePath -> ServerPartT m Response
fileServe ixFiles localpath  = 
    withRequest (fileServe' localpath (doIndex (ixFiles++defaultIxFiles)) mimeTypes)

-- | Serve files with a mime type map under a directory.
--   Uses the function to transform URIs to FilePaths.
fileServe' :: (MonadIO m) =>
              String
              -> (Map.Map String String -> Request -> String -> WebT m Response)
              -> Map.Map String String
              -> Request
              -> WebT m Response
fileServe' localpath fdir mime rq = do
    let fp2 = takeWhile (/=',') fp
        fp = filepath
        safepath = filter (\x->not (null x) && head x /= '.') (rqPaths rq)
        filepath = intercalate "/"  (localpath:safepath)
        fp' = if null safepath then "" else last safepath
    if "TESTH" `isPrefixOf` fp'
        then renderResponse mime rq $ fakeFile $ (read $ drop 5 $ fp' :: Integer)
        else do
    fe <- liftIO $ doesFileExist fp
    fe2 <- liftIO $ doesFileExist fp2
    de <- liftIO $ doesDirectoryExist fp
    -- error $ "show ilepath: " ++show (fp,de)
    let status | de   = "DIR"
               | fe   = "file"
               | fe2  = "group"
               | True = "NOT FOUND"
    liftIO $ logM "HAppS.Server.HTTP.FileServe" INFO ("fileServe: "++show fp++" \t"++status)
    if de then fdir mime rq fp else do
    getFile mime fp >>= flip either (renderResponse mime rq) 
                (const $ returnGroup localpath mime rq safepath)

returnFile :: (MonadIO m) =>
              Map.Map String String -> Request -> String -> WebT m Response
returnFile mime rq fp =  
    getFile mime fp >>=  either fileNotFound (renderResponse mime rq)

-- if fp has , separated then return concatenation with content-type of last
-- and last modified of latest
tr :: (Eq a) => a -> a -> [a] -> [a]
tr a b list = map (\x->if x==a then b else x) list
ltrim :: String -> String
ltrim = dropWhile (flip elem " \t\r")   

returnGroup :: (MonadIO m) =>
               String -> Map.Map String String -> Request -> [String] -> WebT m Response
returnGroup localPath mime rq fp = do
  let fps0 = map ((:[]). ltrim) $ lines $ tr ',' '\n' $ last fp
      fps = map (intercalate "/" . ((localPath:init fp) ++)) fps0

  -- if (head $ head fps0)=="TEST" then   renderResponse mime rq fakeFile else do

  mbFiles <-  mapM (getFile mime) $ fps
  let notFounds = [x | Left x <- mbFiles]
      files = [x | Right x <- mbFiles]
  if not $ null notFounds 
    then fileNotFound $ drop (length localPath) $ head notFounds else do
  let totSize = sum $ map (snd . fst) files
      maxTime = maximum $ map (fst . fst) files :: ClockTime

  renderResponse mime rq ((maxTime,totSize),(fst $ snd $ head files,
                                             L.concat $ map (snd . snd) files))



fileNotFound :: (Monad m) => String -> WebT m Response
fileNotFound fp = do setResponseCode 404 
                     return $ toResponse $ "File not found "++ fp
--fakeLen = 71* 1024
fakeFile :: (Integral a) =>
            a -> ((ClockTime, Int64), (String, L.ByteString))
fakeFile fakeLen = ((TOD 0 0,L.length body),("text/javascript",body))
    where
      body = L.pack $ (("//"++(show len)++" ") ++ ) $ (take len $ repeat '0') ++ "\n"
      len = fromIntegral fakeLen

getFile :: (MonadIO m) =>
           Map.Map String String
           -> String
           -> m (Either String ((ClockTime, Integer), (String, L.ByteString)))
getFile mime fp = do
  let ct = Map.findWithDefault "text/plain" (getExt fp) mime
  fe <- liftIO $ doesFileExist fp
  if not fe then return $ Left fp else do
  
  time <- liftIO  $ getModificationTime fp
  h <- liftIO $ openBinaryFile fp ReadMode
  size <- liftIO $ hFileSize h
  lbs <- liftIO $ L.hGetContents h
  return $ Right ((time,size),(ct,lbs))



renderResponse :: (Monad m,
                   Show t1) =>
                  t
                  -> Request
                  -> ((ClockTime, t1), (String, L.ByteString))
                  -> WebT m Response
renderResponse _ rq ((modtime,size),(ct,body)) = do

  let notmodified = getHeader "if-modified-since" rq == Just (P.pack $ repr)
      repr = formatCalendarTime defaultTimeLocale 
             "%a, %d %b %Y %X GMT" (toUTCTime modtime)
  -- "Mon, 07 Jan 2008 19:51:02 GMT"
  -- when (isJust $ getHeader "if-modified-since"  rq) $ error $ show $ getHeader "if-modified-since" rq
  if notmodified then do setResponseCode 304 ; return $ toResponse "" else do
  --  modifyResponse (setHeader "HUH" $ show $ (fmap P.unpack mod == Just repr,mod,Just repr))
  modifyResponse (setHeader "Last-modified" repr)
  -- if %Z or UTC are in place of GMT below, wget complains that the last-modified header is invalid
  modifyResponse (setHeader "Content-Length" (show size))
  modifyResponse (setHeader "Content-Type" ct)  
  return $ resultBS 200 body

              


getExt :: String -> String
getExt fPath = reverse $ takeWhile (/='.') $ reverse fPath

-- | Ready collection of common mime types.
mimeTypes :: MimeMap
mimeTypes = Map.fromList
	    [("xml","application/xml")
	    ,("xsl","application/xml")
	    ,("js","text/javascript")
	    ,("html","text/html")
	    ,("css","text/css")
	    ,("gif","image/gif")
	    ,("jpg","image/jpeg")
	    ,("png","image/png")
	    ,("txt","text/plain")
	    ,("doc","application/msword")
	    ,("exe","application/octet-stream")
	    ,("pdf","application/pdf")
	    ,("zip","application/zip")
	    ,("gz","application/x-gzip")
	    ,("ps","application/postscript")
	    ,("rtf","application/rtf")
	    ,("wav","application/x-wav")
	    ,("hs","text/plain")]



blockDotFiles :: (Request -> IO Response) -> Request -> IO Response
blockDotFiles fn rq
    | isDot (intercalate "/" (rqPaths rq)) = return $ result 403 "Dot files not allowed."
    | otherwise = fn rq

isDot :: String -> Bool
isDot = isD . reverse
    where
    isD ('.':'/':_) = True
    isD ['.']       = True
    --isD ('/':_)     = False
    isD (_:cs)      = isD cs
    isD []          = False
