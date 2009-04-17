{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Happstack.Server.HTTP.FileServe
    (
     MimeMap,
     blockDotFiles,
     doIndex,
     doIndexStrict,
     errorwrapper,
     fileServe,
     fileServeStrict,
     isDot,
     mimeTypes
    ) where

import Control.Exception.Extensible

import Control.Monad.Reader
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Data.Int
import Happstack.Server.SimpleHTTP hiding (path)
import System.Directory
import System.IO
import System.Locale(defaultTimeLocale)
import System.Log.Logger
import System.Time -- (formatCalendarTime, toUTCTime,TOD(..))
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as Map
import qualified Happstack.Server.SimpleHTTP as SH

ioErrors :: SomeException -> Maybe IOException
ioErrors = fromException

errorwrapper :: (MonadIO m, MonadPlus m, FilterMonad Response m) => String -> String -> m Response
errorwrapper binarylocation loglocation
    = require getErrorLog $ \errorLog ->
      return $ toResponse errorLog
    where getErrorLog
                = handleJust ioErrors (const (return Nothing)) $
                do bintime <- getModificationTime binarylocation
                   logtime <- getModificationTime loglocation
                   if (logtime > bintime)
                     then fmap Just $ readFile loglocation
                     else return Nothing


type MimeMap = Map.Map String String

type GetFileFunc = (MonadIO m) =>
    Map.Map String String
    -> String
    -> m (Either String ((ClockTime, Integer), (String, L.ByteString)))



doIndex :: (ServerMonad m, FilterMonad Response m, MonadIO m) =>
           [String] -> MimeMap -> String -> m Response
doIndex = doIndex' getFile

-- | A variant of 'doIndex' that relies on 'getFileStrict'
doIndexStrict :: (ServerMonad m, FilterMonad Response m, MonadIO m) =>
                 [String] -> MimeMap -> String -> m Response
doIndexStrict = doIndex' getFileStrict


doIndex' :: (ServerMonad m, FilterMonad Response m, MonadIO m) =>
            GetFileFunc
         -> [String]
         -> MimeMap
         -> String
         -> m Response
doIndex' _getFileFunc []           _mime _fp = forbidden $ toResponse "Directory index forbidden"
doIndex'  getFileFunc (index:rest)  mime  fp =
    do
    let path = fp++'/':index
    --print path
    fe <- liftIO $ doesFileExist path
    if fe then retFile path else doIndex rest mime fp
    where retFile = returnFile getFileFunc mime



defaultIxFiles :: [String]
defaultIxFiles= ["index.html","index.xml","index.gif"]


-- | Serve a file (lazy version). For efficiency reasons when serving large
-- files, will escape the computation early if a file is successfully served,
-- to prevent filters from being applied; if a filter were applied, we would
-- need to compute the content-length (thereby forcing the spine of the
-- ByteString into memory) rather than reading it from the filesystem.
--
-- Note that using lazy fileServe can result in some filehandles staying open
-- until the garbage collector gets around to closing them.
fileServe :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m) =>
             [FilePath]         -- ^ index files if the path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServe ixFiles localpath = do
    resp <- fileServe' localpath
                       (doIndex (ixFiles++defaultIxFiles))
                       mimeTypes
                       getFile

    escape' $ resp { rsFlags = RsFlags {rsfContentLength=False} }



-- | Serve a file (strict version). Reads the entire file strictly into
-- memory, and ensures that the handle is properly closed. Unlike lazy
-- fileServe, this function doesn't shortcut the computation early, and it
-- allows for filtering (ex: gzip compression) to be applied
fileServeStrict :: (ServerMonad m, FilterMonad Response m, MonadIO m) =>
                   [FilePath]   -- ^ index files if the path is a directory
                -> FilePath     -- ^ file/directory to serve
                -> m Response
fileServeStrict ixFiles localpath = do
    resp <- fileServe' localpath
                       (doIndex (ixFiles++defaultIxFiles))
                       mimeTypes
                       getFileStrict

    -- clear "Content-Length" because it could be modified by filters
    -- downstream
    let headers = rsHeaders resp
    return $ resp {rsHeaders = Map.delete (P.pack "content-length") headers}



-- | Serve files with a mime type map under a directory.
--   Uses the function to transform URIs to FilePaths.
fileServe' :: (ServerMonad m, FilterMonad Response m, MonadIO m) =>
              String
              -> (Map.Map String String -> String -> m Response)
              -> Map.Map String String
              -> GetFileFunc
              -> m Response
fileServe' localpath fdir mime getFileFunc = do
    rq <- askRq
    let fp2 = takeWhile (/=',') fp
        fp = filepath
        safepath = filter (\x->not (null x) && head x /= '.') (rqPaths rq)
        filepath = intercalate "/"  (localpath:safepath)
        fp' = if null safepath then "" else last safepath
    if "TESTH" `isPrefixOf` fp'
        then renderResponse mime $ fakeFile $ (read $ drop 5 $ fp' :: Integer)
        else do
    fe <- liftIO $ doesFileExist fp
    fe2 <- liftIO $ doesFileExist fp2
    de <- liftIO $ doesDirectoryExist fp
    -- error $ "show ilepath: " ++show (fp,de)
    let status | de   = "DIR"
               | fe   = "file"
               | fe2  = "group"
               | True = "NOT FOUND"
    liftIO $ logM "Happstack.Server.HTTP.FileServe" DEBUG ("fileServe: "++show fp++" \t"++status)
    if de then fdir mime fp else do
    getFileFunc mime fp >>= flip either (renderResponse mime)
        (const $ returnGroup localpath mime safepath)


returnFile :: (ServerMonad m, FilterMonad Response m, MonadIO m) =>
              GetFileFunc -> Map.Map String String -> String -> m Response
returnFile getFileFunc mime fp =
    getFileFunc mime fp >>=  either fileNotFound (renderResponse mime)



-- if fp has , separated then return concatenation with content-type of last
-- and last modified of latest
tr :: (Eq a) => a -> a -> [a] -> [a]
tr a b = map (\x->if x==a then b else x)
ltrim :: String -> String
ltrim = dropWhile (flip elem " \t\r")

returnGroup :: (ServerMonad m, FilterMonad Response m, MonadIO m) =>
               String -> Map.Map String String -> [String] -> m Response
returnGroup localPath mime fp = do
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

  renderResponse mime ((maxTime,totSize),(fst $ snd $ head files,
                                             L.concat $ map (snd . snd) files))



fileNotFound :: (Monad m, FilterMonad Response m) => String -> m Response
fileNotFound fp = do setResponseCode 404
                     return $ toResponse $ "File not found "++ fp
--fakeLen = 71* 1024
fakeFile :: (Integral a) =>
            a -> ((ClockTime, Int64), (String, L.ByteString))
fakeFile fakeLen = ((TOD 0 0,L.length body),("text/javascript",body))
    where
      body = L.pack $ (("//"++(show len)++" ") ++ ) $ (replicate len '0') ++ "\n"
      len = fromIntegral fakeLen

-- | @getFile mimeMap path@ will lazily read the file as a ByteString
-- with a content type provided by matching the file extension with the
-- @mimeMap@.  getFile will return an error string or ((timeFetched,size), (contentType,fileContents))
getFile :: (MonadIO m) =>
           Map.Map String String
           -> String
           -> m (Either String ((ClockTime, Integer), (String, L.ByteString)))
getFile mime fp = do
  let ct = Map.findWithDefault "text/plain" (getExt fp) mime
  fe <- liftIO $ doesFileExist fp
  if not fe then return $ Left fp else do

  time <- liftIO $ getModificationTime fp
  h    <- liftIO $ openBinaryFile fp ReadMode
  size <- liftIO $ hFileSize h
  lbs  <- liftIO $ L.hGetContents h
  return $ Right ((time,size),(ct,lbs))

-- | As 'getFile' but strictly fetches the file, instead of lazily.
getFileStrict :: (MonadIO m) =>
                 Map.Map String String
              -> String
              -> m (Either String ((ClockTime, Integer), (String, L.ByteString)))
getFileStrict mime fp = do
  let ct = Map.findWithDefault "text/plain" (getExt fp) mime
  fe     <- liftIO $ doesFileExist fp

  if not fe then return $ Left fp else do

  time     <- liftIO $ getModificationTime fp
  s        <- liftIO $ P.readFile fp
  let lbs  = L.fromChunks [s]
  let size = toInteger . P.length $ s
  return $ Right ((time,size),(ct,lbs))


renderResponse :: (Monad m,
                   ServerMonad m,
                   FilterMonad Response m,
                   Show t1) =>
                  t
                  -> ((ClockTime, t1), (String, L.ByteString))
                  -> m Response
renderResponse _ ((modtime,size),(ct,body)) = do
  rq <- askRq
  let notmodified = getHeader "if-modified-since" rq == Just (P.pack $ repr)
      repr = formatCalendarTime defaultTimeLocale
             "%a, %d %b %Y %X GMT" (toUTCTime modtime)
  -- "Mon, 07 Jan 2008 19:51:02 GMT"
  -- when (isJust $ getHeader "if-modified-since"  rq) $ error $ show $ getHeader "if-modified-since" rq
  if notmodified then do setResponseCode 304 ; return $ toResponse "" else do

  return $ ((setHeader "Last-modified" repr) .
            (setHeader "Content-Length" (show size)) .
            (setHeader "Content-Type" ct)) $
           resultBS 200 body




getExt :: String -> String
getExt = reverse . takeWhile (/='.') . reverse

-- | Ready collection of common mime types.
mimeTypes :: MimeMap
mimeTypes = Map.fromList
	    [("xml","application/xml")
	    ,("xsl","application/xml")
	    ,("js","text/javascript")
	    ,("html","text/html")
	    ,("htm","text/html")
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


-- | Prevents files of the form '.foo' or 'bar/.foo' from being served
blockDotFiles :: (Request -> IO Response) -> Request -> IO Response
blockDotFiles fn rq
    | isDot (intercalate "/" (rqPaths rq)) = return $ result 403 "Dot files not allowed."
    | otherwise = fn rq

-- | Returns True if the given String either starts with a . or is of the form
-- "foo/.bar", e.g. the typical *nix convention for hidden files.
isDot :: String -> Bool
isDot = isD . reverse
    where
    isD ('.':'/':_) = True
    isD ['.']       = True
    --isD ('/':_)     = False
    isD (_:cs)      = isD cs
    isD []          = False
