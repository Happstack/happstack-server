{-# LANGUAGE FlexibleContexts, Rank2Types #-}
-- |File Serving functions
module Happstack.Server.HTTP.FileServe
    (
     -- * Content-Type \/ Mime-Type
     MimeMap,
     mimeTypes,
     asContentType,
     guessContentType,
     guessContentTypeM,
     -- * Low-Level
     sendFileResponse,     
     lazyByteStringResponse,
     strictByteStringResponse,
     filePathSendFile,
     filePathLazy,
     filePathStrict,
     -- * High-Level
     -- ** Serving a single file
     serveFile,
     serveFileUsing,
     -- ** Serving files from a directory
     fileServe',
     fileServe,
     fileServeLazy,
     fileServeStrict,
     -- * Other
     blockDotFiles,
     defaultIxFiles,
     doIndex,
     doIndex',
     doIndexLazy,
     doIndexStrict,
     errorwrapper,
     isDot
    ) where

import Control.Exception.Extensible (IOException, SomeException, Exception(fromException), bracket, handleJust)
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.Maybe (fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as Map
import Happstack.Server.SimpleHTTP (FilterMonad, ServerMonad(askRq), Request(..), Response(..), WebMonad, toResponse, resultBS, setHeader, forbidden, getHeader, nullRsFlags, result, require, rsfContentLength, ifModifiedSince )
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime)
import System.IO (Handle, IOMode(ReadMode), hFileSize, hClose, openBinaryFile)
import System.FilePath ((</>), joinPath, takeExtension)
import System.Log.Logger (Priority(DEBUG), logM)
import System.Time (CalendarTime, toUTCTime)

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

-- * Mime-Type / Content-Type

type MimeMap = Map String String

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


guessContentType :: MimeMap -> FilePath -> Maybe String
guessContentType mimeMap filepath =
    case getExt filepath of
      "" -> Nothing
      ext -> Map.lookup ext mimeMap

guessContentTypeM :: (Monad m) => MimeMap -> (FilePath -> m String)
guessContentTypeM mimeMap filePath = return $ fromMaybe "text/plain" $ guessContentType mimeMap filePath

asContentType :: (Monad m) => String -> (FilePath -> m String)
asContentType = const . return

defaultIxFiles :: [String]
defaultIxFiles= ["index.html","index.xml","index.gif"]

fileNotFound :: (Monad m, FilterMonad Response m) => FilePath -> m Response
fileNotFound fp = return $ result 404 $ "File not found " ++ fp

-- | Similar to 'takeExtension' but does not include the extension separator char
getExt :: FilePath -> String
getExt fp = drop 1 $ takeExtension fp

-- | Prevents files of the form '.foo' or 'bar/.foo' from being served
blockDotFiles :: (Request -> IO Response) -> Request -> IO Response
blockDotFiles fn rq
    | isDot (joinPath (rqPaths rq)) = return $ result 403 "Dot files not allowed."
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

-- * Low-level functions for generating a Response

-- | Use sendFile to send the contents of a Handle
sendFileResponse :: String  -- ^ content-type string
                 -> Handle  -- ^ file handle for content to send
                 -> Maybe (CalendarTime, Request) -- ^ mod-time for the handle (MUST NOT be later than server's time of message origination), incoming request (used to check for if-modified-since header)
                 -> Integer -- ^ offset into Handle
                 -> Integer -- ^ number of bytes to send
                 -> Response
sendFileResponse ct handle mModTime offset count =
    let res = ((setHeader "Content-Length" (show count)) .
               (setHeader "Content-Type" ct) $ 
               (SendFile 200 Map.empty nullRsFlags{rsfContentLength=False} Nothing handle 0 count)
              )
    in case mModTime of
         Nothing -> res
         (Just (modTime, request)) -> ifModifiedSince modTime request res

-- | Send the contents of a Lazy ByteString
lazyByteStringResponse :: String   -- ^ content-type string (e.g. @\"text/plain; charset=utf-8\"@)
                       -> L.ByteString   -- ^ lazy bytestring content to send
                       -> Maybe (CalendarTime, Request) -- ^ mod-time for the bytestring, incoming request (used to check for if-modified-since header)
                       -> Integer -- ^ offset into the bytestring
                       -> Integer -- ^ number of bytes to send (offset + count must be less than or equal to the length of the bytestring)
                       -> Response
lazyByteStringResponse ct body mModTime offset count =
    let res = ((setHeader "Content-Type" ct) $
               resultBS 200 (L.take (fromInteger count) $ (L.drop (fromInteger offset))  body)
              )
    in case mModTime of
         Nothing -> res
         (Just (modTime, request)) -> ifModifiedSince modTime request res

-- | Send the contents of a Lazy ByteString
strictByteStringResponse :: String   -- ^ content-type string (e.g. @\"text/plain; charset=utf-8\"@)
                         -> S.ByteString   -- ^ lazy bytestring content to send
                         -> Maybe (CalendarTime, Request) -- ^ mod-time for the bytestring, incoming request (used to check for if-modified-since header)
                         -> Integer -- ^ offset into the bytestring
                         -> Integer -- ^ number of bytes to send (offset + count must be less than or equal to the length of the bytestring)
                         -> Response
strictByteStringResponse ct body mModTime offset count =
    let res = ((setHeader "Content-Type" ct) $
               resultBS 200 (L.fromChunks [S.take (fromInteger count) $ S.drop (fromInteger offset) body])
              )
    in case mModTime of
         Nothing -> res
         (Just (modTime, request)) -> ifModifiedSince modTime request res

-- | Send the specified file with the specified mime-type using sendFile()
--
-- NOTE: assumes file exists and is readable by the server. See 'serveFileUsing'.
--
-- WARNING: No security checks are performed.
filePathSendFile :: (ServerMonad m, MonadIO m)
                 => String   -- ^ content-type string
                 -> FilePath -- ^ path to file on disk
                 -> m Response
filePathSendFile contentType fp =
    do handle  <- liftIO $ openBinaryFile fp ReadMode -- garbage collection should close this
       modtime <- liftIO $ getModificationTime fp
       count   <- liftIO $ hFileSize handle
       rq      <- askRq
       return $ sendFileResponse contentType handle (Just (toUTCTime modtime, rq)) 0 count

-- | Send the specified file with the specified mime-type using Lazy ByteStrings
--
-- NOTE: assumes file exists and is readable by the server. See 'serveFileUsing'.
--
-- WARNING: No security checks are performed.
filePathLazy :: (ServerMonad m, MonadIO m)
                 => String   -- ^ content-type string
                 -> FilePath -- ^ path to file on disk
                 -> m Response
filePathLazy contentType fp =
    do handle  <- liftIO $ openBinaryFile fp ReadMode -- garbage collection should close this
       contents <- liftIO $ L.hGetContents handle
       modtime  <- liftIO $ getModificationTime fp
       count    <- liftIO $ hFileSize handle
       rq       <- askRq
       return $ lazyByteStringResponse contentType contents (Just (toUTCTime modtime, rq)) 0 count

-- | Send the specified file with the specified mime-type using Lazy ByteStrings
--
-- NOTE: assumes file exists and is readable by the server. See 'serveFileUsing'.
--
-- WARNING: No security checks are performed.
filePathStrict :: (ServerMonad m, MonadIO m)
                 => String   -- ^ content-type string
                 -> FilePath -- ^ path to file on disk
                 -> m Response
filePathStrict contentType fp =
    do contents <- liftIO $ S.readFile fp
       modtime  <- liftIO $ getModificationTime fp
       count    <- liftIO $ bracket (openBinaryFile fp ReadMode) hClose hFileSize
       rq       <- askRq
       return $ strictByteStringResponse contentType contents (Just (toUTCTime modtime, rq)) 0 count

-- * High-level functions for serving files


-- ** Serve a single file

-- | Serve a single, specified file.
-- 
-- example 1:
-- 
--  Serve using sendfile() and the specified content-type
--
-- > serveFileUsing filePathSendFile (asContentType "image/jpeg") "/srv/data/image.jpg"
--
--
-- example 2:
-- 
--  Serve using a lazy ByteString and the guess the content-type from the extension
-- 
-- > serveFileUsing filePathLazy (guessContentTypeM mimeTypes) "/srv/data/image.jpg"
-- 
-- WARNING: No security checks are performed.
serveFileUsing :: (ServerMonad m, FilterMonad Response m, MonadIO m) 
               => (String -> FilePath -> m Response) -- ^ typically 'filePathSendFile', 'filePathLazy', or 'filePathStrict'
               -> (FilePath -> m String)  -- ^ function for determining content-type of file. Typically 'asContentType' or 'guessContentTypeM'
               -> FilePath -- ^ path to the file to serve
               -> m Response
serveFileUsing serveFn mimeFn fp = 
    do fe <- liftIO $ doesFileExist fp
       if fe
          then do mt <- mimeFn fp
                  serveFn mt fp
          else fileNotFound fp

-- | Alias for 'serveFileUsing' 'filePathSendFile'
serveFile :: (ServerMonad m, FilterMonad Response m, MonadIO m) => (FilePath -> m String) -> FilePath -> m Response
serveFile = serveFileUsing filePathSendFile

-- ** Serve files from a directory

-- | Serve files from a directory and it's subdirectories (parameterizable version)
-- 
-- Parameterize this function to create functions like, 'fileServe', 'fileServeLazy', and 'fileServeStrict'
--
-- You supply:
--
--  1. a low-level function which takes a content-type and 'FilePath' and generates a Response
--  2. a function which determines the content-type from the 'FilePath'
--  3. a list of all the default index files
--
-- NOTE: unlike fileServe, there are no index files by default. See 'defaultIxFiles'.
fileServe' :: ( WebMonad Response m
              , ServerMonad m
              , FilterMonad Response m
              , MonadIO m
              ) 
           => (String -> FilePath -> m Response) -- ^ function which takes a content-type and filepath and generates a response (typically 'filePathSendFile', 'filePathLazy', or 'filePathStrict')
           -> (FilePath -> m String) -- ^ function which returns the mime-type for FilePath
           -> [FilePath]         -- ^ index files if the path is a directory
           -> FilePath           -- ^ file/directory to serve
           -> m Response
fileServe' serveFn mimeFn ixFiles localpath = do
    rq <- askRq
    let safepath = filter (\x->not (null x) && head x /= '.') (rqPaths rq)
        fp = joinPath  (localpath:safepath)
    fe <- liftIO $ doesFileExist fp
    de <- liftIO $ doesDirectoryExist fp
    let status | de   = "DIR"
               | fe   = "file"
               | True = "NOT FOUND"
    liftIO $ logM "Happstack.Server.HTTP.FileServe" DEBUG ("fileServe: "++show fp++" \t"++status)
    if de
        then doIndex' serveFn mimeFn (ixFiles++defaultIxFiles) fp
        else if fe 
                then serveFileUsing serveFn mimeFn fp
                else fileNotFound fp


-- | Serve files from a directory and it's subdirectories (sendFile version). Should perform much better than its predecessors.
fileServe :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m) =>
             [FilePath]         -- ^ index files if the path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServe ixFiles localPath = fileServe' filePathSendFile (guessContentTypeM mimeTypes) (ixFiles ++ defaultIxFiles) localPath

-- | Serve files from a directory and it's subdirectories (lazy ByteString version).
-- 
-- May leak file handles.
fileServeLazy :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m) =>
             [FilePath]         -- ^ index files if the path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServeLazy ixFiles localPath = fileServe' filePathLazy (guessContentTypeM mimeTypes) (ixFiles ++ defaultIxFiles) localPath

-- | Serve files from a directory and it's subdirectories (strict ByteString version). 
fileServeStrict :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m) =>
             [FilePath]         -- ^ index files if the path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServeStrict ixFiles localPath = fileServe' filePathStrict (guessContentTypeM mimeTypes) (ixFiles ++ defaultIxFiles) localPath

-- * Index

doIndex :: (ServerMonad m, FilterMonad Response m, MonadIO m)
        => [String]
        -> MimeMap
        -> String
        -> m Response
doIndex ixFiles mimeMap localPath = doIndex' filePathSendFile (guessContentTypeM mimeMap) ixFiles localPath

doIndexLazy :: (ServerMonad m, FilterMonad Response m, MonadIO m)
        => [String]
        -> MimeMap
        -> String
        -> m Response
doIndexLazy ixFiles mimeMap localPath = doIndex' filePathLazy (guessContentTypeM mimeMap) ixFiles localPath

doIndexStrict :: (ServerMonad m, FilterMonad Response m, MonadIO m)
        => [String]
        -> MimeMap
        -> String
        -> m Response
doIndexStrict ixFiles mimeMap localPath = doIndex' filePathStrict (guessContentTypeM mimeMap) ixFiles localPath

doIndex' :: (ServerMonad m, FilterMonad Response m, MonadIO m)
        => (String -> FilePath -> m Response)
        -> (FilePath -> m String)
        -> [String]
        -> String
        -> m Response
doIndex' _serveFn _mime  []          _fp = forbidden $ toResponse "Directory index forbidden"
doIndex'  serveFn mimeFn (index:rest) fp =
    do let path = fp </> index
       fe <- liftIO $ doesFileExist path
       if fe 
          then serveFileUsing serveFn mimeFn path 
          else doIndex' serveFn mimeFn rest fp
