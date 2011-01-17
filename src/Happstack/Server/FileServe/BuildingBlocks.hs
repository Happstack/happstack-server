{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, ScopedTypeVariables, Rank2Types #-}
-- | Build your own file serving functions
--
-- If the functions in "Happstack.Server.FileServe" do not quite do
-- you want you can roll your own by reusing pieces from this module.
--
-- You will likely want to start by copying the source for a function
-- like, 'serveDirectory' and then modifying it to suit your needs.
--
module Happstack.Server.FileServe.BuildingBlocks
    (
     -- * High-Level
     -- ** Serving files from a directory
     fileServe,
     fileServe',
     fileServeLazy,
     fileServeStrict,
     Browsing(..),
     serveDirectory,
     -- ** Serving a single file
     serveFile,
     serveFileUsing,
     -- * Low-Level
     sendFileResponse,     
     lazyByteStringResponse,
     strictByteStringResponse,
     filePathSendFile,
     filePathLazy,
     filePathStrict,
     -- * Content-Type \/ Mime-Type
     MimeMap,
     mimeTypes,
     asContentType,
     guessContentType,
     guessContentTypeM,
     -- * Other
     blockDotFiles,
     defaultIxFiles,
     tryIndex,
     doIndex,
     doIndex',
     doIndexLazy,
     doIndexStrict,
     fileNotFound,
     isDot
    ) where

import Control.Exception.Extensible (IOException, SomeException, Exception(fromException), bracket, handleJust)
import Control.Monad (MonadPlus(mzero), msum)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.Data  (Data, Typeable)
import Data.Maybe (fromMaybe, maybe)
import           Data.Map (Map)
import qualified Data.Map as Map
import Happstack.Server.Monads     (Happstack, ServerMonad(askRq), FilterMonad, WebMonad, require)
import Happstack.Server.Response   (ToMessage(toResponse), ifModifiedSince, forbidden, ok, seeOther)
import Happstack.Server.Types      (Length(ContentLength), Request(rqPaths, rqUri), Response(SendFile), RsFlags(rsfLength), nullRsFlags, result, resultBS, setHeader)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, getModificationTime)
import System.FilePath ((</>), addTrailingPathSeparator, joinPath, takeExtension)
import System.IO (IOMode(ReadMode), hFileSize, hClose, openBinaryFile)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import System.Log.Logger (Priority(DEBUG), logM)
import System.Time (CalendarTime, formatCalendarTime, toCalendarTime, toUTCTime)

import           Text.Blaze                  ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- FIXME: why are these functions here ?

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

-- |a 'Map' from file extensions to content-types
-- 
-- example:
--
-- > myMimeMap :: MimeMap
-- > myMimeMap = Map.fromList [("gz","application/x-gzip"), ... ]
--
-- see also: 'mimeTypes'
type MimeMap = Map String String

-- | try to guess the content-type of a file based on its extension
--
-- see also: 'guessContentTypeM'
guessContentType :: MimeMap -> FilePath -> Maybe String
guessContentType mimeMap filepath =
    case getExt filepath of
      "" -> Nothing
      ext -> Map.lookup ext mimeMap

-- | try to guess the content-type of a file based on its extension
--
-- defaults to "application/octet-stream" if no match was found.
--
-- Useful as an argument to 'serveFile'
--
-- see also: 'guessContentType', 'serveFile'
guessContentTypeM :: (Monad m) => MimeMap -> (FilePath -> m String)
guessContentTypeM mimeMap filePath = return $ fromMaybe "application/octet-stream" $ guessContentType mimeMap filePath

-- | returns a specific content type, completely ignoring the 'FilePath' argument. 
--
-- Use this with 'serveFile' if you want to explicitly specify the
-- content-type.
--
-- see also: 'guessContentTypeM', 'serveFile'
asContentType :: (Monad m) => 
                 String  -- ^ the content-type to return
              -> (FilePath -> m String)
asContentType = const . return

-- | a list of common index files. Specifically: @index.html@, @index.xml@, @index.gif@
-- 
-- Typically used as an argument to 'serveDiretory'.
defaultIxFiles :: [FilePath]
defaultIxFiles= ["index.html","index.xml","index.gif"]

-- | return a simple "File not found 404 page."
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
                 -> FilePath  -- ^ file path for content to send
                 -> Maybe (CalendarTime, Request) -- ^ mod-time for the handle (MUST NOT be later than server's time of message origination), incoming request (used to check for if-modified-since header)
                 -> Integer -- ^ offset into Handle
                 -> Integer -- ^ number of bytes to send
                 -> Response
sendFileResponse ct filePath mModTime _offset count =
    let res = ((setHeader "Content-Type" ct) $ 
               (SendFile 200 Map.empty (nullRsFlags { rsfLength = ContentLength }) Nothing filePath 0 count)
              )
    in case mModTime of
         Nothing -> res
         (Just (modTime, request)) -> ifModifiedSince modTime request res

-- | Send the contents of a Lazy ByteString
--
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
       return $ sendFileResponse contentType fp (Just (toUTCTime modtime, rq)) 0 count

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

-- | Serve a single, specified file. The name of the file being served is specified explicity. It is not derived automatically from the 'Request' url.
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
serveFileUsing :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) 
               => (String -> FilePath -> m Response) -- ^ typically 'filePathSendFile', 'filePathLazy', or 'filePathStrict'
               -> (FilePath -> m String)  -- ^ function for determining content-type of file. Typically 'asContentType' or 'guessContentTypeM'
               -> FilePath -- ^ path to the file to serve
               -> m Response
serveFileUsing serveFn mimeFn fp = 
    do fe <- liftIO $ doesFileExist fp
       if fe
          then do mt <- mimeFn fp
                  serveFn mt fp
          else mzero

-- | Serve a single, specified file. The name of the file being served is specified explicity. It is not derived automatically from the 'Request' url.
-- 
-- example 1:
-- 
--  Serve as a specific content-type:
--
-- > serveFile (asContentType "image/jpeg") "/srv/data/image.jpg"
--
--
-- example 2:
-- 
--  Serve guessing the content-type from the extension:
-- 
-- > serveFile (guessContentTypeM mimeTypes) "/srv/data/image.jpg"
--
-- If the specified path does not exist or is not a file, this function will return 'mzero'.
-- 
-- WARNING: No security checks are performed.
--
-- NOTE: alias for 'serveFileUsing' 'filePathSendFile'
serveFile :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) => 
             (FilePath -> m String)   -- ^ function for determining content-type of file. Typically 'asContentType' or 'guessContentTypeM'
          -> FilePath                 -- ^ path to the file to serve
          -> m Response
serveFile = serveFileUsing filePathSendFile

-- ** Serve files from a directory

-- | Serve files from a directory and its subdirectories (parameterizable version)
-- 
-- Parameterize this function to create functions like, 'fileServe', 'fileServeLazy', and 'fileServeStrict'
--
-- You supply:
--
--  1. a low-level function which takes a content-type and 'FilePath' and generates a Response
--
--  2. a function which determines the content-type from the 'FilePath'
--
--  3. a list of all the default index files
--
-- NOTE: unlike fileServe, there are no index files by default. See 'defaultIxFiles'.
fileServe' :: ( WebMonad Response m
              , ServerMonad m
              , FilterMonad Response m
              , MonadIO m
              , MonadPlus m
              ) 
           => (String -> FilePath -> m Response) -- ^ function which takes a content-type and filepath and generates a response (typically 'filePathSendFile', 'filePathLazy', or 'filePathStrict')
           -> (FilePath -> m String) -- ^ function which returns the mime-type for FilePath
--           -> [FilePath]         -- ^ index file names, in case the requested path is a directory
           -> (FilePath -> m Response)
           -> FilePath           -- ^ file/directory to serve
           -> m Response
fileServe' serveFn mimeFn indexFn localpath = do
    rq <- askRq
    let safepath = filter (\x->not (null x) && x /= ".." && x /= ".") (rqPaths rq)
        fp = joinPath  (localpath:safepath)
    fe <- liftIO $ doesFileExist fp
    de <- liftIO $ doesDirectoryExist fp
    let status | de   = "DIR"
               | fe   = "file"
               | True = "NOT FOUND"
    liftIO $ logM "Happstack.Server.FileServe" DEBUG ("fileServe: "++show fp++" \t"++status)
    if de
        then if last (rqUri rq) == '/'
--             then indexFn serveFn mimeFn (ixFiles++defaultIxFiles) fp
             then indexFn fp
             else do let path' = addTrailingPathSeparator (rqUri rq)
                     seeOther path' (toResponse path')
        else if fe 
                then serveFileUsing serveFn mimeFn fp
                else mzero

-- | Serve files from a directory and its subdirectories using 'sendFile'.
-- 
-- Usage:
--
-- > fileServe ["index.html"] "path/to/files/on/disk"
--
--  'fileServe' does not support directory browsing. See 'serveDirectory'
--
-- DEPRECATED: use 'serveDirectory' instead.
--
-- Note:
-- 
--  The list of index files @[\"index.html\"]@ is only used to determine what file to show if the user requests a directory. You *do not* need to explicitly list all the files you want to serve.
-- 
fileServe :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
             [FilePath]         -- ^ index file names, in case the requested path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServe ixFiles localPath = 
    fileServe' serveFn mimeFn indexFn localPath
        where
          serveFn    = filePathSendFile
          mimeFn     = guessContentTypeM mimeTypes
          indexFiles = (ixFiles ++ defaultIxFiles) 
          indexFn    = doIndex' filePathSendFile mimeFn indexFiles
--          indexFn    = browseIndex filePathSendFile mimeFn indexFiles
{-# DEPRECATED fileServe "use serveDirectory instead." #-}

-- | Serve files from a directory and its subdirectories (lazy ByteString version).
--
-- WARNING: May leak file handles. You should probably use 'fileServe' instead.
fileServeLazy :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
             [FilePath]         -- ^ index file names, in case the requested path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServeLazy ixFiles localPath =
    fileServe' serveFn mimeFn indexFn localPath
        where
          serveFn    = filePathLazy
          mimeFn     = guessContentTypeM mimeTypes
          indexFiles = (ixFiles ++ defaultIxFiles) 
          indexFn    = doIndex' filePathSendFile mimeFn indexFiles

-- | Serve files from a directory and its subdirectories (strict ByteString version). 
--
-- WARNING: the entire file will be read into RAM before being served. You should probably use 'fileServe' instead.
fileServeStrict :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
             [FilePath]         -- ^ index file names, in case the next argument is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServeStrict ixFiles localPath =
    fileServe' serveFn mimeFn indexFn localPath
        where
          serveFn    = filePathStrict
          mimeFn     = guessContentTypeM mimeTypes
          indexFiles = (ixFiles ++ defaultIxFiles) 
          indexFn    = doIndex' filePathSendFile mimeFn indexFiles

-- * Index

doIndex :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m)
        => [String]
        -> MimeMap
        -> FilePath
        -> m Response
doIndex ixFiles mimeMap localPath = doIndex' filePathSendFile (guessContentTypeM mimeMap) ixFiles localPath

doIndexLazy :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m)
        => [String]
        -> MimeMap
        -> FilePath
        -> m Response
doIndexLazy ixFiles mimeMap localPath = doIndex' filePathLazy (guessContentTypeM mimeMap) ixFiles localPath

doIndexStrict :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m)
        => [String]
        -> MimeMap
        -> FilePath
        -> m Response
doIndexStrict ixFiles mimeMap localPath = doIndex' filePathStrict (guessContentTypeM mimeMap) ixFiles localPath

doIndex' :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m)
        => (String -> FilePath -> m Response)
        -> (FilePath -> m String)
        -> [String]
        -> FilePath
        -> m Response
doIndex' serveFn mimeFn ixFiles fp =
    msum [ tryIndex serveFn mimeFn ixFiles fp
         , forbidden $ toResponse "Directory index forbidden"
         ]

-- | try to find an index file, calls mzero on failure
tryIndex :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m)
        => (String -> FilePath -> m Response) -- ^ usually 'filePathSendFile'
        -> (FilePath -> m String)             -- ^ function to calculate mime type, usually 'guessContentTypeM'
        -> [String]                           -- ^ list of index files. See also 'defaultIxFiles'
        -> FilePath                           -- ^ directory to search in
        -> m Response
tryIndex _serveFn _mime  []          _fp = mzero
tryIndex  serveFn mimeFn (index:rest) fp =
    do let path = fp </> index
       fe <- liftIO $ doesFileExist path
       if fe 
          then serveFileUsing serveFn mimeFn path 
          else tryIndex serveFn mimeFn rest fp

-- * Directory Browsing

browseIndex :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m, ToMessage b) =>
                (FilePath -> [FilePath] -> m b)
             -> (String -> FilePath -> m Response)
             -> (FilePath -> m String)
             -> [String]
             -> FilePath
             -> m Response
browseIndex renderFn serveFn mimeFn ixFiles localPath =
    do c       <- liftIO $ getDirectoryContents localPath
       listing <- renderFn localPath $ filter (/= ".") c
       ok $ toResponse $ listing

data EntryKind = File | Directory | UnknownKind deriving (Eq, Ord, Read, Show, Data, Typeable, Enum)

-- | a function to generate an HTML page showing the contents of a directory on the disk
--
-- see also: 'browseIndex', 'renderDirectoryContentsTable'
renderDirectoryContents :: (MonadIO m) =>
                           FilePath    -- ^ path to directory on disk
                        -> [FilePath]  -- ^ list of entries in that path
                        -> m H.Html 
renderDirectoryContents localPath fps =
    do fps' <- liftIO $ mapM (getMetaData localPath) fps
       return $ H.html $ do 
         H.head $ do
           H.title $ H.string "Directory Listing"
           H.meta  ! A.http_equiv (H.stringValue "Content-Type") ! A.content (H.stringValue "text/html;charset=utf-8")
           H.style $ H.string $ unlines [ "table { border-collapse: collapse; font-family: 'sans-serif'; }"
                                        , "table, th, td { border: 1px solid #98BF21; }" 
                                        , "td.size { text-align: right; }"
                                        , "td.date { text-align: right; }"
                                        , "td { padding-right: 1em; padding-left: 1em; }"
                                        , "tr { background-color: white; }"
                                        , "tr.alt { background-color: #EAF2D3 }"
                                        , "th { background-color: #A7C942; color: white; font-size: 1.125em; }"
                                        ]
         H.body $ do
           H.h1 $ H.string "Directory Listing"
           renderDirectoryContentsTable fps'

-- | a function to generate an HTML table showing the contents of a directory on the disk
--
-- This function generates most of the content of the
-- 'renderDirectoryContents' page. If you want to style the page
-- differently, or add google analytics code, etc, you can just create
-- a new page template to wrap around this HTML.
--
-- see also: 'getMetaData', 'renderDirectoryContents'
renderDirectoryContentsTable :: [(FilePath, Maybe CalendarTime, Maybe Integer, EntryKind)] -- ^ list of files+meta data, see 'getMetaData'
                             -> H.Html
renderDirectoryContentsTable fps =
           H.table $ do H.thead $ do H.th $ H.string ""
                                     H.th $ H.string "Name"
                                     H.th $ H.string "Last modified"
                                     H.th $ H.string "Size"
                        H.tbody $ mapM_ mkRow (zip fps $ cycle [False, True])
    where
      mkRow :: ((FilePath, Maybe CalendarTime, Maybe Integer, EntryKind), Bool) -> H.Html
      mkRow ((fp, modTime, count, kind), alt) = 
          (if alt then (! A.class_ (H.stringValue "alt")) else id) $
          H.tr $ do
                   H.td (mkKind kind)
                   H.td (H.a ! A.href (H.stringValue fp)  $ H.string fp)
                   H.td ! A.class_ (H.stringValue "date") $ (H.string $ maybe "-" (formatCalendarTime defaultTimeLocale "%d-%b-%Y %X %Z") modTime)
                   H.td ! A.class_ (H.stringValue "size") $ (H.string $ maybe "-" show count)
      mkKind :: EntryKind -> H.Html
      mkKind File        = return ()
      mkKind Directory   = H.string "➦"
      mkKind UnknownKind = return ()

-- | look up the meta data associated with a file
getMetaData :: FilePath -- ^ path to directory on disk containing the entry
            -> FilePath -- ^ entry in that directory
            -> IO (FilePath, Maybe CalendarTime, Maybe Integer, EntryKind)
getMetaData localPath fp =
     do let localFp = localPath </> fp
        modTime <- (fmap Just . toCalendarTime =<< getModificationTime localFp) `catch` 
                   (const $ return Nothing)
        count <- do de <- doesDirectoryExist localFp
                    if de
                      then do return Nothing
                      else do bracket (openBinaryFile localFp ReadMode) hClose (fmap Just . hFileSize) 
                                          `catch` (\(e :: IOException) -> return Nothing)
        kind <- do fe <- doesFileExist localFp
                   if fe
                      then return File
                      else do de <- doesDirectoryExist localFp
                              if de 
                                 then return Directory
                                 else return UnknownKind
        return (fp, modTime, count, kind)

-- | see 'serveDirectory'
data Browsing 
    = EnableBrowsing | DisableBrowsing 
      deriving (Eq, Enum, Ord, Read, Show, Data, Typeable)

-- | Serve files and directories from a directory and its subdirectories using 'sendFile'.
-- 
-- Usage:
--
-- > serveDirectory EnableBrowsing ["index.html"] "path/to/files/on/disk"
--
-- If the requested path does not match a file or directory on the
-- disk, then 'serveDirectory' calls 'mzero'.
--
-- If the requested path is a file then the file is served normally. 
--
-- If the requested path is a directory, then the result depends on
-- what the first two arguments to the function are.
--
-- The first argument controls whether directory browsing is
-- enabled.
--
-- The second argument is a list of index files (such as
-- index.html).
--
-- When a directory is requested, 'serveDirectory' will first try to
-- find one of the index files (in the order they are listed). If that
-- fails, it will show a directory listing if 'EnableBrowsing' is set,
-- otherwise it will return @forbidden \"Directory index forbidden\"@.
-- 
-- Here is an explicit list of all the possible outcomes when the
-- argument is a (valid) directory:
--
-- [@'DisableBrowsing', empty index file list@]
--
--  This will always return, forbidden \"Directory index forbidden\"
--
-- [@'DisableBrowsing', non-empty index file list@]
--
-- 1. If an index file is found it will be shown.
--
-- 2. Otherwise returns, forbidden \"Directory index forbidden\"
--
-- [@'EnableBrowsing', empty index file list@] 
--
-- Always shows a directory index.
--
-- [@'EnableBrowsing', non-empty index file list@]
--
-- 1. If an index file is found it will be shown
--
-- 2. Otherwise shows a directory index
--
-- see also: 'defaultIxFiles', 'serveFile'
serveDirectory :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
                  Browsing    -- ^ allow directory browsing
               -> [FilePath]  -- ^ index file names, in case the requested path is a directory
               -> FilePath    -- ^ file/directory to serve
               -> m Response
serveDirectory browsing ixFiles localPath = 
    fileServe' serveFn mimeFn indexFn localPath
        where
          serveFn = filePathSendFile
          mimeFn  = guessContentTypeM mimeTypes
          indexFn fp =
              msum [ tryIndex filePathSendFile mimeFn ixFiles fp
                   , if browsing == EnableBrowsing
                        then browseIndex renderDirectoryContents filePathSendFile mimeFn ixFiles fp
                        else forbidden $ toResponse "Directory index forbidden"
                   ]

-- | Ready collection of common mime types.
-- Except for the first two entries, the mappings come from an Ubuntu 8.04 \/etc\/mime.types file.
mimeTypes :: MimeMap
mimeTypes = Map.fromList [("gz","application/x-gzip"),("cabal","application/x-cabal"),("%","application/x-trash"),("323","text/h323"),("3gp","video/3gpp"),("7z","application/x-7z-compressed"),("abw","application/x-abiword"),("ai","application/postscript"),("aif","audio/x-aiff"),("aifc","audio/x-aiff"),("aiff","audio/x-aiff"),("alc","chemical/x-alchemy"),("art","image/x-jg"),("asc","text/plain"),("asf","video/x-ms-asf"),("asn","chemical/x-ncbi-asn1"),("aso","chemical/x-ncbi-asn1-binary"),("asx","video/x-ms-asf"),("atom","application/atom"),("atomcat","application/atomcat+xml"),("atomsrv","application/atomserv+xml"),("au","audio/basic"),("avi","video/x-msvideo"),("b","chemical/x-molconn-Z"),("bak","application/x-trash"),("bat","application/x-msdos-program"),("bcpio","application/x-bcpio"),("bib","text/x-bibtex"),("bin","application/octet-stream"),("bmp","image/x-ms-bmp"),("boo","text/x-boo"),("book","application/x-maker"),("bsd","chemical/x-crossfire"),("c","text/x-csrc"),("c++","text/x-c++src"),("c3d","chemical/x-chem3d"),("cab","application/x-cab"),("cac","chemical/x-cache"),("cache","chemical/x-cache"),("cap","application/cap"),("cascii","chemical/x-cactvs-binary"),("cat","application/vnd.ms-pki.seccat"),("cbin","chemical/x-cactvs-binary"),("cbr","application/x-cbr"),("cbz","application/x-cbz"),("cc","text/x-c++src"),("cdf","application/x-cdf"),("cdr","image/x-coreldraw"),("cdt","image/x-coreldrawtemplate"),("cdx","chemical/x-cdx"),("cdy","application/vnd.cinderella"),("cef","chemical/x-cxf"),("cer","chemical/x-cerius"),("chm","chemical/x-chemdraw"),("chrt","application/x-kchart"),("cif","chemical/x-cif"),("class","application/java-vm"),("cls","text/x-tex"),("cmdf","chemical/x-cmdf"),("cml","chemical/x-cml"),("cod","application/vnd.rim.cod"),("com","application/x-msdos-program"),("cpa","chemical/x-compass"),("cpio","application/x-cpio"),("cpp","text/x-c++src"),("cpt","application/mac-compactpro"),("crl","application/x-pkcs7-crl"),("crt","application/x-x509-ca-cert"),("csf","chemical/x-cache-csf"),("csh","application/x-csh"),("csm","chemical/x-csml"),("csml","chemical/x-csml"),("css","text/css"),("csv","text/csv"),("ctab","chemical/x-cactvs-binary"),("ctx","chemical/x-ctx"),("cu","application/cu-seeme"),("cub","chemical/x-gaussian-cube"),("cxf","chemical/x-cxf"),("cxx","text/x-c++src"),("d","text/x-dsrc"),("dat","chemical/x-mopac-input"),("dcr","application/x-director"),("deb","application/x-debian-package"),("dif","video/dv"),("diff","text/x-diff"),("dir","application/x-director"),("djv","image/vnd.djvu"),("djvu","image/vnd.djvu"),("dl","video/dl"),("dll","application/x-msdos-program"),("dmg","application/x-apple-diskimage"),("dms","application/x-dms"),("doc","application/msword"),("dot","application/msword"),("dv","video/dv"),("dvi","application/x-dvi"),("dx","chemical/x-jcamp-dx"),("dxr","application/x-director"),("emb","chemical/x-embl-dl-nucleotide"),("embl","chemical/x-embl-dl-nucleotide"),("eml","message/rfc822"),("ent","chemical/x-ncbi-asn1-ascii"),("eps","application/postscript"),("etx","text/x-setext"),("exe","application/x-msdos-program"),("ez","application/andrew-inset"),("fb","application/x-maker"),("fbdoc","application/x-maker"),("fch","chemical/x-gaussian-checkpoint"),("fchk","chemical/x-gaussian-checkpoint"),("fig","application/x-xfig"),("flac","application/x-flac"),("fli","video/fli"),("fm","application/x-maker"),("frame","application/x-maker"),("frm","application/x-maker"),("gal","chemical/x-gaussian-log"),("gam","chemical/x-gamess-input"),("gamin","chemical/x-gamess-input"),("gau","chemical/x-gaussian-input"),("gcd","text/x-pcs-gcd"),("gcf","application/x-graphing-calculator"),("gcg","chemical/x-gcg8-sequence"),("gen","chemical/x-genbank"),("gf","application/x-tex-gf"),("gif","image/gif"),("gjc","chemical/x-gaussian-input"),("gjf","chemical/x-gaussian-input"),("gl","video/gl"),("gnumeric","application/x-gnumeric"),("gpt","chemical/x-mopac-graph"),("gsf","application/x-font"),("gsm","audio/x-gsm"),("gtar","application/x-gtar"),("h","text/x-chdr"),("h++","text/x-c++hdr"),("hdf","application/x-hdf"),("hh","text/x-c++hdr"),("hin","chemical/x-hin"),("hpp","text/x-c++hdr"),("hqx","application/mac-binhex40"),("hs","text/x-haskell"),("hta","application/hta"),("htc","text/x-component"),("htm","text/html"),("html","text/html"),("hxx","text/x-c++hdr"),("ica","application/x-ica"),("ice","x-conference/x-cooltalk"),("ico","image/x-icon"),("ics","text/calendar"),("icz","text/calendar"),("ief","image/ief"),("iges","model/iges"),("igs","model/iges"),("iii","application/x-iphone"),("inp","chemical/x-gamess-input"),("ins","application/x-internet-signup"),("iso","application/x-iso9660-image"),("isp","application/x-internet-signup"),("ist","chemical/x-isostar"),("istr","chemical/x-isostar"),("jad","text/vnd.sun.j2me.app-descriptor"),("jar","application/java-archive"),("java","text/x-java"),("jdx","chemical/x-jcamp-dx"),("jmz","application/x-jmol"),("jng","image/x-jng"),("jnlp","application/x-java-jnlp-file"),("jpe","image/jpeg"),("jpeg","image/jpeg"),("jpg","image/jpeg"),("js","application/x-javascript"),("kar","audio/midi"),("key","application/pgp-keys"),("kil","application/x-killustrator"),("kin","chemical/x-kinemage"),("kml","application/vnd.google-earth.kml+xml"),("kmz","application/vnd.google-earth.kmz"),("kpr","application/x-kpresenter"),("kpt","application/x-kpresenter"),("ksp","application/x-kspread"),("kwd","application/x-kword"),("kwt","application/x-kword"),("latex","application/x-latex"),("lha","application/x-lha"),("lhs","text/x-literate-haskell"),("lsf","video/x-la-asf"),("lsx","video/x-la-asf"),("ltx","text/x-tex"),("lyx","application/x-lyx"),("lzh","application/x-lzh"),("lzx","application/x-lzx"),("m3u","audio/mpegurl"),("m4a","audio/mpeg"),("maker","application/x-maker"),("man","application/x-troff-man"),("mcif","chemical/x-mmcif"),("mcm","chemical/x-macmolecule"),("mdb","application/msaccess"),("me","application/x-troff-me"),("mesh","model/mesh"),("mid","audio/midi"),("midi","audio/midi"),("mif","application/x-mif"),("mm","application/x-freemind"),("mmd","chemical/x-macromodel-input"),("mmf","application/vnd.smaf"),("mml","text/mathml"),("mmod","chemical/x-macromodel-input"),("mng","video/x-mng"),("moc","text/x-moc"),("mol","chemical/x-mdl-molfile"),("mol2","chemical/x-mol2"),("moo","chemical/x-mopac-out"),("mop","chemical/x-mopac-input"),("mopcrt","chemical/x-mopac-input"),("mov","video/quicktime"),("movie","video/x-sgi-movie"),("mp2","audio/mpeg"),("mp3","audio/mpeg"),("mp4","video/mp4"),("mpc","chemical/x-mopac-input"),("mpe","video/mpeg"),("mpeg","video/mpeg"),("mpega","audio/mpeg"),("mpg","video/mpeg"),("mpga","audio/mpeg"),("ms","application/x-troff-ms"),("msh","model/mesh"),("msi","application/x-msi"),("mvb","chemical/x-mopac-vib"),("mxu","video/vnd.mpegurl"),("nb","application/mathematica"),("nc","application/x-netcdf"),("nwc","application/x-nwc"),("o","application/x-object"),("oda","application/oda"),("odb","application/vnd.oasis.opendocument.database"),("odc","application/vnd.oasis.opendocument.chart"),("odf","application/vnd.oasis.opendocument.formula"),("odg","application/vnd.oasis.opendocument.graphics"),("odi","application/vnd.oasis.opendocument.image"),("odm","application/vnd.oasis.opendocument.text-master"),("odp","application/vnd.oasis.opendocument.presentation"),("ods","application/vnd.oasis.opendocument.spreadsheet"),("odt","application/vnd.oasis.opendocument.text"),("oga","audio/ogg"),("ogg","application/ogg"),("ogv","video/ogg"),("ogx","application/ogg"),("old","application/x-trash"),("otg","application/vnd.oasis.opendocument.graphics-template"),("oth","application/vnd.oasis.opendocument.text-web"),("otp","application/vnd.oasis.opendocument.presentation-template"),("ots","application/vnd.oasis.opendocument.spreadsheet-template"),("ott","application/vnd.oasis.opendocument.text-template"),("oza","application/x-oz-application"),("p","text/x-pascal"),("p7r","application/x-pkcs7-certreqresp"),("pac","application/x-ns-proxy-autoconfig"),("pas","text/x-pascal"),("pat","image/x-coreldrawpattern"),("patch","text/x-diff"),("pbm","image/x-portable-bitmap"),("pcap","application/cap"),("pcf","application/x-font"),("pcf.Z","application/x-font"),("pcx","image/pcx"),("pdb","chemical/x-pdb"),("pdf","application/pdf"),("pfa","application/x-font"),("pfb","application/x-font"),("pgm","image/x-portable-graymap"),("pgn","application/x-chess-pgn"),("pgp","application/pgp-signature"),("php","application/x-httpd-php"),("php3","application/x-httpd-php3"),("php3p","application/x-httpd-php3-preprocessed"),("php4","application/x-httpd-php4"),("phps","application/x-httpd-php-source"),("pht","application/x-httpd-php"),("phtml","application/x-httpd-php"),("pk","application/x-tex-pk"),("pl","text/x-perl"),("pls","audio/x-scpls"),("pm","text/x-perl"),("png","image/png"),("pnm","image/x-portable-anymap"),("pot","text/plain"),("ppm","image/x-portable-pixmap"),("pps","application/vnd.ms-powerpoint"),("ppt","application/vnd.ms-powerpoint"),("prf","application/pics-rules"),("prt","chemical/x-ncbi-asn1-ascii"),("ps","application/postscript"),("psd","image/x-photoshop"),("py","text/x-python"),("pyc","application/x-python-code"),("pyo","application/x-python-code"),("qt","video/quicktime"),("qtl","application/x-quicktimeplayer"),("ra","audio/x-pn-realaudio"),("ram","audio/x-pn-realaudio"),("rar","application/rar"),("ras","image/x-cmu-raster"),("rd","chemical/x-mdl-rdfile"),("rdf","application/rdf+xml"),("rgb","image/x-rgb"),("rhtml","application/x-httpd-eruby"),("rm","audio/x-pn-realaudio"),("roff","application/x-troff"),("ros","chemical/x-rosdal"),("rpm","application/x-redhat-package-manager"),("rss","application/rss+xml"),("rtf","application/rtf"),("rtx","text/richtext"),("rxn","chemical/x-mdl-rxnfile"),("sct","text/scriptlet"),("sd","chemical/x-mdl-sdfile"),("sd2","audio/x-sd2"),("sda","application/vnd.stardivision.draw"),("sdc","application/vnd.stardivision.calc"),("sdd","application/vnd.stardivision.impress"),("sdf","application/vnd.stardivision.math"),("sds","application/vnd.stardivision.chart"),("sdw","application/vnd.stardivision.writer"),("ser","application/java-serialized-object"),("sgf","application/x-go-sgf"),("sgl","application/vnd.stardivision.writer-global"),("sh","application/x-sh"),("shar","application/x-shar"),("shtml","text/html"),("sid","audio/prs.sid"),("sik","application/x-trash"),("silo","model/mesh"),("sis","application/vnd.symbian.install"),("sisx","x-epoc/x-sisx-app"),("sit","application/x-stuffit"),("sitx","application/x-stuffit"),("skd","application/x-koan"),("skm","application/x-koan"),("skp","application/x-koan"),("skt","application/x-koan"),("smi","application/smil"),("smil","application/smil"),("snd","audio/basic"),("spc","chemical/x-galactic-spc"),("spl","application/futuresplash"),("spx","audio/ogg"),("src","application/x-wais-source"),("stc","application/vnd.sun.xml.calc.template"),("std","application/vnd.sun.xml.draw.template"),("sti","application/vnd.sun.xml.impress.template"),("stl","application/vnd.ms-pki.stl"),("stw","application/vnd.sun.xml.writer.template"),("sty","text/x-tex"),("sv4cpio","application/x-sv4cpio"),("sv4crc","application/x-sv4crc"),("svg","image/svg+xml"),("svgz","image/svg+xml"),("sw","chemical/x-swissprot"),("swf","application/x-shockwave-flash"),("swfl","application/x-shockwave-flash"),("sxc","application/vnd.sun.xml.calc"),("sxd","application/vnd.sun.xml.draw"),("sxg","application/vnd.sun.xml.writer.global"),("sxi","application/vnd.sun.xml.impress"),("sxm","application/vnd.sun.xml.math"),("sxw","application/vnd.sun.xml.writer"),("t","application/x-troff"),("tar","application/x-tar"),("taz","application/x-gtar"),("tcl","application/x-tcl"),("tex","text/x-tex"),("texi","application/x-texinfo"),("texinfo","application/x-texinfo"),("text","text/plain"),("tgf","chemical/x-mdl-tgf"),("tgz","application/x-gtar"),("tif","image/tiff"),("tiff","image/tiff"),("tk","text/x-tcl"),("tm","text/texmacs"),("torrent","application/x-bittorrent"),("tr","application/x-troff"),("ts","text/texmacs"),("tsp","application/dsptype"),("tsv","text/tab-separated-values"),("txt","text/plain"),("udeb","application/x-debian-package"),("uls","text/iuls"),("ustar","application/x-ustar"),("val","chemical/x-ncbi-asn1-binary"),("vcd","application/x-cdlink"),("vcf","text/x-vcard"),("vcs","text/x-vcalendar"),("vmd","chemical/x-vmd"),("vms","chemical/x-vamas-iso14976"),("vrm","x-world/x-vrml"),("vrml","model/vrml"),("vsd","application/vnd.visio"),("wad","application/x-doom"),("wav","audio/x-wav"),("wax","audio/x-ms-wax"),("wbmp","image/vnd.wap.wbmp"),("wbxml","application/vnd.wap.wbxml"),("wk","application/x-123"),("wm","video/x-ms-wm"),("wma","audio/x-ms-wma"),("wmd","application/x-ms-wmd"),("wml","text/vnd.wap.wml"),("wmlc","application/vnd.wap.wmlc"),("wmls","text/vnd.wap.wmlscript"),("wmlsc","application/vnd.wap.wmlscriptc"),("wmv","video/x-ms-wmv"),("wmx","video/x-ms-wmx"),("wmz","application/x-ms-wmz"),("wp5","application/wordperfect5.1"),("wpd","application/wordperfect"),("wrl","model/vrml"),("wsc","text/scriptlet"),("wvx","video/x-ms-wvx"),("wz","application/x-wingz"),("xbm","image/x-xbitmap"),("xcf","application/x-xcf"),("xht","application/xhtml+xml"),("xhtml","application/xhtml+xml"),("xlb","application/vnd.ms-excel"),("xls","application/vnd.ms-excel"),("xlt","application/vnd.ms-excel"),("xml","application/xml"),("xpi","application/x-xpinstall"),("xpm","image/x-xpixmap"),("xsl","application/xml"),("xtel","chemical/x-xtel"),("xul","application/vnd.mozilla.xul+xml"),("xwd","image/x-xwindowdump"),("xyz","chemical/x-xyz"),("zip","application/zip"),("zmt","chemical/x-mopac-input"),("~","application/x-trash")]

