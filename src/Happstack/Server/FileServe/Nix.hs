{-# LANGUAGE FlexibleContexts #-}
-- | Serve files whose on-disk timestamps are meaningless because they
-- live in the Nix store.
--
-- On NixOS (and more generally, whenever files are served straight out
-- of @\/nix\/store@, or via a symlink into it), every file's
-- modification time is reset to a fixed epoch
-- (@1970-01-01T00:00:01Z@). 'Happstack.Server.FileServe.serveFile' and
-- 'Happstack.Server.FileServe.serveDirectory' rely on
-- 'System.Directory.getModificationTime' to generate the
-- @Last-Modified@ header used for conditional GETs, so browsers never
-- see a file as having changed after a new package is deployed, and
-- can keep serving a stale cached copy indefinitely.
--
-- Nix store paths are content-addressed
-- (@\/nix\/store\/\<hash\>-\<name\>@), so the hash itself is a perfect,
-- free entity tag: it changes whenever the file's content changes, and
-- stays fixed otherwise. The functions in this module resolve the
-- requested path (following any symlinks, such as those an activation
-- script uses to point a stable path at the current store path),
-- derive an @ETag@ from the resulting store hash, and use it to honor
-- @if-none-match@ instead of @if-modified-since@ whenever the file
-- resolves into the store.
--
-- Use 'serveFileNix' and 'serveDirectoryNix' exactly as you would
-- 'Happstack.Server.FileServe.serveFile' and
-- 'Happstack.Server.FileServe.serveDirectory'.
module Happstack.Server.FileServe.Nix
    ( -- * Serving Functions
      serveFileNix
    , serveFileFromNix
    , serveDirectoryNix
    , serveDirectoryNix'
      -- * Low-Level
    , sendFileResponseNix
    , filePathSendFileNix
      -- * Nix store hashes
    , nixStoreHash
    , getNixETag
    ) where

import Control.Monad                       (MonadPlus, msum)
import Control.Monad.Trans                 (MonadIO(liftIO))
import qualified Data.Map                  as Map
import Data.List                           (stripPrefix)
import Data.Time                           (UTCTime)
import Happstack.Server.FileServe.BuildingBlocks
    ( Browsing(..), browseIndex, combineSafe, fileServe', guessContentTypeM
    , mimeTypes, renderDirectoryContents, serveFileUsing, tryIndex
    )
import Happstack.Server.Monads             (ServerMonad(askRq), FilterMonad, WebMonad)
import Happstack.Server.Response           (ToMessage(toResponse), forbidden, ifModifiedSince, ifNoneMatch)
import Happstack.Server.Types              (Length(ContentLength), Request, Response(SendFile), RsFlags(rsfLength), nullRsFlags, setHeader)
import System.Directory                    (canonicalizePath, getModificationTime)
import System.IO                           (IOMode(ReadMode), hFileSize, withBinaryFile)

-- | Extract the content hash from a Nix store path, if the given path
-- lies directly under @\/nix\/store@.
--
-- >>> nixStoreHash "/nix/store/9b9f3z6y1k9vjjb3l5x1x1x1x1x1x1x1-hello-2.10/bin/hello"
-- Just "9b9f3z6y1k9vjjb3l5x1x1x1x1x1x1x1"
nixStoreHash :: FilePath -> Maybe String
nixStoreHash fp =
    do rest <- stripPrefix "/nix/store/" fp
       let hash = takeWhile (/= '-') rest
       if length hash == 32 && all isNixBase32Char hash
          then Just hash
          else Nothing
    where
      isNixBase32Char c = c `elem` ("0123456789abcdfghijklmnpqrsvwxyz" :: String)

-- | Resolve a 'FilePath' (following symlinks) and compute an entity
-- tag from its Nix store hash, if it has one.
--
-- Returns 'Nothing' if the resolved path is not inside @\/nix\/store@,
-- in which case callers should fall back to modification-time based
-- caching.
getNixETag :: FilePath -> IO (Maybe String)
getNixETag fp =
    do resolved <- canonicalizePath fp
       return $ nixStoreHash resolved

-- | Like 'Happstack.Server.FileServe.BuildingBlocks.sendFileResponse',
-- but prefers a Nix-store-derived @ETag@ (see 'getNixETag') over
-- @if-modified-since@ when one is available.
sendFileResponseNix :: String            -- ^ content-type string
                     -> FilePath         -- ^ file path for content to send
                     -> Maybe String     -- ^ entity tag, see 'getNixETag'
                     -> Maybe UTCTime    -- ^ mod-time for the file, used only when no entity tag is available
                     -> Request          -- ^ incoming request (used to check for if-modified-since \/ if-none-match)
                     -> Integer          -- ^ offset into file
                     -> Integer          -- ^ number of bytes to send
                     -> Response
sendFileResponseNix ct filePath mEtag mModTime request offset count =
    let base = setHeader "Content-Type" ct $
               SendFile 200 Map.empty (nullRsFlags { rsfLength = ContentLength }) Nothing filePath offset count
    in case mEtag of
         Just etag -> ifNoneMatch etag request base
         Nothing   -> maybe base (\modTime -> ifModifiedSince modTime request base) mModTime

-- | Send the specified file using @sendfile()@, with a Nix-store-aware
-- @ETag@ when the file resolves into the store.
--
-- NOTE: assumes file exists and is readable by the server. See
-- 'Happstack.Server.FileServe.BuildingBlocks.serveFileUsing'.
--
-- WARNING: No security checks are performed.
filePathSendFileNix :: (ServerMonad m, MonadIO m)
                     => String   -- ^ content-type string
                     -> FilePath -- ^ path to file on disk
                     -> m Response
filePathSendFileNix contentType fp =
    do count   <- liftIO $ withBinaryFile fp ReadMode hFileSize
       modtime <- liftIO $ getModificationTime fp
       etag    <- liftIO $ getNixETag fp
       rq      <- askRq
       return $ sendFileResponseNix contentType fp etag (Just modtime) rq 0 count

-- | Like 'Happstack.Server.FileServe.serveFile', but uses
-- 'filePathSendFileNix' to serve, so files that resolve into the Nix
-- store get a content-hash @ETag@ instead of a useless, constant
-- @Last-Modified@ time.
serveFileNix :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
                 (FilePath -> m String)   -- ^ function for determining content-type of file. Typically 'Happstack.Server.FileServe.asContentType' or 'Happstack.Server.FileServe.guessContentTypeM'
              -> FilePath                 -- ^ path to the file to serve
              -> m Response
serveFileNix = serveFileUsing filePathSendFileNix

-- | Like 'serveFileNix', but uses 'Happstack.Server.FileServe.BuildingBlocks.combineSafe'
-- to prevent directory traversal attacks when the path to the file is
-- supplied by the user.
serveFileFromNix :: (ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
                     FilePath                 -- ^ directory wherein served files must be contained
                  -> (FilePath -> m String)   -- ^ function for determining content-type of file
                  -> FilePath                 -- ^ path to the file to serve
                  -> m Response
serveFileFromNix root mimeFn fp =
    maybe no yes $ combineSafe root fp
  where
    no  = forbidden $ toResponse "Directory traversal forbidden"
    yes = serveFileNix mimeFn

-- | Like 'Happstack.Server.FileServe.serveDirectory', but serves files
-- and index pages via 'filePathSendFileNix'.
serveDirectoryNix :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
                      Browsing    -- ^ allow directory browsing
                   -> [FilePath]  -- ^ index file names, in case the requested path is a directory
                   -> FilePath    -- ^ file/directory to serve
                   -> m Response
serveDirectoryNix browsing ixFiles localPath =
    serveDirectoryNix' browsing ixFiles (guessContentTypeM mimeTypes) localPath

-- | Like 'serveDirectoryNix' but with custom mime types.
serveDirectoryNix' :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m)
                    => Browsing    -- ^ allow directory browsing
                    -> [FilePath]  -- ^ index file names, in case the requested path is a directory
                    -> (FilePath -> m String) -- ^ function which returns the mime-type for FilePath
                    -> FilePath    -- ^ file/directory to serve
                    -> m Response
serveDirectoryNix' browsing ixFiles mimeFn localPath =
    fileServe' filePathSendFileNix mimeFn indexFn localPath
        where
          indexFn fp =
              msum [ tryIndex filePathSendFileNix mimeFn ixFiles fp
                   , if browsing == EnableBrowsing
                        then browseIndex renderDirectoryContents filePathSendFileNix mimeFn ixFiles fp
                        else forbidden $ toResponse "Directory index forbidden"
                   ]
