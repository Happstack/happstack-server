-- | functions for serving static files from the disk
module Happstack.Server.FileServe 
    ( -- * Serving Functions 
      Browsing(..)
    , serveDirectory
    , serveFile
     -- * Content-Type \/ Mime-Type
     , MimeMap
     , mimeTypes
     , asContentType
     , guessContentTypeM
     -- * Index Files
    , defaultIxFiles
    -- * Deprecated
    , fileServe
    )
    where

import Happstack.Server.FileServe.BuildingBlocks
