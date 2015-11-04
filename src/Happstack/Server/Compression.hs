{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
-- | Filter for compressing the 'Response' body.
module Happstack.Server.Compression
    ( compressedResponseFilter
    , compressedResponseFilter'
    , compressWithFilter
    , gzipFilter
    , deflateFilter
    , identityFilter
    , starFilter
    , standardEncodingHandlers
    ) where

import Happstack.Server.Internal.Compression ( compressedResponseFilter
                                             , compressedResponseFilter'
                                             , compressWithFilter
                                             , gzipFilter
                                             , deflateFilter
                                             , identityFilter
                                             , starFilter
                                             , standardEncodingHandlers
                                             )

