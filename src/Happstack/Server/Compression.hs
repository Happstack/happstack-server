{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
-- | Filter for compressing the 'Response' body.
module Happstack.Server.Compression
    ( compressedResponseFilter
    ) where

import Happstack.Server.Internal.Compression (compressedResponseFilter)
