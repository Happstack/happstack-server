{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}
-- | A wrapper and type class so that functions like 'seeOther' can take a URI which is represented by a 'String', 'URI.URI', or other instance of 'ToSURI'. 
module Happstack.Server.SURI where

import Control.Arrow (first)
import Data.Maybe
import Data.Generics
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Network.URI as URI

-- | Retrieves the path component from the URI
path :: SURI -> String
path  = URI.uriPath . suri

-- | Retrieves the query component from the URI
query :: SURI -> String
query  = URI.uriQuery . suri

-- | Retrieves the scheme component from the URI
scheme :: SURI -> String
scheme  = URI.uriScheme . suri

-- | Modifies the scheme component of the URI using the provided function
u_scheme :: (String -> String) -> SURI -> SURI
u_scheme f (SURI u) = SURI (u {URI.uriScheme=f $ URI.uriScheme u})

-- | Modifies the path component of the URI using the provided function
u_path :: (String -> String) -> SURI -> SURI
u_path f (SURI u) = SURI $ u {URI.uriPath=f $ URI.uriPath u}

-- | Sets the scheme component of the URI
a_scheme :: String -> SURI -> SURI
a_scheme a (SURI u) = SURI $ u {URI.uriScheme=a}

-- | Sets the path component of the URI
a_path :: String -> SURI -> SURI
a_path a (SURI u) = SURI $ u {URI.uriPath=a}

escape, unEscape :: String -> String
unEscape = URI.unEscapeString . map (\x->if x=='+' then ' ' else x)
escape = URI.escapeURIString URI.isAllowedInURI

-- | Returns true if the URI is absolute
isAbs :: SURI -> Bool
isAbs = not . null . URI.uriScheme . suri

newtype SURI = SURI {suri::URI.URI} deriving (Eq,Data,Typeable)
instance Show SURI where
    showsPrec d (SURI uri) = showsPrec d $ show uri
instance Read SURI where
    readsPrec d = mapFst fromJust .  filter (isJust . fst) . mapFst parse . readsPrec d 
      where
        mapFst :: (a -> b) -> [(a,x)] -> [(b,x)]
        mapFst = map . first

instance Ord SURI where
    compare a b = show a `compare` show b

-- | Render should be used for prettyprinting URIs.
render :: (ToSURI a) => a -> String
render = show . suri . toSURI

-- | Parses a URI from a String.  Returns Nothing on failure.
parse :: String -> Maybe SURI
parse =  fmap SURI . URI.parseURIReference 

-- | Convenience class for converting data types to URIs
class ToSURI x where toSURI::x->SURI

instance ToSURI SURI where toSURI=id
instance ToSURI URI.URI where toSURI=SURI
instance ToSURI String where 
    toSURI = maybe (SURI $ URI.URI "" Nothing "" "" "") id . parse
instance ToSURI Text.Text where toSURI = toSURI . Text.unpack
instance ToSURI LazyText.Text where toSURI = toSURI . LazyText.unpack

--handling obtaining things from URI paths
class FromPath x where fromPath::String->x
