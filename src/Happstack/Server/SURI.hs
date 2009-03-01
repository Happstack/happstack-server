{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

module Happstack.Server.SURI where
import Data.Maybe
import Data.Generics
import Happstack.Util.Common(mapFst)
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

isAbs :: SURI -> Bool
isAbs = not . null . URI.uriScheme . suri
--isAbs = maybe True ( mbParsed

newtype SURI = SURI {suri::URI.URI} deriving (Eq,Data,Typeable)
instance Show SURI where
    showsPrec d (SURI uri) = showsPrec d $ show uri
instance Read SURI where
    readsPrec d = mapFst fromJust .  filter (isJust . fst) . mapFst parse . readsPrec d 

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


--handling obtaining things from URI paths
class FromPath x where fromPath::String->x
