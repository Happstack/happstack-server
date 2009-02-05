{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

module Happstack.Server.SURI where
import Data.Maybe
import Data.Generics
import Happstack.Util.Common(mapFst)
import qualified Network.URI as URI

path, query, scheme :: SURI -> String
path  = URI.uriPath . suri
query  = URI.uriQuery . suri
scheme  = URI.uriScheme . suri

u_scheme, u_path :: (String -> String) -> SURI -> SURI
u_scheme f (SURI u) = SURI (u {URI.uriScheme=f $ URI.uriScheme u})
u_path f (SURI u) = SURI $ u {URI.uriPath=f $ URI.uriPath u}
a_scheme, a_path :: String -> SURI -> SURI
a_scheme a (SURI u) = SURI $ u {URI.uriScheme=a}
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

--parse  = fmap SURI . URI.parseURIReference ::String->Maybe SURI

-- | Render should be used for prettyprinting URIs.
render :: (ToSURI a) => a -> String
render x = (show . suri . toSURI) x
parse :: String -> Maybe SURI
parse =  fmap SURI . URI.parseURIReference 


class ToSURI x where toSURI::x->SURI
instance ToSURI SURI where toSURI=id
instance ToSURI URI.URI where toSURI=SURI
instance ToSURI String where 
    toSURI = maybe (SURI $ URI.URI "" Nothing "" "" "") id . parse


--handling obtaining things from URI paths
class FromPath x where fromPath::String->x
