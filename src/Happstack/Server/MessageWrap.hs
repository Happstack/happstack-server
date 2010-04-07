{-# LANGUAGE FlexibleInstances #-}

module Happstack.Server.MessageWrap where

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Happstack.Server.HTTP.Types as H
import Happstack.Server.HTTP.Multipart
import Happstack.Server.SURI as SURI
import Happstack.Util.Common

queryInput :: SURI -> [(String, Input)]
queryInput uri = formDecode (case SURI.query uri of
                               '?':r -> r
                               xs    -> xs)

bodyInput :: Request -> [(String, Input)]
bodyInput req | (rqMethod req /= POST) && (rqMethod req /= PUT) = []
bodyInput req =
    let ctype = getHeader "content-type" req >>= parseContentType . P.unpack
        getBS (Body bs) = bs
    in decodeBody ctype (getBS $ rqBody req)


-- | Decodes application\/x-www-form-urlencoded inputs.      
formDecode :: String -> [(String, Input)]
formDecode [] = []
formDecode qString = 
    if null pairString then rest else 
           (SURI.unEscape name,simpleInput $ SURI.unEscape val):rest
    where (pairString,qString')= split (=='&') qString
          (name,val)=split (=='=') pairString
          rest=if null qString' then [] else formDecode qString'

decodeBody :: Maybe ContentType
           -> L.ByteString
           -> [(String,Input)]
decodeBody ctype inp
    = case ctype of
        Just (ContentType "application" "x-www-form-urlencoded" _)
            -> formDecode (L.unpack inp)
        Just (ContentType "multipart" "form-data" ps)
            -> multipartDecode ps inp
        Just _ -> [] -- unknown content-type, the user will have to
                     -- deal with it by looking at the raw content
        -- No content-type given, assume x-www-form-urlencoded
        Nothing -> formDecode (L.unpack inp)

-- | Decodes multipart\/form-data input.
multipartDecode :: [(String,String)] -- ^ Content-type parameters
                -> L.ByteString        -- ^ Request body
                -> [(String,Input)]  -- ^ Input variables and values.
multipartDecode ps inp =
    case lookup "boundary" ps of
         Just b -> case parseMultipartBody b inp of
                        Just (MultiPart bs) -> map bodyPartToInput bs
                        Nothing -> [] -- FIXME: report parse error
         Nothing -> [] -- FIXME: report that there was no boundary

bodyPartToInput :: BodyPart -> (String,Input)
bodyPartToInput (BodyPart hs b) =
    case getContentDisposition hs of
              Just (ContentDisposition "form-data" ps) ->
                  (fromMaybe "" $ lookup "name" ps,
                   Input { inputValue = b,
                           inputFilename = lookup "filename" ps,
                           inputContentType = ctype })
              _ -> ("ERROR",simpleInput "ERROR") -- FIXME: report error
    where ctype = fromMaybe defaultInputType (getContentType hs)

-- | Packs a string into an Input of type "text/plain"
simpleInput :: String -> Input
simpleInput v
    = Input { inputValue = L.pack v
            , inputFilename = Nothing
            , inputContentType = defaultInputType
            }

-- | The default content-type for variables.
defaultInputType :: ContentType
defaultInputType = ContentType "text" "plain" [] -- FIXME: use some default encoding?

-- | Get the path components from a String.
pathEls :: String -> [String]
pathEls = (drop 1) . map SURI.unEscape . splitList '/' 

-- | Like 'Read' except Strings and Chars not quoted.
class (Read a)=>ReadString a where readString::String->a; readString =read 

instance ReadString Int 
instance ReadString Double 
instance ReadString Float 
instance ReadString SURI.SURI where readString = read . show
instance ReadString [Char] where readString=id
instance ReadString Char where 
    readString s= if length t==1 then head t else read t where t=trim s 
