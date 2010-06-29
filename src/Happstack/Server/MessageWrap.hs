{-# LANGUAGE FlexibleInstances #-}

module Happstack.Server.MessageWrap where

import Control.Concurrent.MVar (tryTakeMVar, putMVar)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Data.Int (Int64)
import Happstack.Server.HTTP.Types as H
import Happstack.Server.HTTP.Multipart
import Happstack.Server.HTTP.RFC822Headers (parseContentType)
import Happstack.Server.SURI as SURI
import Happstack.Util.Common

queryInput :: SURI -> [(String, Input)]
queryInput uri = formDecode (case SURI.query uri of
                               '?':r -> r
                               xs    -> xs)

data BodyPolicy 
    = BodyPolicy { inputWorker :: Int64 -> Int64 -> Int64 -> InputWorker
                 , maxDisk     :: Int64 -- ^ maximum bytes to save to disk (files)
                 , maxRAM      :: Int64 -- ^ maximum bytes to hold in RAM 
                 , maxHeader   :: Int64 -- ^ maximum header size (this only affects headers in the multipart/form-data)
                 }

defaultBodyPolicy :: FilePath -> Int64 -> Int64 -> Int64 -> BodyPolicy
defaultBodyPolicy tmpDir md mr mh =
    BodyPolicy { inputWorker = defaultInputIter tmpDir 0 0 0
               , maxDisk   = md
               , maxRAM    = mr
               , maxHeader = mh
               }

bodyInput :: (MonadIO m) => BodyPolicy -> Request -> m ([(String, Input)], Maybe String)
bodyInput _ req | (rqMethod req /= POST) && (rqMethod req /= PUT) = return ([], Nothing)
bodyInput bodyPolicy req =
  liftIO $
    let ctype = getHeader "content-type" req >>= parseContentType . P.unpack
        getBS (Body bs) = bs
    in do mbi <- tryTakeMVar (rqInputsBody req)
          case mbi of
            (Just bi) ->
                do putMVar (rqInputsBody req) bi
                   return (bi, Nothing)
            Nothing ->
                 do rqBody <- takeRequestBody req
                    case rqBody of
                      Nothing          -> return ([], Just $ "bodyInput: Request body was already consumed.")
                      (Just (Body bs)) -> 
                          do r@(inputs, err) <- decodeBody bodyPolicy ctype bs
                             putMVar (rqInputsBody req) inputs
                             return r

-- | Decodes application\/x-www-form-urlencoded inputs.      
-- TODO: should any of the [] be error conditions?
formDecode :: String -> [(String, Input)]
formDecode [] = []
formDecode qString = 
    if null pairString then rest else 
           (SURI.unEscape name,simpleInput $ SURI.unEscape val):rest
    where (pairString,qString')= split (=='&') qString
          (name,val)=split (=='=') pairString
          rest=if null qString' then [] else formDecode qString'

-- FIXME: no size limits on application/x-www-form-urlencoded yet
-- FIXME: is usend L.unpack really the right thing to do
decodeBody :: BodyPolicy
           -> Maybe ContentType
           -> L.ByteString
           -> IO ([(String,Input)], Maybe String)
decodeBody bp ctype inp
    = case ctype of
        Just (ContentType "application" "x-www-form-urlencoded" _) ->
            return (formDecode (L.unpack (L.take (maxRAM bp) inp)), Nothing)
        Just (ContentType "multipart" "form-data" ps) ->
            multipartDecode ((inputWorker bp) (maxDisk bp) (maxRAM bp) (maxHeader bp)) ps inp
        Just ct -> 
            return ([], Just $ "decodeBody: unsupported content-type: " ++ show ct) -- unknown content-type, the user will have to
                     -- deal with it by looking at the raw content
        -- No content-type given, assume x-www-form-urlencoded
        Nothing -> return (formDecode (L.unpack (L.take (maxRAM bp) inp)), Nothing)

-- | Decodes multipart\/form-data input.
multipartDecode :: InputWorker
                -> [(String,String)] -- ^ Content-type parameters
                -> L.ByteString      -- ^ Request body
                -> IO ([(String,Input)], Maybe String) -- ^ Input variables and values.
multipartDecode inputWorker ps inp =
    case lookup "boundary" ps of
         Just b  -> multipartBody inputWorker (L.pack b) inp
         Nothing -> return ([], Just $ "boundary not found in parameters: " ++ show ps)

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
