module Happstack.Server.Internal.Multipart where

import           Control.Monad (MonadPlus(mplus), foldM)
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Internal      as B
import           Data.ByteString.Lazy.Internal (ByteString(Chunk, Empty))
import           Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Lazy.UTF8     as LU
import qualified Data.ByteString.Char8         as S
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)
import           Data.Int (Int64)
import           Text.ParserCombinators.Parsec (ParseError, parse)
import           Happstack.Server.Internal.Types (Input(..))
import           Happstack.Server.Internal.RFC822Headers ( ContentType(..), ContentDisposition(..), Header
                                                         , getContentDisposition, getContentType, pHeaders)
import           System.IO (Handle, hClose, openBinaryTempFile)

-- | similar to the normal 'span' function, except the predicate gets the whole rest of the lazy bytestring, not just one character.
--
-- TODO: this function has not been profiled.
spanS :: (L.ByteString -> Bool) -> L.ByteString -> (L.ByteString, L.ByteString)
spanS f cs0 = spanS' 0 cs0
  where spanS' n Empty = (Empty, Empty)
        spanS' n bs@(Chunk c cs)
            | n >= S.length c = 
                let (x, y) = spanS' 0 cs
                in (Chunk c x, y)
            | not (f (Chunk (S.drop n c) cs)) = L.splitAt (fromIntegral n) bs
            | otherwise = (spanS' (n + 1) bs)
{-# INLINE spanS #-}

takeWhileS :: (L.ByteString -> Bool) -> L.ByteString -> L.ByteString
takeWhileS f cs0 = takeWhile' 0 cs0
  where takeWhile' n Empty = Empty
        takeWhile' n bs@(Chunk c cs)
            | n >= S.length c = Chunk c (takeWhile' 0 cs)
            | not (f (Chunk (S.drop n c) cs)) = (Chunk (S.take n c) Empty)
            | otherwise = takeWhile' (n + 1) bs

crlf :: L.ByteString
crlf = L.pack "\r\n"

crlfcrlf :: L.ByteString
crlfcrlf = L.pack "\r\n\r\n"

blankLine :: L.ByteString
blankLine = L.pack "\r\n\r\n"

dropWhileS :: (L.ByteString -> Bool) -> L.ByteString -> L.ByteString
dropWhileS f cs0 = dropWhile' cs0
    where dropWhile' bs 
              | L.null bs  = bs
              | f bs       = dropWhile' (L.drop 1 bs)
              | otherwise  = bs

data BodyPart = BodyPart L.ByteString L.ByteString  -- ^ headers body
    deriving (Eq, Ord, Read, Show)

data Work 
    = BodyWork ContentType [(String, String)] L.ByteString
    | HeaderWork L.ByteString 

type InputWorker = Work -> IO InputIter

data InputIter 
    = Failed (Maybe (String, Input)) String
    | BodyResult (String, Input) InputWorker
    | HeaderResult [Header] InputWorker

type FileSaver = FilePath 		-- ^ tempdir
		-> Int64 		-- ^ quota
		-> FilePath 		-- ^ filename of field
		-> L.ByteString 	-- ^ content to save
		-> IO (Bool, Int64 , FilePath)	-- ^ truncated?, saved bytes, saved filename

defaultFileSaver tmpDir diskQuota filename b =
    do (fn, h) <- openBinaryTempFile tmpDir filename
       (trunc, len) <- hPutLimit diskQuota h b
       hClose h
       return (trunc, len, fn) 

defaultInputIter :: FileSaver -> FilePath -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Work -> IO InputIter
defaultInputIter fileSaver tmpDir diskCount ramCount headerCount maxDisk maxRAM maxHeader (BodyWork ctype ps b)
    | diskCount > maxDisk = return $ Failed Nothing ("diskCount (" ++ show diskCount ++ ") is greater than maxDisk (" ++ show maxDisk  ++ ")")
    | ramCount  > maxRAM  = return $ Failed Nothing ("ramCount ("  ++ show ramCount  ++ ") is greater than maxRAM ("  ++ show maxRAM   ++ ")")
    | otherwise =
        case lookup "filename" ps of
          Nothing -> 
              let (b',rest) = L.splitAt (maxRAM - ramCount) b
                  input = (fromMaybe "" $ lookup "name" ps
                          , Input { inputValue       = (Right b')
                                  , inputFilename    = Nothing
                                  , inputContentType = ctype })
              in if L.null rest
                  then return $ BodyResult input (defaultInputIter fileSaver tmpDir diskCount (ramCount + L.length b) headerCount maxDisk maxRAM maxHeader)
                  else return $ Failed (Just input) ("Reached RAM quota of " ++ show maxRAM ++ " bytes.")

          (Just filename) ->
              do (trunc, len, fn) <- fileSaver tmpDir (maxDisk - diskCount) filename b
                 let input = ( fromMaybe "" $ lookup "name" ps
                             , Input { inputValue       = Left fn
                                     , inputFilename    = (Just filename)
                                     , inputContentType = ctype })
                 if trunc
                    then return $ Failed (Just input) ("Reached disk quota of " ++ show maxDisk ++ " bytes.")
                    else return $ BodyResult input (defaultInputIter fileSaver tmpDir (diskCount + len) ramCount headerCount maxDisk maxRAM maxHeader)

defaultInputIter fileSaver tmpDir diskCount ramCount headerCount maxDisk maxRAM maxHeader (HeaderWork bs) =
    case L.splitAt (maxHeader - headerCount) bs of
      (_hs, rest)
          | not (L.null rest) -> return $ Failed Nothing ("Reached header quota of " ++ show maxHeader ++ " bytes.")
          | otherwise ->
              case parse pHeaders (LU.toString bs) (LU.toString bs) of
                (Left e) -> return $ Failed Nothing (show e)
                (Right hs) ->
                    return $ HeaderResult hs
                               (defaultInputIter fileSaver tmpDir diskCount ramCount (headerCount + (L.length bs)) maxDisk maxRAM maxHeader)
{-# INLINE defaultInputIter #-}

hPutLimit :: Int64 -> Handle -> L.ByteString -> IO (Bool, Int64)
hPutLimit maxCount h bs = hPutLimit' maxCount h 0 bs
{-# INLINE hPutLimit #-}

hPutLimit' :: Int64 -> Handle -> Int64 -> L.ByteString -> IO (Bool, Int64)
hPutLimit' _maxCount h count Empty = return (False, count)
hPutLimit'  maxCount h count (Chunk c cs)
    | (count + fromIntegral (S.length c)) > maxCount =
        do S.hPut h (S.take (fromIntegral (maxCount - count)) c)
           return (True, maxCount)
    | otherwise =
        do S.hPut h c
           hPutLimit' maxCount h (count + fromIntegral (S.length c)) cs
{-# INLINE hPutLimit' #-}

-- FIXME: can we safely use L.unpack, or do we need to worry about encoding issues in the headers?
bodyPartToInput :: InputWorker -> BodyPart -> IO InputIter -- (Either String (String,Input))
bodyPartToInput inputWorker (BodyPart rawHS b) =
    do r <- inputWorker (HeaderWork rawHS)
       case r of
         (Failed i e) -> return $ Failed i e
         (HeaderResult hs cont) ->
          let ctype = fromMaybe defaultInputType (getContentType hs) in
          case getContentDisposition hs of
              Just (ContentDisposition "form-data" ps) ->
                  cont (BodyWork ctype ps b)

              cd -> return $ Failed Nothing ("Expected content-disposition: form-data but got " ++ show cd)

bodyPartsToInputs :: InputWorker -> [BodyPart] -> IO ([(String,Input)], Maybe String)
bodyPartsToInputs _ [] = 
    return ([], Nothing)
bodyPartsToInputs inputWorker (b:bs) =
    do r <- bodyPartToInput inputWorker b
       case r of
         (Failed mInput e) ->
             case mInput of
               Nothing  -> return ([], Just e)
               (Just i) -> return ([i], Just e)
         (BodyResult i cont) ->
             do (is, err) <- bodyPartsToInputs cont bs
                return (i:is, err)
         (HeaderResult _ _) ->
             return ([], Just "InputWorker is broken. Returned a HeaderResult when a BodyResult was required.")

multipartBody :: InputWorker -> L.ByteString -> L.ByteString -> IO ([(String, Input)], Maybe String)
multipartBody inputWorker boundary s =
    do let (bodyParts, mErr) = parseMultipartBody boundary s
       (inputs, mErr2) <- bodyPartsToInputs inputWorker bodyParts
       return (inputs, mErr2 `mplus` mErr)

-- | Packs a string into an Input of type "text/plain"
simpleInput :: String -> Input
simpleInput v
    = Input { inputValue       = Right (L.pack v)
            , inputFilename    = Nothing
            , inputContentType = defaultInputType
            }

-- | The default content-type for variables.
defaultInputType :: ContentType
defaultInputType = ContentType "text" "plain" [] -- FIXME: use some default encoding?

parseMultipartBody :: L.ByteString -> L.ByteString -> ([BodyPart], Maybe String)
parseMultipartBody boundary s =
    case dropPreamble boundary s of
      (partData, Just e)  -> ([], Just e)
      (partData, Nothing) -> splitParts boundary partData

dropPreamble :: L.ByteString -> L.ByteString -> (L.ByteString, Maybe String)
dropPreamble b s | isBoundary b s = (dropLine s, Nothing)
                 | L.null s = (s, Just $ "Boundary " ++ L.unpack b ++ " not found.")
                 | otherwise = dropPreamble b (dropLine s)

dropLine :: L.ByteString -> L.ByteString
dropLine = L.drop 2 . dropWhileS (not . L.isPrefixOf crlf)

-- | Check whether a string starts with two dashes followed by
--   the given boundary string.
isBoundary :: L.ByteString -- ^ The boundary, without the initial dashes
           -> L.ByteString
           -> Bool
isBoundary b s = startsWithDashes s && b `L.isPrefixOf` L.drop 2 s

-- | Checks whether a string starts with two dashes.
startsWithDashes :: L.ByteString -> Bool
startsWithDashes s = L.pack "--" `L.isPrefixOf` s

splitParts :: L.ByteString -> L.ByteString -> ([BodyPart], Maybe String)
splitParts boundary s =
--    | not (isBoundary boundary s) = ([], Just $ "Missing boundary: " ++ L.unpack boundary ++ "\n" ++ L.unpack s)
    case L.null s of
      True -> ([], Nothing)
      False ->
          case splitPart boundary s of
            (p, s') ->
                let (ps,e) = splitParts boundary s'
                in (p:ps, e)
{-# INLINE splitParts #-}

splitPart :: L.ByteString -> L.ByteString -> (BodyPart, L.ByteString)
splitPart boundary s =
    case splitBlank s of
      (headers, rest) ->
          case splitBoundary boundary (L.drop 4 rest) of
            (body, rest') -> (BodyPart (L.append headers crlf) body, rest')
{-# INLINE splitPart #-}


splitBlank :: L.ByteString -> (L.ByteString, L.ByteString)
splitBlank s = spanS (not . L.isPrefixOf crlfcrlf) s
{-# INLINE splitBlank #-}


splitBoundary :: L.ByteString -> L.ByteString -> (L.ByteString, L.ByteString)
splitBoundary boundary s =
    case spanS (not . L.isPrefixOf (L.pack "\r\n--" `L.append` boundary)) s of
      (x,y) -> (x, dropLine (L.drop 2 y))
{-# INLINE splitBoundary #-}

splitAtEmptyLine :: L.ByteString -> Maybe (L.ByteString, L.ByteString)
splitAtEmptyLine s =
    case splitBlank s of
      (before, after) | L.null after -> Nothing
                      | otherwise    -> Just (L.append before crlf, L.drop 4 after)
{-# INLINE splitAtEmptyLine #-}
      
-- | Split a string at the first CRLF. The CRLF is not included
--   in any of the returned strings.
splitAtCRLF :: ByteString -- ^ String to split.
            -> Maybe (ByteString,ByteString)
            -- ^  Returns 'Nothing' if there is no CRLF.
splitAtCRLF s =
    case spanS (not . L.isPrefixOf crlf) s of
      (before, after) | L.null after -> Nothing
                      | otherwise    -> Just (before, L.drop 2 after)
{-# INLINE splitAtCRLF #-}
