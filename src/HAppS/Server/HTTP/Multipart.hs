-- #hide

-----------------------------------------------------------------------------
-- |
-- Module      :  HAppS.Server.HTTP.Multipart
-- Copyright   :  (c) Peter Thiemann 2001,2002
--                (c) Bjorn Bringert 2005-2006
--                (c) Lemmih 2007
-- License     :  BSD-style
--
-- Maintainer  :  lemmih@vo.com
-- Stability   :  experimental
-- Portability :  xbnon-portable
--
-- Parsing of the multipart format from RFC2046.
-- Partly based on code from WASHMail.
--
-----------------------------------------------------------------------------
module HAppS.Server.HTTP.Multipart
    (
     -- * Multi-part messages
     MultiPart(..), BodyPart(..), Header
    , parseMultipartBody, hGetMultipartBody
     -- * Headers
    , ContentType(..), ContentTransferEncoding(..)
    , ContentDisposition(..)
    , parseContentType
    , parseContentTransferEncoding
    , parseContentDisposition
    , getContentType
    , getContentTransferEncoding
    , getContentDisposition

    , splitAtEmptyLine
    , splitAtCRLF
    ) where

import Control.Monad
import Data.Int (Int64)
import Data.Maybe
import System.IO (Handle)

import HAppS.Server.HTTP.RFC822Headers

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

--
-- * Multi-part stuff.
--

data MultiPart = MultiPart [BodyPart]
               deriving (Show, Read, Eq, Ord)

data BodyPart = BodyPart [Header] ByteString
                deriving (Show, Read, Eq, Ord)

-- | Read a multi-part message from a 'ByteString'.
parseMultipartBody :: String -- ^ Boundary
                   -> ByteString -> Maybe MultiPart
parseMultipartBody b s = 
    do
    ps <- splitParts (BS.pack b) s
    liftM MultiPart $ mapM parseBodyPart ps

-- | Read a multi-part message from a 'Handle'.
--   Fails on parse errors.
hGetMultipartBody :: String -- ^ Boundary
                  -> Handle
                  -> IO MultiPart
hGetMultipartBody b h = 
    do
    s <- BS.hGetContents h
    case parseMultipartBody b s of
        Nothing -> fail "Error parsing multi-part message"
        Just m  -> return m



parseBodyPart :: ByteString -> Maybe BodyPart
parseBodyPart s =
    do
    (hdr,bdy) <- splitAtEmptyLine s
    hs <- parseM pHeaders "<input>" (BS.unpack hdr)
    return $ BodyPart hs bdy

--
-- * Splitting into multipart parts.
--

-- | Split a multipart message into the multipart parts.
splitParts :: ByteString -- ^ The boundary, without the initial dashes
           -> ByteString 
           -> Maybe [ByteString]
splitParts b s = dropPreamble b s >>= spl
  where
  spl x = case splitAtBoundary b x of
            Nothing -> Nothing
            Just (s1,d,s2) | isClose b d -> Just [s1]
                           | otherwise -> spl s2 >>= Just . (s1:)

-- | Drop everything up to and including the first line starting 
--   with the boundary. Returns 'Nothing' if there is no 
--   line starting with a boundary.
dropPreamble :: ByteString -- ^ The boundary, without the initial dashes
             -> ByteString 
             -> Maybe ByteString
dropPreamble b s | isBoundary b s = fmap snd (splitAtCRLF s)
                 | otherwise = dropLine s >>= dropPreamble b

-- | Split a string at the first boundary line.
splitAtBoundary :: ByteString -- ^ The boundary, without the initial dashes
                -> ByteString -- ^ String to split.
                -> Maybe (ByteString,ByteString,ByteString)
                   -- ^ The part before the boundary, the boundary line,
                   --   and the part after the boundary line. The CRLF
                   --   before and the CRLF (if any) after the boundary line
                   --   are not included in any of the strings returned.
                   --   Returns 'Nothing' if there is no boundary.
splitAtBoundary b s = spl 0
  where
  spl i = case findCRLF (BS.drop i s) of
              Nothing -> Nothing
              Just (j,l) | isBoundary b s2 -> Just (s1,d,s3)
                         | otherwise -> spl (i+j+l)
                  where 
                  s1 = BS.take (i+j) s
                  s2 = BS.drop (i+j+l) s
                  (d,s3) = splitAtCRLF_ s2

-- | Check whether a string starts with two dashes followed by
--   the given boundary string.
isBoundary :: ByteString -- ^ The boundary, without the initial dashes
           -> ByteString
           -> Bool
isBoundary b s = startsWithDashes s && b `BS.isPrefixOf` BS.drop 2 s

-- | Check whether a string for which 'isBoundary' returns true
--   has two dashes after the boudary string.
isClose :: ByteString -- ^ The boundary, without the initial dashes
        -> ByteString 
        -> Bool
isClose b s = startsWithDashes (BS.drop (2+BS.length b) s)

-- | Checks whether a string starts with two dashes.
startsWithDashes :: ByteString -> Bool
startsWithDashes s = BS.pack "--" `BS.isPrefixOf` s


--
-- * RFC 2046 CRLF
--

-- | Drop everything up to and including the first CRLF.
dropLine :: ByteString -> Maybe ByteString
dropLine s = fmap snd (splitAtCRLF s)

-- | Split a string at the first empty line. The CRLF (if any) before the
--   empty line is included in the first result. The CRLF after the
--   empty line is not included in the result.
--   'Nothing' is returned if there is no empty line.
splitAtEmptyLine :: ByteString -> Maybe (ByteString, ByteString)
splitAtEmptyLine s | startsWithCRLF s = Just (BS.empty, dropCRLF s)
                   | otherwise = spl 0
  where
  spl i = case findCRLF (BS.drop i s) of
              Nothing -> Nothing
              Just (j,l) | startsWithCRLF s2 -> Just (s1, dropCRLF s2)
                         | otherwise -> spl (i+j+l)
                where (s1,s2) = BS.splitAt (i+j+l) s

-- | Split a string at the first CRLF. The CRLF is not included
--   in any of the returned strings.
splitAtCRLF :: ByteString -- ^ String to split.
            -> Maybe (ByteString,ByteString)
            -- ^  Returns 'Nothing' if there is no CRLF.
splitAtCRLF s = case findCRLF s of
                  Nothing -> Nothing
                  Just (i,l) -> Just (s1, BS.drop l s2)
                      where (s1,s2) = BS.splitAt i s

-- | Like 'splitAtCRLF', but if no CRLF is found, the first
--   result is the argument string, and the second result is empty.
splitAtCRLF_ :: ByteString -> (ByteString,ByteString)
splitAtCRLF_ s = fromMaybe (s,BS.empty) (splitAtCRLF s)

-- | Get the index and length of the first CRLF, if any.
findCRLF :: ByteString -- ^ String to split.
         -> Maybe (Int64,Int64)
findCRLF s = 
    case findCRorLF s of
              Nothing -> Nothing
              Just j | BS.null (BS.drop (j+1) s) -> Just (j,1)
              Just j -> case (BS.index s j, BS.index s (j+1)) of
                           ('\n','\r') -> Just (j,2)
                           ('\r','\n') -> Just (j,2)
                           _           -> Just (j,1)

findCRorLF :: ByteString -> Maybe Int64
findCRorLF s = BS.findIndex (\c -> c == '\n' || c == '\r') s

startsWithCRLF :: ByteString -> Bool
startsWithCRLF s = not (BS.null s) && (c == '\n' || c == '\r')
  where c = BS.index s 0

-- | Drop an initial CRLF, if any. If the string is empty, 
--   nothing is done. If the string does not start with CRLF,
--   the first character is dropped.
dropCRLF :: ByteString -> ByteString
dropCRLF s | BS.null s = BS.empty
           | BS.null (BS.drop 1 s) = BS.empty
           | c0 == '\n' && c1 == '\r' = BS.drop 2 s
           | c0 == '\r' && c1 == '\n' = BS.drop 2 s
           | otherwise = BS.drop 1 s
  where c0 = BS.index s 0
        c1 = BS.index s 1
