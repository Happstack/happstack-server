-- #hide

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.RFC822Headers
-- Copyright   :  (c) Peter Thiemann 2001,2002
--                (c) Bjorn Bringert 2005-2006
--                (c) Lemmih 2007
-- License     :  BSD-style
--
-- Maintainer  :  lemmih@vo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Parsing of RFC822-style headers (name, value pairs)
-- Partly based on code from WASHMail.
--
-----------------------------------------------------------------------------
module Happstack.Server.Internal.RFC822Headers
    ( -- * Headers
      Header, 
      pHeader,
      pHeaders,
      parseHeaders,

      -- * Content-type
      ContentType(..), 
      getContentType,
      parseContentType,
      showContentType,

      -- * Content-transfer-encoding
      ContentTransferEncoding(..),
      getContentTransferEncoding,
      parseContentTransferEncoding,

      -- * Content-disposition
      ContentDisposition(..),
      getContentDisposition,                           
      parseContentDisposition,
                              
      -- * Utilities
      parseM
      ) where

import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

type Header = (String, String)

pHeaders :: Parser [Header]
pHeaders = many pHeader

parseHeaders :: Monad m => SourceName -> String -> m [Header]
parseHeaders = parseM pHeaders

pHeader :: Parser Header
pHeader = 
    do name <- many1 headerNameChar
       char ':'
       many ws1
       line <- lineString
       crLf
       extraLines <- many extraFieldLine
       return (map toLower name, concat (line:extraLines))

extraFieldLine :: Parser String
extraFieldLine = 
    do sp <- ws1
       line <- lineString
       crLf
       return (sp:line)

--
-- * Parameters (for Content-type etc.)
--

showParameters :: [(String,String)] -> String
showParameters = concatMap f
    where f (n,v) = "; " ++ n ++ "=\"" ++ concatMap esc v ++ "\""
          esc '\\' = "\\\\"
          esc '"'  = "\\\""
          esc c | c `elem` ['\\','"'] = '\\':[c]
                | otherwise = [c]

p_parameter :: Parser (String,String)
p_parameter =
  do lexeme $ char ';'
     p_name <- lexeme $ p_token
     lexeme $ char '='
     -- Workaround for seemingly standardized web browser bug
     -- where nothing is escaped in the filename parameter
     -- of the content-disposition header in multipart/form-data
     let litStr = if p_name == "filename" 
                   then buggyLiteralString
                   else literalString
     p_value <- litStr <|> p_token
     return (map toLower p_name, p_value)


-- 
-- * Content type
--

-- | A MIME media type value.
--   The 'Show' instance is derived automatically.
--   Use 'showContentType' to obtain the standard
--   string representation.
--   See <http://www.ietf.org/rfc/rfc2046.txt> for more
--   information about MIME media types.
data ContentType = 
	ContentType {
                     -- | The top-level media type, the general type
                     --   of the data. Common examples are
                     --   \"text\", \"image\", \"audio\", \"video\",
                     --   \"multipart\", and \"application\".
                     ctType :: String,
                     -- | The media subtype, the specific data format.
                     --   Examples include \"plain\", \"html\",
                     --   \"jpeg\", \"form-data\", etc.
                     ctSubtype :: String,
                     -- | Media type parameters. On common example is
                     --   the charset parameter for the \"text\" 
                     --   top-level type, e.g. @(\"charset\",\"ISO-8859-1\")@.
                     ctParameters :: [(String, String)]
                    }
    deriving (Show, Read, Eq, Ord)

-- | Produce the standard string representation of a content-type,
--   e.g. \"text\/html; charset=ISO-8859-1\".
showContentType :: ContentType -> String
showContentType (ContentType x y ps) = x ++ "/" ++ y ++ showParameters ps

pContentType :: Parser ContentType
pContentType = 
  do many ws1
     c_type <- p_token
     lexeme $ char '/'
     c_subtype <- lexeme $ p_token
     c_parameters <- many p_parameter
     return $ ContentType (map toLower c_type) (map toLower c_subtype) c_parameters

-- | Parse the standard representation of a content-type.
--   If the input cannot be parsed, this function calls
--   'fail' with a (hopefully) informative error message.
parseContentType :: Monad m => String -> m ContentType
parseContentType = parseM pContentType "Content-type"

getContentType :: Monad m => [Header] -> m ContentType
getContentType hs = lookupM "content-type" hs >>= parseContentType

--
-- * Content transfer encoding
--

data ContentTransferEncoding =
	ContentTransferEncoding String
    deriving (Show, Read, Eq, Ord)

pContentTransferEncoding :: Parser ContentTransferEncoding
pContentTransferEncoding =
  do many ws1
     c_cte <- p_token
     return $ ContentTransferEncoding (map toLower c_cte)

parseContentTransferEncoding :: Monad m => String -> m ContentTransferEncoding
parseContentTransferEncoding = 
    parseM pContentTransferEncoding "Content-transfer-encoding"

getContentTransferEncoding :: Monad m => [Header] -> m ContentTransferEncoding
getContentTransferEncoding hs = 
    lookupM "content-transfer-encoding" hs >>= parseContentTransferEncoding

--
-- * Content disposition
--

data ContentDisposition =
	ContentDisposition String [(String, String)]
    deriving (Show, Read, Eq, Ord)

pContentDisposition :: Parser ContentDisposition
pContentDisposition =
  do many ws1
     c_cd <- p_token
     c_parameters <- many p_parameter
     return $ ContentDisposition (map toLower c_cd) c_parameters

parseContentDisposition :: Monad m => String -> m ContentDisposition
parseContentDisposition = parseM pContentDisposition "Content-disposition"

getContentDisposition :: Monad m => [Header] -> m ContentDisposition
getContentDisposition hs = 
    lookupM "content-disposition" hs  >>= parseContentDisposition

--
-- * Utilities
--

parseM :: Monad m => Parser a -> SourceName -> String -> m a
parseM p n inp =
  case parse p n inp of
    Left e -> fail (show e)
    Right x -> return x

lookupM :: (Monad m, Eq a, Show a) => a -> [(a,b)] -> m b
lookupM n = maybe (fail ("No such field: " ++ show n)) return . lookup n

-- 
-- * Parsing utilities
--

-- | RFC 822 LWSP-char
ws1 :: Parser Char
ws1 = oneOf " \t"

lexeme :: Parser a -> Parser a
lexeme p = do x <- p; many ws1; return x

-- | RFC 822 CRLF (but more permissive)
crLf :: Parser String
crLf = try (string "\n\r" <|> string "\r\n") <|> string "\n" <|> string "\r"

-- | One line
lineString :: Parser String
lineString = many (noneOf "\n\r")

literalString :: Parser String
literalString = do char '\"'
		   str <- many (noneOf "\"\\" <|> quoted_pair)
		   char '\"'
		   return str

-- No web browsers seem to implement RFC 2046 correctly,
-- since they do not escape double quotes and backslashes
-- in the filename parameter in multipart/form-data.
--
-- Note that this eats everything until the last double quote on the line.
buggyLiteralString :: Parser String
buggyLiteralString = 
    do char '\"'
       str <- manyTill anyChar (try lastQuote)
       return str
  where lastQuote = do char '\"' 
                       notFollowedBy (try (many (noneOf "\"") >> char '\"'))

headerNameChar :: Parser Char
headerNameChar = noneOf "\n\r:"

especials, tokenchar :: [Char]
especials = "()<>@,;:\\\"/[]?.="
tokenchar = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" \\ especials

p_token :: Parser String
p_token = many1 (oneOf tokenchar)

text_chars :: [Char]
text_chars = map chr ([1..9] ++ [11,12] ++ [14..127])

p_text :: Parser Char
p_text = oneOf text_chars

quoted_pair :: Parser Char
quoted_pair = do char '\\'
		 p_text
