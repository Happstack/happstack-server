-- this file is used to read mime-types file such as this one:
--
-- http://svn.apache.org/viewvc/httpd/httpd/branches/2.4.x/docs/conf/mime.types?view=co
--
-- and output the mimeTypes function for BuildingBlocks.hs
--
module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main =
  do args <- getArgs
     case args of
       [fp] ->
         do h <- openFile fp ReadMode
            c <- hGetContents h
            genMimeTypes fp c
       _ ->
         do putStrLn "usage: GenMimeTypes ./mime.types"
            exitFailure

genMimeTypes :: FilePath -> String -> IO ()
genMimeTypes fp c =
  do case parse (many1 parseMimeType) fp c of
       (Left e) -> error (show e)
       (Right pairs) ->
         putStrLn $ mkMimeTypes pairs

mkMimeTypes :: [(String, [String])] -> String
mkMimeTypes mts =
  let mts' = concatMap flatten mts
  in "mimeTypes = Map.fromList " ++ show ( ("gz","application/x-gzip") : ("cabal","text/x-cabal") : mts')
     where
       flatten :: (String, [String]) -> [(String, String)]
       flatten (mt, exts) = [ (e, mt) | e <- exts ]

pComment :: Parser ()
pComment =
  do char '#'
     manyTill anyChar endOfLine
     pure ()

pMimeType :: Parser String
pMimeType = many1 (letter <|> digit <|> oneOf "/-+._")

pExts :: Parser [String]
pExts =
  sepBy1 pExt (char ' ') <* endOfLine

pExt  :: Parser String
pExt =
  many1 (letter <|> digit <|> oneOf "_-")

-- | this parser is completely adhoc. I added things until the file appeared to parse.
-- do not be afraid to modify it further if the parser starts failing.
parseMimeType :: Parser (String, [String])
parseMimeType =
  do many pComment
     mt <- pMimeType
     spaces
     exts <- pExts
     pure (mt,exts)

