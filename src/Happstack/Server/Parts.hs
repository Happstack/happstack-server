{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Happstack.Server.Parts(
    compressedResponseFilter
   ,gzipFilter
   ,deflateFilter
   ,encodings
) where
import Happstack.Server.SimpleHTTP
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Codec.Compression.GZip as GZ
import qualified Codec.Compression.Zlib as Z

-- | reads the \"Accept-Encoding\" header.  Then, if possible
-- will compress the response body with methods "gzip" or "deflate"
--
-- Returns the name of the coding chosen
compressedResponseFilter::
    (FilterMonad Response m, MonadPlus m, WebMonad Response m, ServerMonad m)
    => m String
compressedResponseFilter = do
    getHeaderM "Accept-Encoding" >>=
        (maybe (return "identity") installHandler)

  where
    badEncoding = "Encoding returned not in the list of known encodings"

    installHandler accept = do
      let eEncoding = bestEncoding $ BS.unpack accept
      (coding,action) <- case eEncoding of
          Left _ -> do
            setResponseCode 406
            finishWith $ toResponse ""

          Right a -> return (a, fromMaybe (fail badEncoding)
                                          (lookup a allEncodingHandlers))

      setHeaderM "Content-Encoding" coding
      action
      return coding


-- | compresses the body of the response with gzip.
-- does not set any headers.
gzipFilter::(FilterMonad Response m) => m()
gzipFilter = do
    composeFilter (\r -> r{rsBody = GZ.compress $ rsBody r})

-- | compresses the body of the response with zlib's
-- deflate method
-- does not set any headers.
deflateFilter::(FilterMonad Response m) => m()
deflateFilter = do
    composeFilter (\r -> r{rsBody = Z.compress $ rsBody r})

-- | based on the rules describe in rfc2616 sec. 14.3
bestEncoding :: String -> Either String String
bestEncoding encs = do
        encList<-either (Left . show) (Right) $ parse encodings "" encs
        case acceptable encList of
            [] -> Left "no encoding found"
            a -> Right $ head a
    where
        -- first intersect with the list of encodings we know how to deal with at all
        knownEncodings:: [(String,Maybe Double)] -> [(String, Maybe Double)]
        knownEncodings m = intersectBy (\x y->fst x == fst y) m (map (\x -> (x,Nothing)) allEncodings)
        -- this expands the wildcard, by figuring out if we need to include "identity" in the list
        -- Then it deletes the wildcard entry, drops all the "q=0" entries (which aren't allowed).
        --
        -- note this implementation is a little conservative.  if someone were to specify "*"
        -- without a "q" value, it would be this server is willing to accept any format at all.
        -- We pretty much assume we can't send them /any/ format and that they really
        -- meant just "identity" this seems safe to me.
        knownEncodings':: [(String,Maybe Double)] -> [(String, Maybe Double)]
        knownEncodings' m = filter dropZero $ deleteBy (\(a,_) (b,_)->a==b) ("*",Nothing) $
            case lookup "*" (knownEncodings m) of
                Nothing -> addIdent $ knownEncodings m
                Just (Just a) | a>0 -> addIdent $ knownEncodings m
                              | otherwise -> knownEncodings m
                Just (Nothing) -> addIdent $ knownEncodings m
        dropZero (_, Just a) | a==0 = False
                          | otherwise = True
        dropZero (_, Nothing) = True
        addIdent:: [(String,Maybe Double)] -> [(String, Maybe Double)]
        addIdent m = if isNothing $ lookup "identity" m
            then m ++ [("identity",Nothing)]
            else m
        -- finally we sort the list of available encodings.
        acceptable:: [(String,Maybe Double)] -> [String]
        acceptable l = map fst $ sortBy (flip cmp) $  knownEncodings'  l
        -- let the client choose but break ties with gzip
        encOrder = reverse $ zip (reverse allEncodings) [1..]
        m0 = maybe (0.0::Double) id
        cmp (s,mI) (t,mJ) | m0 mI == m0 mJ
            = compare (m0 $ lookup s encOrder) (m0 $ lookup t encOrder)
                          | otherwise = compare (m0 mI) (m0 mJ)


allEncodingHandlers:: (FilterMonad Response m) => [(String, m ())]
allEncodingHandlers = zip allEncodings handlers

allEncodings :: [String]
allEncodings =
    ["gzip"
    ,"x-gzip"
--    ,"compress" -- as far as I can tell there is no haskell library that supports this
--    ,"x-compress" -- as far as I can tell, there is no haskell library that supports this
    ,"deflate"
    ,"identity"
    ,"*"
    ]

handlers::(FilterMonad Response m) => [m ()]
handlers =
    [gzipFilter
    ,gzipFilter
--    ,compressFilter
--    ,compressFilter
    ,deflateFilter
    ,return ()
    ,fail "chose * as content encoding"
    ]

-- | unsupported:  a parser for the Accept-Encoding header
encodings :: GenParser Char st [([Char], Maybe Double)]
encodings = ws >> (encoding1 `sepBy` try sep) >>= (\x -> ws >> eof >> return x)
    where
        ws = many space
        sep = do
            ws
            char ','
            ws
        
        encoding1 :: GenParser Char st ([Char], Maybe Double)
        encoding1 = do
            encoding <- many1 alphaNum <|> string "*"
            ws
            quality<-optionMaybe qual
            return (encoding, fmap read quality)
        qual = do
            char ';' >> ws >> char 'q' >> ws >> char '=' >> ws
            q<-float
            return q
        int = many1 digit
        float = do
                wholePart<-many1 digit
                fractionalPart<-option "" fraction
                return $ wholePart ++ fractionalPart
            <|>
                do
                fractionalPart<-fraction
                return fractionalPart
        fraction :: GenParser Char st String
        fraction = do
            char '.'
            fractionalPart<-option "" int
            return $ '.':fractionalPart


