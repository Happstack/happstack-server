{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
-- | Filter for compressing the 'Response' body.
module Happstack.Server.Internal.Compression
    ( compressedResponseFilter
    , compressWithFilter
    , gzipFilter
    , deflateFilter
    , encodings
    ) where
import Happstack.Server.SimpleHTTP
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Codec.Compression.GZip as GZ
import qualified Codec.Compression.Zlib as Z

-- | reads the @Accept-Encoding@ header.  Then, if possible
-- will compress the response body with methods @gzip@ or @deflate@.
--
-- > main = 
-- >   simpleHTTP nullConf $ 
-- >      do str <- compressedResponseFilter
-- >         return $ toResponse ("This response compressed using: " ++ str)
compressedResponseFilter::
    (FilterMonad Response m, MonadPlus m, WebMonad Response m, ServerMonad m)
    => m String -- ^ name of the encoding chosen
compressedResponseFilter = do
    getHeaderM "Accept-Encoding" >>=
        (maybe (return "identity") installHandler)

  where
    badEncoding = "Encoding returned not in the list of known encodings"

    installHandler accept = do
      let eEncoding = bestEncoding allEncodings $ BS.unpack accept
      (coding, identityAllowed, action) <- case eEncoding of
          Left _ -> do
            setResponseCode 406
            finishWith $ toResponse ""

          Right encs@(a:_) -> return (a 
                                     , "identity" `elem` encs
                                     , fromMaybe (fail badEncoding)
                                          (lookup a allEncodingHandlers)
                                     )
          Right [] -> fail badEncoding
      action coding identityAllowed
      return coding

-- | Ignore the @Accept-Encoding@ header in the 'Request' and attempt to compress the body of the response with @gzip@.
--
-- calls 'compressWithFilter' using 'GZ.compress'.
--
-- see also: 'compressedResponseFilter'
gzipFilter::(FilterMonad Response m) => 
            String -- ^ encoding to use for Content-Encoding header
          -> Bool   -- ^ fallback to identity for SendFile
          -> m ()
gzipFilter = compressWithFilter GZ.compress

-- | Ignore the @Accept-Encoding@ header in the 'Request' and attempt compress the body of the response with zlib's
-- @deflate@ method
--
-- calls 'compressWithFilter' using 'Z.compress'.
--
-- see also: 'compressedResponseFilter'
deflateFilter::(FilterMonad Response m) => 
               String -- ^ encoding to use for Content-Encoding header
             -> Bool   -- ^ fallback to identity for SendFile
             -> m ()
deflateFilter = compressWithFilter Z.compress

-- | Ignore the @Accept-Encoding@ header in the 'Request' and attempt to compress the body of the response using the supplied compressor.
--
-- We can not compress files being transfered using 'SendFile'. If
-- @identity@ is an allowed encoding, then just return the 'Response'
-- unmodified. Otherwise we return "406 Not Acceptable".
--
-- see also: 'gzipFilter' and 'defaultFilter'
compressWithFilter :: (FilterMonad Response m) =>
                      (L.ByteString -> L.ByteString) -- ^ function to compress the body
                   -> String -- ^ encoding to use for Content-Encoding header
                   -> Bool   -- ^ fallback to identity for SendFile
                   -> m ()
compressWithFilter compressor encoding identityAllowed =
    composeFilter $ \r -> 
        case r of
          Response{} -> setHeader "Content-Encoding" encoding $ r {rsBody = compressor $ rsBody r}
          _ | identityAllowed -> r
            | otherwise       -> (toResponse "") { rsCode = 406 }

-- | based on the rules describe in rfc2616 sec. 14.3
bestEncoding :: [String] -> String -> Either String [String]
bestEncoding availableEncodings encs = do
        encList<-either (Left . show) (Right) $ parse encodings "" encs
        case acceptable encList of
            [] -> Left "no encoding found"
            a -> Right $ a
    where
        -- first intersect with the list of encodings we know how to deal with at all
        knownEncodings:: [(String,Maybe Double)] -> [(String, Maybe Double)]
        knownEncodings m = intersectBy (\x y->fst x == fst y) m (map (\x -> (x,Nothing)) availableEncodings)
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
        encOrder = reverse $ zip (reverse availableEncodings) [1..]
        m0 = maybe (0.0::Double) id
        cmp (s,mI) (t,mJ) | m0 mI == m0 mJ
            = compare (m0 $ lookup s encOrder) (m0 $ lookup t encOrder)
                          | otherwise = compare (m0 mI) (m0 mJ)


allEncodingHandlers:: (FilterMonad Response m) => [(String, String -> Bool -> m ())]
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

handlers::(FilterMonad Response m) => [String -> Bool -> m ()]
handlers =
    [gzipFilter
    ,gzipFilter
--    ,compressFilter
--    ,compressFilter
    ,deflateFilter
    , \encoding _ -> setHeaderM "Accept-Encoding" encoding
    ,const $ fail "chose * as content encoding"
    ]

-- | a parser for the Accept-Encoding header
encodings :: GenParser Char st [([Char], Maybe Double)]
encodings = ws >> (encoding1 `sepBy` try sep) >>= (\x -> ws >> eof >> return x)
    where
        ws :: GenParser Char st ()
        ws = many space >> return ()

        sep :: GenParser Char st ()
        sep = do
            ws
            _ <- char ','
            ws
        
        encoding1 :: GenParser Char st ([Char], Maybe Double)
        encoding1 = do
            encoding <- many1 (alphaNum <|> char '-') <|> string "*"
            ws
            quality<-optionMaybe qual
            return (encoding, fmap readDec' quality)

        qual :: GenParser Char st String
        qual = do
            char ';' >> ws >> char 'q' >> ws >> char '=' >> ws
            q<-float
            return q

        int :: GenParser Char st String
        int = many1 digit

        float :: GenParser Char st String
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
            _ <- char '.'
            fractionalPart<-option "" int
            return $ '.':fractionalPart
