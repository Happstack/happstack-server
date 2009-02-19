{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Happstack.Server.Parts(
    contentEncodingFilter
   ,gzipFilter
   ,compressFilter
) where
import Happstack.Server.SimpleHTTP
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Codec.Compression.GZip as GZ
import qualified Codec.Compression.Zlib as Z

contentEncodingFilter::
    (FilterMonad Response m, MonadPlus m, WebMonad Response m, ServerMonad m)
    => b -> m b
contentEncodingFilter r = do
    mbAccept<-getHeaderM "Accept-Encoding"
    accept<- maybe mzero return mbAccept
    let eEncoding = bestEncoding $ BS.unpack accept
    (coding,action) <- case eEncoding of
        Left _ -> do
            setResponseCode 406
            finishWith $ toResponse ""
        Right a -> return (a, maybe (fail "Encoding returned not in the list of known encodings")
            id (lookup a allEncodingHandlers))
    setHeaderM "Content-Encoding" coding
    action
    return r

gzipFilter::(FilterMonad Response m) => m()
gzipFilter = do
    composeFilter (\r -> r{rsBody = GZ.compress $ rsBody r})

compressFilter::(FilterMonad Response m) => m()
compressFilter = do
    composeFilter (\r -> r{rsBody = Z.compress $ rsBody r})

-- | based on the rules describe in rfc2616 sec. 14.3
bestEncoding :: String -> Either String String
bestEncoding encs = do
        encList<-either (Left . show) (Right) $ parse encodings "" encs
        case acceptable encList of
            [] -> Left "no encoding found"
            a -> Right $ head a
    where
        knownEncodings:: [(String,Maybe Double)] -> [(String, Maybe Double)]
        knownEncodings m = intersectBy (\x y->fst x == fst y) m (map (\x -> (x,Nothing)) allEncodings)
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
    ,"compress"
    ,"x-compress"
    ,"identity"
    ,"*"
    ]

handlers::(FilterMonad Response m) => [m ()]
handlers =
    [gzipFilter
    ,gzipFilter
    ,compressFilter
    ,compressFilter
    ,return ()
    ,fail $ "chose * as content encoding"
    ]

encodings :: GenParser Char st [([Char], Maybe Double)]
encodings = (sepBy encoding1 sep) >>= (\x -> eof >> return x)
    where
        sep = do
            char ','
            many $ char ' '
        encoding1 :: GenParser Char st ([Char], Maybe Double)
        encoding1 = do
            encoding <- many1 letter <|> string "*"
            quality<-optionMaybe qual
            return (encoding, fmap read quality)
        qual = do
            string ";q="
            q<-float
            return $ q
        int = many1 digit
        float = do
                wholePart<-many1 digit
                fractionalPart<-option "" fraction
                return $ wholePart ++ fractionalPart
            <|>
                do
                fractionalPart<-fraction
                return $ fractionalPart
        fraction :: GenParser Char st String
        fraction = do
            char '.'
            fractionalPart<-option "" int
            return $ '.':fractionalPart


