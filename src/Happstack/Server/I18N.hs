module Happstack.Server.I18N 
     ( acceptLanguage
     , bestLanguage
     ) where

import Control.Applicative ((<$>))

import Control.Arrow ((>>>), first, second)
import Data.Function (on)
import qualified Data.ByteString.Char8 as C
import Data.List     (sortBy)
import Data.Maybe    (fromMaybe)
import Data.Text     as Text (Text, breakOnAll, pack, singleton)
import Happstack.Server.Monads (Happstack, getHeaderM)
import Happstack.Server.Internal.Compression (encodings)
import Text.ParserCombinators.Parsec (parse)

-- TODO: proper Accept-Language parser

-- | parse the 'Accept-Language' header, or [] if not found.
acceptLanguage :: (Happstack m) => m [(Text, Maybe Double)]
acceptLanguage =
    do mAcceptLanguage <- (fmap C.unpack) <$> getHeaderM "Accept-Language"
       case mAcceptLanguage of
         Nothing   -> return []
         (Just al) ->
             case parse encodings al al of
               (Left _) -> return []
               (Right encs) -> return (map (first Text.pack) encs)

-- | deconstruct the 'acceptLanguage' results a return a list of
-- languages sorted by preference in descending order.
--
-- Note: this implementation does not conform to RFC4647
--
-- Among other things, it does not handle wildcards. A proper
-- implementation needs to take a list of available languages.
bestLanguage :: [(Text, Maybe Double)] -> [Text]
bestLanguage range =
    -- is no 'q' param, set 'q' to 1.0
    map (second $ fromMaybe 1)     >>>
    -- sort in descending order
    sortBy (flip compare `on` snd) >>>
    -- remove entries with '*' or q == 0. Removing '*' entries is not
    -- technically correct, but it is the best we can do with out a
    -- list of available languages.
    filter (\(lang, q) -> lang /= (Text.singleton '*') && q > 0)  >>>
    -- lookup fallback (RFC 4647, Section 3.4)
    concatMap (explode . fst) $
    range
    where
      -- | example: "en-us-gb" -> ["en-us-gb","en-us","en"]
      explode :: Text -> [Text]
      explode lang = lang : (reverse $ map fst $ breakOnAll (singleton '-') lang)
