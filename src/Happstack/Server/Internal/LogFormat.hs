module Happstack.Server.Internal.LogFormat
  ( formatTimeCombined
  , formatRequestCombined
  , logNotice
  , logWarn
  , logDebug
  , logError
  , __LOC__
  ) where

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (FormatTime(..), formatTime, defaultTimeLocale)
#else
import Data.Time.Format (FormatTime(..), formatTime)
import System.Locale    (defaultTimeLocale)
#endif
import Data.Text (pack, Text, unpack)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Syntax (Loc(loc_module), location, Q, Exp)

#if defined(MIN_VERSION_hslogger)
import System.Log.Logger (Priority(..), logM)

logNotice :: Loc -> Text -> IO ()
logNotice loc = logM (loc_module loc) NOTICE . unpack
logWarn :: Loc -> Text -> IO ()
logWarn loc = logM (loc_module loc) WARNING . unpack
logDebug :: Loc -> Text -> IO ()
logDebug loc = logM (loc_module loc) DEBUG . unpack
logError :: Loc -> Text -> IO ()
logError loc = logM (loc_module loc) ERROR . unpack
#else
import Control.Logging (logS, warnS, debugS', loggingLogger, LogLevel(LevelError), flushLog)

-- I decided the "info" and "warning" level logging don't need flush
-- immediately, but debug and error do.
logNotice :: Loc -> Text -> IO ()
logNotice loc = logS (pack (loc_module loc))
logWarn :: Loc -> Text -> IO ()
logWarn loc = warnS (pack (loc_module loc))
logDebug :: Loc -> Text -> IO ()
logDebug loc = debugS' (pack (loc_module loc))
logError :: Loc -> Text -> IO ()
logError loc msg = loggingLogger LevelError (pack (loc_module loc)) msg >> flushLog
#endif

__LOC__ :: Q Exp
__LOC__ = lift =<< location

-- | Format the time as describe in the Apache combined log format.
--   http://httpd.apache.org/docs/2.2/logs.html#combined
--
-- The format is:
--   [day/month/year:hour:minute:second zone]
--    day = 2*digit
--    month = 3*letter
--    year = 4*digit
--    hour = 2*digit
--    minute = 2*digit
--    second = 2*digit
--    zone = (`+' | `-') 4*digit
formatTimeCombined :: FormatTime t => t -> String
formatTimeCombined = formatTime defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z"

-- | Format the request as describe in the Apache combined log format.
--   http://httpd.apache.org/docs/2.2/logs.html#combined
--
-- The format is: "%h - %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-agent}i\""
-- %h:            This is the IP address of the client (remote host) which made the request to the server.
-- %u:            This is the userid of the person requesting the document as determined by HTTP authentication.
-- %t:            The time that the request was received.
-- %r:            The request line from the client is given in double quotes.
-- %>s:           This is the status code that the server sends back to the client.
-- %b:            The last part indicates the size of the object returned to the client, not including the response headers.
-- %{Referer}:    The "Referer" (sic) HTTP request header.
-- %{User-agent}: The User-Agent HTTP request header.
formatRequestCombined :: FormatTime t =>
  String
  -> String
  -> t
  -> String
  -> Int
  -> Integer
  -> String
  -> String
  -> String
formatRequestCombined host user time requestLine responseCode size referer userAgent =
  unwords
    [ host
    , user
    , "[" ++ formattedTime ++ "]"
    , show requestLine
    , show responseCode
    , show size
    , show referer
    , show userAgent
    ]
  where formattedTime = formatTimeCombined time
