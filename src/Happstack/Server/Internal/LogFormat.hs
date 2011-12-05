module Happstack.Server.Internal.LogFormat
  ( formatTimeCombined
  , formatRequestCombined
  ) where

import System.Locale (defaultTimeLocale)
import Data.Time.Format (FormatTime(..), formatTime)

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
