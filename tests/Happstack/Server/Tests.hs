-- |HUnit tests and QuickQuick properties for Happstack.Server.*
module Happstack.Server.Tests (allTests) where

import Test.HUnit as HU (Test(..),(~:),(~?),(@?=))
import Happstack.Server.Cookie
import Happstack.Server.Parts
import Text.ParserCombinators.Parsec

-- |All of the tests for happstack-util should be listed here. 
allTests :: Test
allTests = 
    "happstack-server tests" ~: [cookieParserTest, acceptEncodingParserTest]

cookieParserTest :: Test
cookieParserTest = 
    "cookieParserTest" ~:
    [parseCookies "$Version=1;Cookie1=value1;$Path=\"/testpath\";$Domain=example.com;cookie2=value2"
        @?= (Right [
            Cookie "1" "/testpath" "example.com" "cookie1" "value1"
          , Cookie "1" "" "" "cookie2" "value2"
          ])
    ,parseCookies "  \t $Version = \"1\" ; cookie1 = \"randomcrap!@#%^&*()-_+={}[]:;'<>,.?/\\|\" , $Path=/  "
        @?= (Right [
            Cookie "1" "/" "" "cookie1" "randomcrap!@#%^&*()-_+={}[]:;'<>,.?/\\|"
          ])
    ,parseCookies " cookie1 = value1  "
        @?= (Right [
            Cookie "" "" "" "cookie1" "value1"
          ])
    ,parseCookies " $Version=\"1\";buggygooglecookie = valuewith=whereitshouldnotbe  "
        @?= (Right [
            Cookie "1" "" "" "buggygooglecookie" "valuewith=whereitshouldnotbe"
          ])
    ]

acceptEncodingParserTest :: Test
acceptEncodingParserTest =
    "acceptEncodingParserTest" ~:
    [either (Left . show) Right (parse encodings "" " gzip;q=1,*, compress ; q = 0.5 ")
        @?= (Right [("gzip", Just 1),("*", Nothing),("compress", Just 0.5)])
    ]
