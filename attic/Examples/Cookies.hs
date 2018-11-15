{-# LANGUAGE OverloadedStrings #-}


module Main where

import Control.Applicative (optional)
import Control.Monad       (msum)
import Data.Maybe          (fromMaybe)
import Data.Text.Lazy      (unpack)
import Happstack.Server
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------


serve :: ServerPart Response -> IO ()
serve part =
  let
    ramQuota  = 1  * 10^6
    diskQuota = 20 * 10^6
    tmpDir    = "/tmp/"
    policy    = defaultBodyPolicy tmpDir diskQuota ramQuota (ramQuota `div` 10)
  in
    simpleHTTP (nullConf { port = 8000}) $ do
      decodeBody policy
      part



main :: IO ()
main = serve $ msum [setCookie, getCookie]


setCookie :: ServerPart Response
setCookie = do
  method POST
  newCk <- lookText "new-msg"
  addCookies
    [(,) Session (mkCookie "theCookie" $ unpack newCk)
    ]
  seeOther ("/" :: String) $ toResponse ()


getCookie :: ServerPart Response
getCookie = do
  method GET
  mMemory <- optional $ lookCookieValue "theCookie"
  let memory = fromMaybe "No saved message." mMemory
  ok $ toResponse $ viewCookie memory


--------------------------------------------------------------------------------


viewCookie :: String -> Html
viewCookie msg =
  H.html $ do
    H.head $ do
      H.title "Happstack cookies example"
      H.style (H.toHtml viewCookieCss)
    H.body $
      H.form
        ! A.method "post"
        ! A.action "/"
        $ do
            H.h3 "Happstack cookies example"
            H.p  "The message in your cookie says:"
            H.h5 (H.toHtml msg)
            H.p  "Enter new message:"
            H.div
              ! A.style "display:flex; align-items: center;"
              $ do
                H.input
                  ! A.type_ "text"
                  ! A.name  "new-msg"
                H.button "☛"
                  ! A.type_ "submit"



viewCookieCss :: String
viewCookieCss = concat
  [ "* {box-sizing:border-box; margin:0; padding:0; }"
  , "body {"
  , "  display:flex; justify-content:center; align-items:center; "
  , "  min-height:100vh; "
  , "  font-family: Arial, Helvetica, sans-serif; color: navy;"
  , "  background-image: "
  , "    radial-gradient(circle farthest-corner at  5% 50%, gold, transparent),"
  , "    radial-gradient(circle farthest-corner at 95% 50%, #f06, transparent);"
  , "} "
  , "form { "
  , "  padding: 40px 60px;"
  , "  border: 1px solid navy; border-radius: 15px;"
  , "  background-color: rgb(235,235,255);"
  , "  box-shadow: 1px 1px 5px 3px rgba(0,0,0,0.2);"
  , "  }"
  , "h3 {"
  , "  margin-bottom:30px; padding-bottom:8px; border-bottom:1px solid silver;"
  , "  font-variant: small-caps; color: navy;"
  , "  text-transform: capitalize;"
  , "}"
  , "p {"
  , "  font-size: 12px; margin-bottom: 12px;"
  , "}"
  , "h5 {"
  , "  margin-bottom: 20px; text-align: center; color: teal;"
  , "}"
  , "input {"
  , "  height: 45px; width: 350px; margin-right: 15px; padding: 10px;"
  , "  border: 1px solid navy; border-radius: 2px; "
  , "}"
  , "button {"
  , "  height: 45px; width: 45px; margin-left: 0px;"
  , "  display: flex; justify-content: center; align-items: center;"
  , "  border: 1px solid navy; border-radius: 100px;"
  , "  background: white;"
  , "  font-size: 25px;"
  , "  transition: all 2s; cursor: pointer;"
  , "}"
  , "input:focus {border-color: blue;}"
  , "button:hover {"
  , "  background-color: rgb(80,255,100); "
  , "  transform: rotate(1440deg); height: 60px; width: 60px;"
  , "  margin-left: 15px;"
  , "}"
  ]


--------------------------------------------------------------------------------
