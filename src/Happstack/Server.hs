{- |
Module      :  Happstack.Server
Copyright   :  (c) Happstack.com 2010; (c) HAppS Inc 2007
License     :  BSD3

Maintainer  :  Happstack team <happs@googlegroups.com>
Stability   :  provisional
Portability :  GHC-only, Windows, Linux, FreeBSD, OS X

Happstack.Server provides a self-contained HTTP server and a rich collection of types and functions for routing Requests, generating Responses, working with query parameters, form data, and cookies, serving files and more.

A very simple, \"Hello World!\" web app looks like:
 
> import Happstack.Server
> main = simpleHTTP nullConf $ ok "Hello World!"

By default the server will listen on port 8000. Run the app and point your browser at: <http://localhost:8000/>

At the core of the Happstack server we have the 'simpleHTTP' function which starts the HTTP server:

> simpleHTTP :: ToMessage a => Conf -> ServerPart a -> IO ()

and we have the user supplied 'ServerPart' (also known as,
'ServerPartT' 'IO'), which generates a 'Response' for each incoming
'Request'.

A trivial HTTP app server might just take a user supplied function like:

> myApp :: Request -> IO Response

For each incoming 'Request' the server would fork a new thread, run
@myApp@ to generate a 'Response', and then send the 'Response' back to
the client. But, that would be a pretty barren wasteland to work in.

The model for 'ServerPart' is essential the same, except we use the
much richer 'ServerPart' monad instead of the 'IO' monad. 

For in-depth documentation and runnable examples I highly recommend The Happstack Crash Course <http://happstack.com/docs/crashcourse/index.html>.

-}
module Happstack.Server 
    ( -- * HTTP Server
      module Happstack.Server.SimpleHTTP
    -- * Request Routing
    , module Happstack.Server.Routing
    -- * Creating Responses
    , module Happstack.Server.Response
    -- * Looking up values in Query String, Request Body, and Cookies
    , module Happstack.Server.RqData
    -- * Create and Set Cookies (see also "Happstack.Server.RqData")
    , module Happstack.Server.Cookie
    -- * File Serving
    , module Happstack.Server.FileServe
    -- * HTTP Realm Authentication
    , module Happstack.Server.Auth
    -- * Error Handling
    , module Happstack.Server.Error
    -- * Web-related Monads
    , module Happstack.Server.Monads
    -- * Proxying
    , module Happstack.Server.Proxy
    -- * Output Validation
    , module Happstack.Server.Validation
    -- * HTTP Types
    , module Happstack.Server.Types
    -- * Other
    , module Happstack.Server.Client
--    , module Happstack.Server.Internal.Monads
    )
    where

import Happstack.Server.Client
import Happstack.Server.SimpleHTTP       (simpleHTTP
                                         , simpleHTTP'
                                         , simpleHTTP''
                                         , simpleHTTPWithSocket
                                         , simpleHTTPWithSocket'
                                         , bindPort
                                         , bindIPv4
                                         , parseConfig)
import Happstack.Server.FileServe
import Happstack.Server.Monads
import Happstack.Server.Auth
import Happstack.Server.Cookie
import Happstack.Server.Error
import Happstack.Server.Response
import Happstack.Server.Routing
import Happstack.Server.Proxy
import Happstack.Server.RqData
import Happstack.Server.Validation
import Happstack.Server.Types
-- import Happstack.Server.Internal.Monads