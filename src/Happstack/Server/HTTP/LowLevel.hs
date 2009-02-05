module Happstack.Server.HTTP.LowLevel
    (-- * HTTP Implementation
     -- $impl

     -- * Problems
     -- $problems

     -- * API
     module Happstack.Server.HTTP.Handler,
     module Happstack.Server.HTTP.Listen,
     module Happstack.Server.HTTP.Types
    ) where

import Happstack.Server.HTTP.Handler
import Happstack.Server.HTTP.Listen
import Happstack.Server.HTTP.Types

-- $impl
-- The Happstack HTTP implementation supports HTTP 1.0 and 1.1.
-- Multiple request on a connection including pipelining is supported.

-- $problems
-- Currently if a client sends an invalid HTTP request the whole
-- connection is aborted and no further processing is done.
--
-- When the connection times out Happstack closes it. In future it could
-- send a 408 response but this may be problematic if the sending
-- of a response caused the problem.
