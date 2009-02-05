module HAppS.Server.HTTP.LowLevel
    (-- * HTTP Implementation
     -- $impl

     -- * Problems
     -- $problems

     -- * API
     module HAppS.Server.HTTP.Handler,
     module HAppS.Server.HTTP.Listen,
     module HAppS.Server.HTTP.Types
    ) where

import HAppS.Server.HTTP.Handler
import HAppS.Server.HTTP.Listen
import HAppS.Server.HTTP.Types

-- $impl
-- The HAppS HTTP implementation supports HTTP 1.0 and 1.1.
-- Multiple request on a connection including pipelining is supported.

-- $problems
-- Currently if a client sends an invalid HTTP request the whole
-- connection is aborted and no further processing is done.
--
-- When the connection times out HAppS closes it. In future it could
-- send a 408 response but this may be problematic if the sending
-- of a response caused the problem.
