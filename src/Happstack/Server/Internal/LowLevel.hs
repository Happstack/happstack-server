module Happstack.Server.Internal.LowLevel
    (-- * HTTP Implementation
     -- $impl

     -- * Problems
     -- $problems

     -- * API
     module Happstack.Server.Internal.Handler,
     module Happstack.Server.Internal.Listen,
     module Happstack.Server.Internal.Types
    ) where

import Happstack.Server.Internal.Handler
import Happstack.Server.Internal.Listen
import Happstack.Server.Internal.Types

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
