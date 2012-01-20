{-# OPTIONS -fth -fglasgow-exts -fallow-undecidable-instances #-}
module Timer where

import Happstack.State
import Happstack.Data
import Data.Typeable
import Data.Generics
import Control.Monad.State (modify)
import Control.Concurrent

newtype Timer = Timer Int deriving (Typeable)
instance Version Timer
$(deriveSerialize ''Timer)

$(deriveNewData [''Timer])

tick () = modify $ \(Timer t) -> Timer (t+1)

$(mkMethods ''Timer [ 'tick ] )

performTicks :: Proxy Timer -> IO ()
performTicks v = do update $ Tick ()
                    threadDelay (10^6) -- Once every 1 second
                    performTicks v

instance Component Timer where
    type Dependencies Timer = End
    initialValue = Timer 0
