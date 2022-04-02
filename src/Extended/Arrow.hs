module Extended.Arrow where

import Control.Arrow qualified as A



infixl 1 >>>

(>>>) :: (b -> c) -> (a -> b) ->  (a -> c)
(>>>) =  flip (A.>>>)