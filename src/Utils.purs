module Utils where

import Prelude

foreign import logthis :: forall a. a -> Unit
foreign import logTwo :: forall a. String -> a -> Unit
foreign import addIndent :: Unit -> Unit
foreign import removeIndent :: Unit -> Unit
