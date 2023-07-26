module Test where

import Prelude
import Effect (Effect)

main :: Int -> Effect Int
main a = do
    x <- getInt unit
    y <- getInt unit
    let v = {t : 10}
        a = v.t + 1
        b = v.t + 1
    pure a

getInt :: Unit -> Effect Int
getInt _ = pure 10