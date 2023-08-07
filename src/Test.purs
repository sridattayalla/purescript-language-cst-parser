module Test where

import Prelude
import Effect (Effect)
import Data.Tuple (Tuple(Tuple))

--main :: Int -> Effect Int
--main a = do
--    x <- getInt unit
--    y <- getInt unit
--    let v = {t : 10}
--        a = v.t + 1
--        b = v.t + 1
--    pure a
--
--getInt :: Unit -> Effect Int
--getInt _ = pure 10

main :: Int -> Int
main a = let
    b = a + 1
    c = b + 2
    _ = 123
    d = let b = 10
        in b + 3
    in b + 3



fn :: Int -> Int
fn x =
    let Tuple a (Tuple b c) = defTuple
        details = {name : "Aurthor Morgan", city: "BlackWaters", age: 37}
        {name, city} = details

    in x

defTuple = Tuple 1 (Tuple 2 3)

l = gn {t: 10, y : 11}

gn x = x.t

data SampleType = Works | NotWorking String

instance showSampleType :: Show SampleType where show _ = ""
