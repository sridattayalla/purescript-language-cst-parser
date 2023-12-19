module Utils where

import Prelude
import PureScript.CST.Types (Label, Label(Label), Name(Name), Separated, Separated(Separated), Token(TokString), Token(TokRawString), Token(TokComma))
import Data.Tuple (Tuple(Tuple))
import Data.Array (cons, filter, uncons, unsnoc)
import Data.Maybe (Maybe, Maybe(Just))

foreign import logthis :: forall a. a -> Unit
foreign import logTwo :: forall a. String -> a -> Unit
foreign import addIndent :: Unit -> Unit
foreign import removeIndent :: Unit -> Unit
foreign import join :: String -> Array String -> String

combine :: forall a. Separated a -> Array a
combine (Separated {head, tail}) = (cons head ((\(Tuple _ x) -> x) <$> tail))

separate :: forall a. Array a -> Maybe (Separated a)
separate arr = (\{head, tail} -> Separated {head, tail: (\x -> Tuple dummySourceToken x ) <$> tail}) <$> uncons arr

filterSeparate :: forall a. Maybe (Separated a) -> (a -> Boolean) -> Maybe (Separated a)
filterSeparate val fn =
    (\(Separated {head, tail}) ->
        let filteredTail = filter (\(Tuple _ a) -> fn a) tail
        in if fn head
             then Just $ Separated {head, tail: filteredTail}
             else (\{head: (Tuple _ a), tail} -> Separated {head: a, tail})
                    <$> uncons filteredTail

        )
    =<< val

fromName :: forall a. Name a -> a
fromName (Name {name}) = name

fromLabel :: Label -> String
fromLabel (Label x) = x

dummyPos = {line : 0, column: 0}
dummySourceToken = {range : {start : dummyPos, end: dummyPos}, leadingComments: [], trailingComments: [], value: TokComma}
