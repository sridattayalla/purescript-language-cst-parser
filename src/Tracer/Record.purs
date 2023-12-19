module Tracer.Record where

import Prelude

import Data.Array (cons, elem, filter, foldl, mapMaybe, notElem)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Tuple (Tuple(Tuple))
import PureScript.CST.Types (Expr(ExprIdent, ExprRecordAccessor, ExprRecord), Ident(Ident), Label(Label), Name(Name), QualifiedName(QualifiedName), RecordLabeled(RecordField, RecordPun), Separated, Wrapped(Wrapped))
import Utils (combine, filterSeparate, fromLabel, fromName, join, logTwo, logthis, separate)
import PureScript.CST.Types (Expr) as CST

fetchDirectAssignment :: forall a. CST.Expr a -> String -> Array { a :: String, b :: String }
fetchDirectAssignment x source = case x of
  ExprRecord (Wrapped { value: Nothing }) ->
    let
      _ = logthis "empty record"
    in
      []
  ExprRecord (Wrapped { value: Just val }) -> traverseRecord val source
  _ ->
    let
      _ = logthis "error in traverseRecord function, input is not record"
    in
      []

removeAssignments :: forall a. CST.Expr a -> Array String -> CST.Expr a
removeAssignments x arr = case x of
  ExprRecord (Wrapped w@{ value: Just val}) ->
    let filteredRec = removeFields arr val
    in ExprRecord (Wrapped $ w {value = filteredRec})
  _ -> x

traverseRecord :: forall a. Separated (RecordLabeled (Expr a)) -> String -> Array { a :: String, b :: String }
traverseRecord seperated source =
  let
    unfiltered =
      ( case _ of
          RecordPun (Name { name: Ident val }) -> Tuple val $ if val == source then Just val else Nothing
          RecordField (Name { name: Label val }) _ expr -> Tuple val (findRecordAccessPath expr source)
      ) <$> (combine seperated)
  in
    mapMaybe (\(Tuple x y) -> (\val -> { a: x, b: val }) <$> y) unfiltered

findRecordAccessPath :: forall a. Expr a -> String -> Maybe String
findRecordAccessPath expr record =
  case expr of
    ExprRecordAccessor { expr, path } ->
      case expr of
        ExprIdent (QualifiedName { name: Ident ident }) -> if record == ident then Just (join "." $ (fromLabel <<< fromName) <$> combine path) else Nothing
        _ -> Nothing
    _ -> Nothing

removeFields :: forall a. Array String -> Separated (RecordLabeled (Expr a)) -> Maybe (Separated (RecordLabeled (Expr a)))
removeFields arr seperated =
  filterSeparate
    (Just seperated)
    case _ of
        RecordPun (Name { name: Ident val }) -> notElem val arr
        RecordField (Name { name: Label val }) _ expr -> notElem val arr

