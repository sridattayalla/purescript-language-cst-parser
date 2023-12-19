module Tracer.Record where

import Prelude
import PureScript.CST.Types (Expr, Module) as CST
import Control.Category (identity)
import PureScript.CST.Traversal (defaultVisitor, rewriteExprTopDown, rewriteModuleTopDown)
import PureScript.CST.Types (Expr(ExprRecord), Ident(Ident), Label(Label), Name(Name), RecordLabeled(RecordField), RecordLabeled(RecordPun), Separated(Separated), Wrapped(Wrapped))
import Data.Array (cons)
import Utils (logthis)
import Data.Maybe (Maybe(Nothing), Maybe(Just))
import Data.Tuple (Tuple(Tuple))

checkDirectAssignment :: forall a. CST.Expr a -> CST.Expr a
checkDirectAssignment = rewriteExprTopDown $
  { onExpr : (\x ->
                let _ = traverseRecord x
                in x
                )
  , onBinder : identity
  , onDecl : identity
  , onType : identity
  }

traverseRecord :: forall a. CST.Expr a -> Unit
traverseRecord = case _ of
    ExprRecord (Wrapped {value : Just (Separated {head, tail})}) ->
        let _ =
                (case _ of
                    RecordPun (Name {name: Ident val}) -> logthis val
                    RecordField (Name {name: Label val}) _ _ -> logthis val
                    ) <$> (cons head ((\(Tuple _ x) -> x ) <$> tail))
        in unit
    ExprRecord (Wrapped {value : Nothing}) -> logthis "empty record"
    _ -> logthis "error in traverseRecord function, input is not record"

