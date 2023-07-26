module Main where

import Prelude
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PureScript.CST.Traversal (foldMapModule, defaultMonoidalVisitor)
import PureScript.CST.Types as CST
import Data.Maybe (Maybe, Maybe(Nothing), Maybe(Just))
import Effect (Effect)
import Effect.Class (liftEffect)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import PureScript.CST.Types (AppSpine(AppType), AppSpine(AppTerm), Binder, Binder(..), Declaration, Declaration(..), DoStatement, DoStatement(DoLet), DoStatement(DoDiscard), DoStatement(DoBind), DoStatement(DoError), Expr, Expr(..), Guarded, Guarded(Unconditional), Guarded(Guarded), Label, LetBinding, LetBinding(LetBindingSignature), LetBinding(LetBindingPattern), LetBinding(LetBindingName), LetBinding(LetBindingError), Name(Name), QualifiedName(QualifiedName), RecordLabeled(RecordPun), RecordLabeled(RecordField), Separated, Separated(Separated), Where(Where), Wrapped(Wrapped))
import Data.Newtype (unwrap)

type QualifiedIdent = Tuple (Maybe CST.ModuleName) CST.Ident
type UsageMap = SemigroupMap QualifiedIdent (Set CST.SourceRange)

foreign import logthis :: forall a. a -> Unit

foreign import logTwo :: forall a. String -> a -> Unit
foreign import addIndent :: Unit -> Unit
foreign import removeIndent :: Unit -> Unit

flattenPath :: Separated (Name Label) -> Array String
flattenPath (Separated {head, tail}) = [unwrap (unwrap head).name] <> (map (\(Tuple _ x) -> (unwrap (unwrap head).name)) tail)

logEx :: forall e. Expr e -> Unit
logEx = case _ of
    ExprHole i -> logthis "~ex hole"
    ExprSection t -> logthis "~ex section"
    ExprIdent (QualifiedName {name}) -> logTwo "ident" (unwrap name)
    ExprTyped _ _ _ -> logthis "~ex typed"
    ExprInfix _ _ -> logthis "~ex infix"
    ExprRecordAccessor {expr, path} -> const unit [logthis "record access", printInBlock (\_ -> const unit [logEx expr, logthis (flattenPath path)])]
    ExprDo {statements} -> let _ = addIndent unit
                               _ = logthis "--do start--"
                               _ = logDo <$> statements
                               _ = logthis "--do end--"
                               _ = removeIndent unit
                           in unit
    ExprApp e arr -> const unit [logEx e, printInBlock (\_ -> const unit $ (case _ of
                                                                 AppType _ _ -> unit
                                                                 AppTerm e -> logEx e) <$> arr)]
    ExprRecord (Wrapped {value}) ->  case value of
                                        Just x -> logthis "--record"
                                        Nothing -> logthis $ "nothing"
    ExprOpName _ -> logthis "~ex op"
    ExprConstructor _ -> logthis "~ex constructor"
    ExprBoolean _ _ -> logthis "~ex bool"
    ExprLambda _ -> logthis "~ex lamba"
    ExprIf _ -> logthis "~ex if"
    ExprCase _ -> logthis "~ex case"
    ExprLet _ -> logthis "~ex let"
    ExprDo _ -> logthis "~ex do"
    ExprAdo _ -> logthis "~ex ado"
    ExprError _ -> logthis "~ex err"
    ExprInt _ i -> logthis i
    ExprChar _ _ -> logthis "~ex char"
    ExprString _ _ -> logthis "~ex str"
    ExprNumber _ _ -> logthis "~ex num"
    ExprArray _ -> logthis "~ex arr"
    ExprParens _ -> logthis "~ex parens"
    ExprOp a bArr -> let _ = logEx a
                     in const unit $ (\(Tuple _ e) -> logEx e) <$> bArr
    ExprNegate _ _ -> logthis "~ex negate"
    ExprRecordUpdate _ _ -> logthis "~ex recudpated"

logDo :: forall e. DoStatement e -> Unit
logDo = case _ of
    DoLet _ a -> let _ = logthis "dolet"
                 in printInBlock (\_ -> const unit (logLetBinding <$> a))
    DoDiscard e -> const unit [logthis "doreturn", printInBlock (\_ -> logEx e)]
    DoBind b _ e -> const unit [logthis "dobind", printInBlock (\_ -> const unit $ logBinder b <> logthis "---" <> logEx e)]
    DoError _ -> logthis "doerror"

printInBlock :: (Unit -> Unit) -> Unit
printInBlock fn =
    let _ = addIndent unit
        _ = fn unit
        _ = removeIndent unit
    in unit

logLetBinding :: forall e. LetBinding e -> Unit
logLetBinding = case _ of
    LetBindingSignature _ -> logthis "binding signature"
    LetBindingName {name, binders, guarded} -> let _ = logthis (unwrap (unwrap name).name)
                                      in printInBlock (\_ -> let _ = (logBinder <$> binders)
                                                             in const unit (logGuarded guarded))
    LetBindingPattern _ _ _ -> logthis "binding pattern"
    LetBindingError _ -> logthis "binding error"

logDecl :: forall e. Declaration e -> Unit
logDecl = case _ of
    DeclData _ _ -> logthis "~dec data"
    DeclValue {name, binders} -> let _ = logTwo "decl val" ((unwrap name).name)
                                 in printInBlock (\_ -> const unit $ logBinder <$> binders)
    _   -> unit

logBinder :: forall e. Binder e -> Unit
logBinder = case _ of
   BinderWildcard _ -> logTwo "bind wild card" ""
   BinderVar (Name {name})-> logTwo "bind var" name
   BinderNamed (Name {name}) _ _ -> logTwo "bind named" name
   BinderConstructor (QualifiedName {name}) _ -> logTwo "bind qualified" name
   BinderRecord _ -> logthis "bind record"
   x   -> logTwo "~bind" x
--   BinderNamed ->
--   BinderConstructor ->
--   BinderBoolean ->
--   BinderChar ->
--   BinderString ->
--   BinderInt ->
--   BinderNumber ->
--   BinderArray ->
--   BinderParens ->
--   BinderTyped ->
--   BinderOp ->
--   BinderError ->

logGuarded :: forall e. Guarded e -> Unit
logGuarded = case _ of
   Unconditional _ (Where {expr, bindings}) -> printInBlock (\_ -> let _ = logEx expr
                                                                   in case bindings of
                                                                        Nothing -> unit
                                                                        Just (Tuple _ lb) -> const unit (logLetBinding <$> lb)
                                                            )
   Guarded _ -> logthis "guarded"

getExprIdents :: forall a. CST.Module a -> UsageMap
getExprIdents = foldMapModule $ defaultMonoidalVisitor
  {
--    onExpr = \x -> let _ = logEx x in case x of
--      a@(CST.ExprIdent (CST.QualifiedName ident)) ->
--        let val =
--             SemigroupMap
--                $ Map.singleton (Tuple ident."module" ident.name)
--                $ Set.singleton ident.token.range
----            _ = logthis $ Tuple ident."module" ident.name
----            _ = logthis $ show ident.token.range
--        in val
--
--      _ -> mempty
     onDecl = \x -> let _ = logDecl x
                     in mempty
--    , onBinder = (\x -> let _ = logBinder x
--                     in mempty)
--    , onBinder = const mempty

  }

main :: Effect Unit
main = launchAff_ do
    contents <- liftEffect <<< Buffer.toString UTF8 =<< readFile "./src/Test.purs"
    case parseModule contents of
        ParseSucceeded x -> do
                            let _ = getExprIdents x
                            pure unit
        _                -> pure unit