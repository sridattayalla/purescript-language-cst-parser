module Main where

import Prelude
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PureScript.CST.Traversal (defaultMonoidalVisitor, defaultVisitor, foldMapModule, rewriteModuleTopDown)
import PureScript.CST.Types as CST
import Data.Maybe (Maybe, Maybe(Nothing), Maybe(Just), isJust)
import Effect (Effect)
import Effect.Class (liftEffect)
import PureScript.CST (RecoveredParserResult(..), parseExpr, parseModule, printModule)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readdir)
import Effect.Aff (Aff, launchAff_)
import PureScript.CST.Types (AppSpine(AppType), AppSpine(AppTerm), Binder, Binder(..), Comment(Space), Comment(Line), Declaration, Declaration(..), DoStatement, DoStatement(DoLet), DoStatement(DoDiscard), DoStatement(DoBind), DoStatement(DoError), Expr, Expr(..), Guarded, Guarded(Unconditional), Guarded(Guarded), Ident, Ident(Ident), Instance, Instance(Instance), InstanceBinding, InstanceBinding(InstanceBindingName), Label, Labeled(Labeled), LetBinding, LetBinding(LetBindingSignature), LetBinding(LetBindingPattern), LetBinding(LetBindingName), LetBinding(LetBindingError), LineFeed(LF), Module, Name(Name), Proper(Proper), QualifiedName(QualifiedName), RecordLabeled(RecordPun), RecordLabeled(RecordField), Separated, Separated(Separated), Token(TokUnderscore), Token(TokSymbolName), Token(TokUpperName), Token(TokLowerName), Where(Where), Wrapped(Wrapped))
import Data.Newtype (unwrap)
import Data.Array.NonEmpty.Internal (NonEmptyArray, NonEmptyArray(NonEmptyArray))
import Data.Array as Arr
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Control.Category (identity)
import Data.NonEmpty (singleton)
import Data.Array.NonEmpty (any, findMap, snoc)
import Effect.Console (log)
import Tracer.Record (checkDirectAssignment) as RecordTracer
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Print (printSourceToken) as Print
import PureScript.CST.Range.TokenList as TokenList
import Data.Foldable (foldMap)
import Utils (logthis)

type QualifiedIdent = Tuple (Maybe CST.ModuleName) CST.Ident
type UsageMap = SemigroupMap QualifiedIdent (Set CST.SourceRange)

{-

      , discountedAmount : state.discountedAmount
      , phoneNumber : state.phoneNumber
      , orderDesc : state.orderDesc
      , customerName : state.customerName
      , currency : state.currency
      , deviceType: state.deviceType
      , integrationType: state.integrationType
-}

labels = ["phoneNumber", "orderDesc", "customerName", "currency", "deviceType", "integrationType", "productSummary"]

flattenPath :: Separated (Name Label) -> Array String
flattenPath (Separated {head, tail}) = [unwrap (unwrap head).name] <> (map (\(Tuple _ x) -> (unwrap (unwrap head).name)) tail)

isRecordAccess :: forall e. Expr e -> Maybe (Tuple String (Array String))
isRecordAccess = case _ of
    ExprRecordAccessor {expr, path}  -> case expr of
                                          ExprIdent (QualifiedName{name}) -> Just $ Tuple (unwrap name) (flattenPath path)
                                          _ -> Nothing
    _ -> Nothing

filterLabel :: forall e. RecordLabeled (Expr e) -> Maybe (Tuple String (Tuple String (Array String)))
filterLabel  = case _ of
    RecordPun (Name{name}) -> Nothing
    RecordField (Name{name}) _ expr -> let val = (unwrap name)
                              in if Arr.elem val labels then
                                    (\x -> let ret = Just (Tuple val x)
                                               _ = logthis $ show ret
                                            in ret) =<< (isRecordAccess expr)
                                 else Nothing

getLabel :: forall e. RecordLabeled e -> String
getLabel = case _ of
    RecordPun (Name{name}) -> unwrap name
    RecordField (Name{name}) _ _ -> unwrap name

getExprIdents :: forall e. Expr e -> Expr e
getExprIdents x = case x of
    ExprRecord (Wrapped {value}) -> let _ = case value of
                                               Just (Separated{head, tail}) ->
                                                 let _ = unit --logthis $ Arr.length tail
                                                 in
                                                 if Arr.length tail == 34
                                                    then const unit $ [filterLabel head] <> ((\(Tuple _ x) -> filterLabel x) <$> tail)
                                                    else unit
                                               Nothing -> unit
        in x
    ExprApp a arr -> ExprApp a (case _ of
                                  AppTerm expr -> AppTerm $ getExprIdents expr
                                  x -> x
                                <$> arr)
    _ -> x

rewriteInstanceBinding :: forall e. InstanceBinding e -> InstanceBinding e
rewriteInstanceBinding  ib =
    let _ = case ib of
               InstanceBindingName {name} -> logthis $ unwrap (unwrap name).name
               _ -> unit
    in ib

underscore = BinderWildcard {range : {start : {line : 0, column: 1}, end: {line : 0, column: 2}}, leadingComments: [], trailingComments: [ Space 1 ], value : TokUnderscore }

partialDecodeSourceToken = {
                             range: { start: { line: 47, column: 57 }, end: { line: 47, column: 69 } },
                             leadingComments:  [ Line LF 1 , Space 4 ],
                             trailingComments: [ Space 1 ],
                             value: TokLowerName Nothing "partialDecode"
                           }

partialDecodeIdent = ExprIdent $ QualifiedName
                                 { token : {
                                             range: { start: { line: 44, column: 57 }, end: { line: 44, column: 69 } },
                                             leadingComments:  [ Space 1 ],
                                             trailingComments: [ ],
                                             value: TokLowerName Nothing "hyperDecode"
                                            }
                                 , module : Nothing
                                 , name : Ident "partialDecode"}

constructorPartialDecode = ExprIdent $ QualifiedName
                                 { token : {
                                             range: { start: { line: 44, column: 57 }, end: { line: 44, column: 69 } },
                                             leadingComments:  [ ],
                                             trailingComments: [ Space 1 ],
                                             value: TokLowerName Nothing "constructorPartialDecode"
                                            }
                                 , module : Nothing
                                 , name : Ident "partialDecode"}

--lambdaUnwrap = ExprLambda {symbol : {
--                                     range: { start: { line: 44, column: 57 }, end: { line: 44, column: 69 } },
--                                     leadingComments:  [ ],
--                                     trailingComments: [ Space 1 ],
--                                     value: TokLowerName Nothing "constructorPartialDecode"
--                                    }
--                          , binders : NonEmptyArray [BinderParens (Wrapped {})]}

constructorPartialDecodeRhs = case _ of
    ExprApp expr arr -> ExprApp constructorPartialDecode arr
    x -> x

attachPartialDecodeInstance :: forall e. NonEmptyArray (InstanceBinding e) -> NonEmptyArray (InstanceBinding e)
attachPartialDecodeInstance arr = v
    where
        v | hasPartialDecode arr = constructorDecode
--          | hasWrapDecode arr = arr <> wrapDecode
          | hasConstructorDecode arr = constructorDecode
          | true = partialDecode
        partialDecode =
            let
                val = findMap (case _ of
                        InstanceBindingName {name, binders, guarded} -> if unwrap (unwrap name).name == "hyperDecode"
                                                        then Just $ InstanceBindingName { name : const (Name $ (unwrap name) {name = Ident "partialDecode", token = partialDecodeSourceToken}) $ logthis (unwrap name).token
                                                                                        , binders : [underscore]
                                                                                        , guarded : case guarded of
                                                                                                        Unconditional st (Where {expr, bindings}) -> Unconditional st (Where {expr : partialDecodeIdent, bindings : Nothing})
                                                                                                        x -> x
                                                                                        }
                                                        else Nothing
                        _ -> Nothing
                     ) arr
            in case val of
                 Nothing -> arr
                 Just x -> snoc arr x
        constructorDecode =
            let
                val = findMap (case _ of
                        InstanceBindingName {name, binders, guarded} ->
                            if unwrap (unwrap name).name == "hyperDecode"
                            then Just $ InstanceBindingName { name : const (Name $ (unwrap name) {name = Ident "partialDecode", token = partialDecodeSourceToken}) $ logthis (unwrap name).token
                                                            , binders : [underscore]
                                                            , guarded : case guarded of
                                                                            Unconditional st (Where {expr, bindings}) -> Unconditional st (Where {expr : constructorPartialDecodeRhs expr, bindings: Nothing})
                                                                            x -> x
                                                            }
                            else Nothing
                        _ -> Nothing) arr
            in case val of
                 Nothing -> arr
                 Just x -> snoc arr x
        hasPartialDecode arr =
            any (case _ of
                    InstanceBindingName {name} -> unwrap (unwrap name).name == "partialDecode"
                    _ -> false
                 ) arr
        hasWrapDecode arr =
            any (case _ of
                    InstanceBindingName {guarded} ->
                        case guarded of
                            Unconditional st (Where {expr}) ->
                                case expr of
                                    ExprIdent (QualifiedName {name}) -> unwrap name == "wrapDecode"
                                    _ -> false
                            _ -> false
                    _ -> false
                 ) arr
        hasConstructorDecode arr =
            any (case _ of
                    InstanceBindingName {guarded} ->
                        case guarded of
                            Unconditional st (Where {expr}) ->
                                case expr of
                                    ExprApp expr _ -> case expr of
                                                        ExprIdent (QualifiedName {name}) -> unwrap name == "constructorDecode"
                                                        _ -> false
                                    _ -> false
                            _ -> false
                    _ -> false
                 ) arr


rewriteInstance :: forall e. Instance e -> Instance e
rewriteInstance i@(Instance {head, body}) =
    if unwrap (unwrap head.className).name == "HyperDecode"
        then Instance
                { head
                , body : body <#> (\(Tuple st arr) -> Tuple st (attachPartialDecodeInstance arr))}
        else i

getDecls :: forall e. Declaration e -> Declaration e
getDecls d =
    case d of
       DeclInstanceChain (Separated{head, tail}) ->
            DeclInstanceChain $ Separated {head : rewriteInstance head, tail : (\(Tuple st a) -> Tuple st $ rewriteInstance a) <$> tail}
       _ -> d

doThings ::  forall a. CST.Module a -> CST.Module a
doThings  = rewriteModuleTopDown $ defaultVisitor
  { onExpr = identity
  , onDecl = getDecls
  }

getAllScreenFiles :: String -> Aff (Array String)
getAllScreenFiles path = do
    files <- readdir path
    let {yes : f, no: folders } =  Arr.partition (\x -> contains (Pattern ".") x) files
        ff = (\x -> path <> x) <$> Arr.filter (\x -> contains (Pattern ".purs") x) f
    Arr.foldl (\acc curr -> (\x -> (\y -> y <> x) <$> acc) =<< (getAllScreenFiles (path <> curr <> "/"))) (pure ff) folders

printUsage :: Array String -> Aff Unit
printUsage paths =
    case Arr.head paths of
        Just path -> do
          let _ = logthis $ "\n" <> path
          contents <- liftEffect <<< Buffer.toString UTF8 =<< readFile path
          case parseModule contents of
            ParseSucceeded x -> do
                            let _ = logthis $ printModule $ doThings x
                            pure unit
            _                -> pure unit
          case Arr.tail paths of
            Just paths -> printUsage paths
            _ -> pure unit
        _ -> pure unit


main :: Effect Unit
main = launchAff_ do
--    files' <- getAllScreenFiles "/Users/sridatta.yalla/code_here/hyper-decoder/test/"
--    let files = Arr.filter (eq "/Users/sridatta.yalla/code_here/hyper-decoder/test/Types.purs") files'
--    printUsage files
--    pure unit
    contents <- liftEffect <<< Buffer.toString UTF8 =<< readFile "./src/input.txt"
    liftEffect $ checkDirectAssignment "appCheckout" contents
    pure unit

checkDirectAssignment :: String -> String -> Effect Unit
checkDirectAssignment obj code =
    case parseExpr code of
        ParseSucceeded x -> do
            let afterConversion = RecordTracer.checkDirectAssignment x
--            log $ foldMap Print.printSourceToken (TokenList.toArray (tokensOf $ afterConversion))
            pure unit
        ParseFailed err -> pure $ logthis err
        ParseSucceededWithErrors _ _ -> log "parse succeded with errors"

