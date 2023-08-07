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
import PureScript.CST (RecoveredParserResult(..), parseModule, printModule)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readdir)
import Effect.Aff (Aff, launchAff_)
import PureScript.CST.Types (AppSpine(AppType), AppSpine(AppTerm), Binder, Binder(..), Declaration, Declaration(..), DoStatement, DoStatement(DoLet), DoStatement(DoDiscard), DoStatement(DoBind), DoStatement(DoError), Expr, Expr(..), Guarded, Guarded(Unconditional), Guarded(Guarded), Ident, Label, Labeled(Labeled), LetBinding, LetBinding(LetBindingSignature), LetBinding(LetBindingPattern), LetBinding(LetBindingName), LetBinding(LetBindingError), Module, Name(Name), QualifiedName(QualifiedName), RecordLabeled(RecordPun), RecordLabeled(RecordField), Separated, Separated(Separated), Where(Where), Wrapped(Wrapped))
import Data.Newtype (unwrap)
import Data.Array.NonEmpty (any, concat, filter, fromArray, toArray)
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Array as Arr
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))

type QualifiedIdent = Tuple (Maybe CST.ModuleName) CST.Ident
type UsageMap = SemigroupMap QualifiedIdent (Set CST.SourceRange)

foreign import logthis :: forall a. a -> Unit

foreign import logTwo :: forall a. String -> a -> Unit
foreign import addIndent :: Unit -> Unit
foreign import removeIndent :: Unit -> Unit

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

doThings ::  forall a. CST.Module a -> CST.Module a
doThings  = rewriteModuleTopDown $ defaultVisitor
  {
    onExpr = getExprIdents
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
--                            let _ = logthis $ printModule $ removeUnused x
                            let _ = doThings x
                            pure unit
            _                -> pure unit
          case Arr.tail paths of
            Just paths -> printUsage paths
            _ -> pure unit
        _ -> pure unit


main :: Effect Unit
main = launchAff_ do
--    contents <- liftEffect <<< Buffer.toString UTF8 =<< readFile "./src/Test.purs"
    files' <- getAllScreenFiles "/home/sridatta/code_here/hyper-widget/src/"
    let files = Arr.filter (not <<< eq "/home/sridatta/code_here/hyper-widget/src/UI/View/PaymentPage/ViewUtils.purs") files'

    printUsage files
    pure unit