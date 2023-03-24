module IceLang.Lint
  ( lint
  ) where

import           Control.Monad.State.Strict
import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Madlib.Operator

import           IceLang.Syntax

showSourceSpan :: SourceSpan -> String
showSourceSpan sourceSpan =
  " ("
  <> show sourceSpan
  <> ")"

data Error
  = NotDefined SourceSpan Text
  | Defined SourceSpan Text
  | AssignmentToConstantVariable SourceSpan

instance Show Error where
  show =
    \case
      NotDefined ss name ->
        "Not defined: " <> Text.unpack name <> showSourceSpan ss

      Defined ss name ->
        "Variable '"
        <> Text.unpack name
        <> "' is already exists"
        <> showSourceSpan ss

      AssignmentToConstantVariable ss ->
        "Assignment to constant variable" <> showSourceSpan ss

data ValueType
  = VFunction
  | VLet
  | VVar
  deriving Show

newtype Value = Value
  { valueType :: ValueType
  } deriving Show

data Env = Env
  { current :: Map.Map Text Value
  , parent  :: Maybe Env
  } deriving Show

addEnv' :: Text -> ValueType -> Env -> Env
addEnv' name vtype env =
  env
    { current = Map.insert name (Value vtype) env.current
    }

extendEnv' :: Env -> Env
extendEnv' env =
  env
    { current = mempty
    , parent = Just env
    }

shrinkEnv' :: Env -> Env
shrinkEnv' env =
  case env.parent of
    Just p ->
      Env p.current p.parent

    Nothing ->
      Env mempty Nothing

data LState = LState
  { env            :: Env
  , errors         :: [Error]
  , lazyStatements :: [([Text], [StatementL])]
  } deriving Show

core :: Map.Map Text Value
core = Map.fromList
  [ ("help", Value VLet)
  , ("print", Value VLet)
  , ("readline", Value VLet)
  , ("USER_ID", Value VLet)
  , ("USER_NAME", Value VLet)
  , ("USER_USERNAME", Value VLet)
  , ("CUSTOM_EMOJIS", Value VLet)
  , ("THIS_URL", Value VLet)
  , ("THIS_ID", Value VLet)
  ]

emptyState :: LState
emptyState = LState
  { env = Env core Nothing
  , errors = []
  , lazyStatements = []
  }

type Lint =
  State LState

addEnv :: Text -> ValueType -> Lint ()
addEnv name vtype =
  modify' $ \st -> st { env = addEnv' name vtype st.env }

addEnvWithCheck :: SourceSpan -> Text -> ValueType -> Lint ()
addEnvWithCheck loc name vtype = do
  st <- get
  case Map.lookup name st.env.current of
    Just _ ->
      addError $ Defined loc name

    Nothing ->
      addEnv name vtype

extendEnv :: Lint ()
extendEnv =
  modify' $ \st -> st { env = extendEnv' st.env }

shrinkEnv :: Lint ()
shrinkEnv =
  modify' $ \st -> st { env = shrinkEnv' st.env }

lookupEnv :: Text -> Lint (Maybe Value)
lookupEnv name = do
  st <- get
  let
    lookupEnv' :: Env -> Maybe Value
    lookupEnv' env =
      case Map.lookup name env.current of
        Just v ->
          Just v

        Nothing ->
          env.parent >>= lookupEnv'
  pure $ lookupEnv' st.env

addLazyStatements :: ([Text], [StatementL]) -> Lint ()
addLazyStatements xs = do
  modify' $ \st -> st { lazyStatements = xs : st.lazyStatements }

addError :: Error -> Lint ()
addError err =
  modify' (\st -> st { errors = st.errors ++ [err] })

lintLazy :: Lint ()
lintLazy = do
  st <- get
  modify' $ \s -> s { lazyStatements = [] }
  st.lazyStatements
    |> mapM_
      (\(vars, ss) -> do
        extendEnv
        mapM_ (`addEnv` VVar) vars
        mapM_ lintStatement ss
        lintLazy
        shrinkEnv
      )

lintBlock :: StatementL -> Lint () -> Lint ()
lintBlock s@(Located _ statement) preAction = do
  extendEnv
  preAction
  case statement of
    SBlock ss -> do
      mapM_ lintStatement ss

    _ -> do
      lintStatement s
  lintLazy
  shrinkEnv

lintTemplate :: TemplateValue -> Lint ()
lintTemplate =
  \case
    TString{} ->
      pure ()

    TExpr e ->
      lintExpr e

lintChainValue :: ChainValue -> Lint ()
lintChainValue =
  \case
    ChainName{} ->
      pure ()

    ChainCall e ->
      mapM_ lintExpr e

lintExpr :: ExprL -> Lint ()
lintExpr (Located loc expr) =
  case expr of
    EVar name -> do
      value <- lookupEnv name
      case value of
        Nothing ->
          addError $ NotDefined loc name

        Just _ ->
          pure ()

    ETemplate ts ->
      mapM_ lintTemplate ts

    EProp l _ ->
      lintExpr l

    EOptChain l chainValue -> do
      lintExpr l
      lintChainValue chainValue

    ENamespace (Located _ EVar{}) _ ->
      pure ()

    ENamespace l _ ->
      lintExpr l

    EIndex l index -> do
      lintExpr l
      lintExpr index

    EArray vs ->
      mapM_ lintExpr vs

    EObject fields ->
      fields
        |> mapM_ (\(_, e) -> lintExpr e)

    EBinOp _ l r -> do
      lintExpr l
      lintExpr r

    ENot e ->
      lintExpr e

    EAbs args body -> do
      addLazyStatements (args, body)

    ECall e params -> do
      lintExpr e
      mapM_ lintExpr params

    EIf thenCond thenBody elifs else_ -> do
      let
        lintThen (cond, body) = do
          lintExpr cond
          addLazyStatements ([], [body])
      lintThen (thenCond, thenBody)
      mapM_ lintThen elifs
      mapM_ (\b -> addLazyStatements ([], [b])) else_

    EMatch cond patterns -> do
      let
        lintPat =
          \case
            PatAny ->
              pure ()

            PatExpr e ->
              lintExpr e

        lintPattern (pat, body) = do
          lintPat pat
          addLazyStatements ([], [body])

      lintExpr cond
      mapM_ lintPattern patterns

    EEval body -> do
      extendEnv
      mapM_ lintStatement body
      shrinkEnv

    _ ->
      pure ()

lintStatement :: StatementL -> Lint ()
lintStatement (Located loc statement) =
  case statement of
    SExpr e ->
      lintExpr e

    SLet name v -> do
      addEnvWithCheck loc name VLet
      lintExpr v

    SVar name v -> do
      addEnvWithCheck loc name VVar
      lintExpr v

    SAssign _ (Located _ (EVar name)) r -> do
      value <- lookupEnv name
      case value of
        Just v ->
          case v.valueType of
            VLet ->
              addError $ AssignmentToConstantVariable loc

            _ ->
              lintExpr r

        Nothing ->
          addError $ NotDefined loc name

    SAssign _ l r -> do
      lintExpr l
      lintExpr r

    SFunDef name args body -> do
      addEnvWithCheck loc name VFunction
      addLazyStatements (args, body)

    SFor let_ value body -> do
      let
        lintLet (_, e) =
          mapM_ lintExpr e

      mapM_ lintLet let_
      lintExpr value

      addLazyStatements
        ( case let_ of
            Just (name, _) ->
              [name]

            Nothing ->
              []
        , [body]
        )

    SEach name value body -> do
      lintExpr value
      addLazyStatements ([name], [body])

    SLoop body ->
      lintBlock body (pure ())

    SBlock body -> do
      addLazyStatements ([], body)

    SReturn e ->
      lintExpr e

    SContinue ->
      pure ()

    SBreak ->
      pure ()

lintToplevels :: [StatementL] -> Lint ()
lintToplevels toplevels = do
  mapM_ lintStatement toplevels
  lintLazy

lint :: Program -> Maybe [Error]
lint p =
  let
    st =
      execState (lintToplevels p.toplevels) emptyState
  in
  if List.null st.errors then
    Nothing

  else
    Just st.errors
