{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Check (typeCheck) where

import           AST
import           Control.Monad.Writer
import           Data.List            (lookup)
import qualified Data.Map             as Map
import           Env
import           Error
import           Formatting
import           Protolude            hiding (Infix, (%))
import           Type

newtype Check a = Check { unCheck :: StateT EnvStack ErrWarn a }
  deriving (Functor, Applicative, Monad, MonadState EnvStack, MonadWriter [Warning], MonadError Error)

-- | Execute Type Check monad
execCheck :: Binding -> Text -> EnvStack -> ErrWarn EnvStack
execCheck b t = execStateT (unCheck (typeDec t b))

-- | Main type checking function
typeCheck :: AST -> ErrWarn Env
typeCheck ast = pure ast >>= createEnv >>= deduceTypes

-- | Take an untyped env and deduce all types
deduceTypes :: Env -> ErrWarn Env
deduceTypes env = do
  res <- foldM (\e (n, b) -> execCheck b n e) [env] env
  case head res of
    Just x  -> return x
    Nothing -> throwError $ OtherError "Failed to type program"

-- | Built in infix operators
builtinInfix :: Map.Map Text Type
builtinInfix = Map.fromList
  [ ("+", TpInfix TpInt TpInt TpInt)
  , ("-", TpInfix TpInt TpInt TpInt)
  , ("*", TpInfix TpInt TpInt TpInt)
  , ("/", TpInfix TpInt TpInt TpInt)
  , ("%", TpInfix TpInt TpInt TpInt)

  , ("==", TpInfix TpInt TpInt TpBool)
  , ("!=", TpInfix TpInt TpInt TpBool)
  , (">=", TpInfix TpInt TpInt TpBool)
  , ("<=", TpInfix TpInt TpInt TpBool)
  , ("<", TpInfix TpInt TpInt TpBool)
  , (">", TpInfix TpInt TpInt TpBool)

  , ("||", TpInfix TpBool TpBool TpBool)
  , ("&&", TpInfix TpBool TpBool TpBool)
  ]

-- | Get the type of a declaration
typeDec :: Text -> Binding -> Check Type
typeDec _ (BdVal expr) = do
  tp <- typeExpr expr
  case tp of
    TpInt -> return TpInt
    _     -> throwError $ OtherError "Bindings must be integers"

typeDec _ (BdConst con) = do
  case con of
    Number _ -> return TpInt
    _        -> throwError $ OtherError "Bindings must be integers"

typeDec name (BdFun args expr) = do

  env <- get
  -- Create scope with arguments
  -- localEnv <- pure $ Map.union (Map.fromList $ map (\(x, t) -> (x, EvLocal x t)) args) env
  let localEnv = map (\x -> (x, BdVal (Con $ Number 0))) args

  -- -- Get types with this scope as the environment
  put (localEnv : env)
  tp <- typeExpr expr
  -- newEnv <- get
  put env
  case tp of
    TpInt -> return TpInt
    _     -> throwError $ OtherError "Function must return an integer"

  -- -- Merge types of arguments with locals
  -- newArgs <- matchArgs args newEnv

  -- -- New env with function type now deduced
  -- put $ putEnv name (BdFun newArgs tp expr) env

  -- return tp

  -- where
  --   matchArgs :: [(Text, Type)] -> EnvStack -> Check [(Text, Type)]
  --   matchArgs a (env:_) = mapM (\p@(n, _) -> case lookup n env of
  --                                              Just (BdVal TpUnknown _) -> warn (UnusedVariable n) >> return p
  --                                              Just (BdVal t _) -> return (n, t)
  --                                              _                -> panic "Can't find arg in env") a
  --   matchArgs _ _ = panic "Empty environment"

-- | Get the type of an expression
typeExpr :: Expr -> Check Type
typeExpr (Con c) = case c of
  (Number _) -> return TpInt
  -- (Bool _)   -> return TpBool

typeExpr (If p e1 e2) = do
  p_t <- typeExpr p
  e1_t <- typeExpr e1
  e2_t <- typeExpr e2

  unless (p_t == TpBool) (throwError $ Mismatch TpBool p_t)
  unless (e1_t == e2_t) (throwError $ Mismatch e1_t e2_t)

  return e1_t

typeExpr (While p e) = do
  p_t <- typeExpr p
  e_t <- typeExpr e
  unless (p_t == TpBool) (throwError $ Mismatch TpBool p_t)
  return e_t

typeExpr (Tuple es) = TpTuple <$> mapM typeExpr es

typeExpr (Id iden) = do
  env <- get
  case findEnv iden env of
    -- XXX Beware of recursive definitions!
    Just (BdVal _)   -> return TpInt
    Just (BdConst _) -> return TpInt
    _                -> throwError $ NotDefined iden

typeExpr (Infix op e1 e2) =
  case findOperator op of
    Just (TpInfix tp1 tp2 tr) -> do
      checkOperand e1 tp1
      checkOperand e2 tp2

      return tr
    _ -> throwError $ NotDefined op
  where
    findOperator o = Map.lookup o builtinInfix
    checkOperand e t = do
      et <- typeExpr e
      unless (t == et) (throwError $ Mismatch t et)

typeExpr (Call fname args) = do
  env <- get

  case findEnv fname env of
    Just (BdFun params _) -> if length args == length params
                             then return TpInt
                             else throwError $ BadCallError (length args) (length params)
    _ -> throwError $ NotDefined fname
