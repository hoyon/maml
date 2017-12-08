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
  , ("==", TpInfix TpInt TpInt TpBool)
  ]

-- | Get the type of a declaration
typeDec :: Text -> Binding -> Check Type
typeDec name (BdVal _ expr) = do
  env <- get
  tp <- typeExpr expr
  -- put $ Map.insert iden (EvVal iden tp expr) env
  put $ putEnv name (BdVal tp expr) env
  return tp

typeDec _ (BdConst tp _) = return tp

typeDec name (BdFun args _ expr) = do
  env <- get
  -- Create scope with arguements
  -- localEnv <- pure $ Map.union (Map.fromList $ map (\(x, t) -> (x, EvLocal x t)) args) env
  let localEnv = map (\(x, t) -> (x, BdVal t (Con $ Number 23))) args

  -- Get types with this scope as the environment
  put (localEnv : env)
  tp <- typeExpr expr
  newEnv <- get

  -- Merge types of arguements with locals
  newArgs <- case head newEnv >>= matchArgs args of
    Just x  -> return x
    Nothing -> throwError $ OtherError "Failed to type arguements"

  -- New env with function type now deduced
  put $ putEnv name (BdFun newArgs tp expr) env

  return tp

  where
    matchArgs :: [(Text, Type)] -> Env -> Maybe [(Text, Type)]
    matchArgs a env = mapM (\(n, _) -> case lookup n env of
                                         Just (BdVal t _) -> Just (n, t)
                                         _                -> Nothing) a

-- | Get the type of an expression
typeExpr :: Expr -> Check Type
typeExpr (Con c) = case c of
  (Number _) -> return TpInt
  (Bool _)   -> return TpBool

typeExpr (If p e1 e2) = do
  p_t <- typeExpr p >>= checkPredicate
  e1_t <- typeExpr e1
  e2_t <- typeExpr e2

  unless (p_t == TpBool) (throwError $ Mismatch TpBool p_t)
  unless (e1_t == e2_t) (throwError $ Mismatch e1_t e2_t)

  return e1_t

typeExpr (While p e) = do
  p_t <- typeExpr p >>= checkPredicate
  e_t <- typeExpr e
  unless (p_t == TpBool) (throwError $ Mismatch TpBool p_t)
  return e_t

typeExpr (Tuple es) = TpTuple <$> mapM typeExpr es

typeExpr (Id iden) = do
  env <- get
  case findEnv iden env of
    -- XXX Beware of recursive definitions!
    Just (BdVal TpUnknown expr) -> typeExpr expr
    Just (BdVal t _)            -> return t
    _                           -> throwError $ NotDefined iden

typeExpr (Infix op e1 e2) =
  case findOperator op of
    Just (TpInfix tp1 tp2 tr) -> do
      checkOperand e1 tp1
      checkOperand e2 tp2

      return tr
    _ -> throwError $ NotDefined op
  where
    findOperator o = Map.lookup o builtinInfix
    checkOperand e t =
      case e of
        Id iden -> do
          b <- checkIden iden t
          unless b (throwError $ Mismatch TpBool t)
        _ -> do
          et <- typeExpr e
          unless (t == et) (throwError $ Mismatch t et)

typeExpr e = throwError . OtherError $ sformat ("Failed to type " % text) (show e)

-- | Check identifier has the right type
checkIden :: Text -> Type -> Check Bool
checkIden iden tf = do
  env <- get
  case findEnv iden env of
    Just (BdVal t expr) | t == tf -> return True
                        | t == TpUnknown -> do
                            put $ putEnv iden (BdVal tf expr) env
                            return True
                        | otherwise -> return False

    _ -> throwError $ NotDefined iden

checkPredicate :: Type -> Check Type
checkPredicate t@(TpUnresolved iden _) = do
      b <- checkIden iden TpBool
      if b
        then return TpBool
        else throwError $ Mismatch TpBool t
checkPredicate x = return x
