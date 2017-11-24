{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Check (typeCheck) where

import           AST
import           Control.Monad.Writer
import           Data.List            (elemIndices)
import qualified Data.Map             as Map
import qualified Data.Set             as S
import           Error
import           Protolude            hiding (Infix)
import           Type

data Env
  = EvVal Text Type Expr
  | EvFun Text [(Text, Type)] Type Expr
  | EvLocal Text Type
  deriving Show

typeCheck :: AST -> ErrWarn EnvMap
typeCheck ast = pure ast >>= checkNames >>= createEnv >>= deduceTypes

-- | Get the name of a declaration
name :: Dec -> Text
name (Val x _)   = x
name (Fun x _ _) = x

-- | Get all names defined in AST
names :: AST -> [Text]
names = fmap name

-- | The number of times each element appears in the list
numOccurances :: (Ord a, Eq a) => [a] -> [(Int, a)]
numOccurances xs = let set = S.fromList xs
                   in S.toList $ S.map (\x -> (length $ elemIndices x xs, x)) set

-- | Duplicate entries in list and the number of times they occur
duplicates :: [Text] -> [(Int, Text)]
duplicates xs = filter (\(x, _) -> x > 1) occurances
  where
    occurances = numOccurances xs

-- | Check for any names defined multiple times
checkNames :: AST -> ErrWarn AST
checkNames ast = case dupes of
                   (x:_) -> throwError . makeErr $ x
                   _     -> return ast
  where
    dupes = duplicates $ names ast
    makeErr (n, x) = MultipleDefinition x n

-- | Create a list of all bindings
createEnv :: AST -> ErrWarn [Env]
createEnv ast = return $ map makeEnv ast

makeEnv :: Dec -> Env
makeEnv (Val x expr) = EvVal x TpUnknown expr
makeEnv (Fun x args expr) = EvFun x [(n, TpUnknown) | n <- args] TpUnknown expr

-- -------------------------------------------------------
-- Type deduction and checking

type EnvMap = Map.Map Text Env

newtype Check a = Check { unCheck :: StateT EnvMap ErrWarn a }
  deriving (Functor, Applicative, Monad, MonadState EnvMap, MonadWriter [Warning], MonadError Error)

-- | Execute Type Check monad
execCheck :: EnvMap -> Env -> ErrWarn EnvMap
execCheck env dec = execStateT (unCheck (typeDec dec)) env

-- | Take an untyped env and deduce all types
deduceTypes :: [Env] -> ErrWarn EnvMap
deduceTypes env = do
  let em = makeMap env
  foldM execCheck em env

makeMap :: [Env] -> EnvMap
makeMap es = Map.fromList $ map env2Pair es

env2Pair :: Env -> (Text, Env)
env2Pair env@(EvVal a _ _)   = (a, env)
env2Pair env@(EvFun a _ _ _) = (a, env)

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
typeDec :: Env -> Check Type
typeDec (EvVal iden _ expr) = do
  env <- get
  tp <- typeExpr expr
  put $ Map.insert iden (EvVal iden tp expr) env
  return tp

typeDec (EvFun fname args _ expr) = do
  env <- get
  -- Create scope with arguements
  localEnv <- pure $ Map.union (Map.fromList $ map (\(x, t) -> (x, EvLocal x t)) args) env
  -- Get types with this scope as the environment
  put localEnv
  tp <- typeExpr expr
  newEnv <- get
  -- Merge types of arguements with locals
  let locals = Map.filter isLocal newEnv

  newArgs <- mapM (\(x, _) -> compareArgs x locals) args

  -- New env with function type now deduced
  put $ Map.union (Map.fromList [(fname, EvFun fname newArgs tp expr)]) env

  return tp

  where
    isLocal x = case x of
      EvLocal{} -> True
      _         -> False
    compareArgs :: Text -> EnvMap -> Check (Text, Type)
    compareArgs iden locals =
      case Map.lookup iden locals of
        Just (EvLocal _ tn) -> if tn == TpUnknown
                               then warn (UnusedVariable iden) >> return (iden, tn)
                               else return (iden, tn)

typeDec _ = panic "Cannot type local env"

-- | Get the type of an expression
typeExpr :: Expr -> Check Type
typeExpr (Con c) = case c of
  (Number _) -> return TpInt
  (Str _)    -> return TpString
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
  case Map.lookup iden env of
    Just (EvVal _ t _) -> return $ unknown t
    Just (EvLocal _ t) -> return $ unknown t
    _                  -> throwError $ NotDefined iden
  where
    unknown TpUnknown = TpUnresolved iden TpUnknown
    unknown x         = x

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

typeExpr e = throwError . OtherError $ "Failed to type " `mappend` show e

-- | Check identifier has the right type
checkIden :: Text -> Type -> Check Bool
checkIden iden tf = do
  env <- get
  case Map.lookup iden env of
    Just (EvVal _ t _) -> if t == tf
                          then return True
                          else return False
    Just (EvLocal _ t) | t == tf        -> return True
                       | t == TpUnknown -> do
                                put $ Map.insert iden (EvLocal iden tf) env
                                return True
                       | otherwise      -> return False
    _ -> throwError $ NotDefined iden

checkPredicate :: Type -> Check Type
checkPredicate t =
  case t of
    TpUnresolved iden _ -> do
      b <- checkIden iden TpBool
      if b
        then return TpBool
        else throwError $ Mismatch TpBool t
    x -> return x
