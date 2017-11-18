{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Check (typeCheck) where

import           AST
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import qualified Data.Map             as Map
import qualified Data.Set             as S
import qualified Data.Text            as T

type Error = T.Text

data Env
  = EvVal T.Text Type Expr
  | EvFun T.Text [(T.Text, Type)] Type Expr
  | EvLocal T.Text Type
  deriving Show

data Type
  = TpInt
  | TpBool
  | TpString
  | TpFun [Type] Type
  | TpInfix Type Type Type
  | TpTuple [Type]
  | TpUnresolved T.Text Type
  | TpUnknown
  deriving (Show, Eq)

typeCheck :: AST -> Either Error EnvMap
typeCheck ast = pure ast >>= checkNames >>= createEnv >>= deduceTypes

-- | Get the name of a declaration
name :: Dec -> T.Text
name (Val x _)   = x
name (Fun x _ _) = x

-- | Get all names defined in AST
names :: AST -> [T.Text]
names = fmap name

-- | The number of times each element appears in the list
numOccurances :: (Ord a, Eq a) => [a] -> [(Int, a)]
numOccurances xs = let set = S.fromList xs
                   in S.toList $ S.map (\x -> (length $ elemIndices x xs, x)) set

-- | Duplicate entries in list and the number of times they occur
duplicates :: [T.Text] -> [(Int, T.Text)]
duplicates xs = filter (\(x, _) -> x > 1) occurances
  where
    occurances = numOccurances xs

-- | Check for any names defined multiple times
checkNames :: AST -> Either Error AST
checkNames ast = if null dupes
                 then Right ast
                 else Left $ T.unlines $ map makeWarn dupes
  where
    dupes = duplicates $ names ast
    makeWarn (n, x) = T.concat ["name ", x, " defined ", T.pack . show $ n, " times"]

-- | Create a list of all bindings
createEnv :: AST -> Either Error [Env]
createEnv ast = Right $ map makeEnv ast

makeEnv :: Dec -> Env
makeEnv (Val x expr) = EvVal x TpUnknown expr
makeEnv (Fun x args expr) = EvFun x [(n, TpUnknown) | n <- args] TpUnknown expr

-- -------------------------------------------------------
-- Type deduction and checking

type EnvMap = Map.Map T.Text Env

newtype TC a = TC { unTC :: WriterT T.Text (StateT EnvMap (Either Error)) a }
  deriving (Functor, Applicative, Monad, MonadState EnvMap, MonadError Error, MonadWriter T.Text)

makeMap :: [Env] -> EnvMap
makeMap es = Map.fromList $ map env2Pair es

env2Pair :: Env -> (T.Text, Env)
env2Pair env@(EvVal a _ _)   = (a, env)
env2Pair env@(EvFun a _ _ _) = (a, env)

-- | Built in infix operators
builtinInfix :: Map.Map T.Text Type
builtinInfix = Map.fromList
  [ ("+", TpInfix TpInt TpInt TpInt)
  , ("-", TpInfix TpInt TpInt TpInt)
  , ("*", TpInfix TpInt TpInt TpInt)
  , ("/", TpInfix TpInt TpInt TpInt)
  , ("==", TpInfix TpInt TpInt TpBool)
  ]

-- | Get the type of a declaration
typeDec :: Env -> TC Type
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
    compareArgs :: T.Text -> EnvMap -> TC (T.Text, Type)
    compareArgs iden locals =
      case Map.lookup iden locals of
        Just (EvLocal _ tn) -> if tn == TpUnknown
                               then tell "Unused variable" >> return (iden, tn)
                               else return (iden, tn)

typeDec _ = error "Cannot type local env"

-- | Get the type of an expression
typeExpr :: Expr -> TC Type
typeExpr (Con c) = case c of
  (Number _) -> return TpInt
  (Str _)    -> return TpString
  (Bool _)   -> return TpBool

typeExpr (If p e1 e2) = do
  p_t <- typeExpr p >>= checkPredicate
  e1_t <- typeExpr e1
  e2_t <- typeExpr e2

  unless (p_t == TpBool) (throwError "Type of predicate must be bool")
  unless (e1_t == e2_t) (throwError "Type of both clauses of `if` statement must be the same")

  return e1_t

typeExpr (While p e) = do
  p_t <- typeExpr p >>= checkPredicate
  e_t <- typeExpr e
  unless (p_t == TpBool) (throwError "Type of predicate must be bool")
  return e_t

typeExpr (Tuple es) = TpTuple <$> mapM typeExpr es

typeExpr (Id iden) = do
  env <- get
  case Map.lookup iden env of
    Just (EvVal _ t _) -> return $ unknown t
    Just (EvLocal _ t) -> return $ unknown t
    _ -> throwError $ T.concat ["Name not defined: ", iden]
  where
    unknown TpUnknown = TpUnresolved iden TpUnknown
    unknown x = x

typeExpr (Infix op e1 e2) =
  case findOperator op of
    Just (TpInfix tp1 tp2 tr) -> do
      checkOperand e1 tp1
      checkOperand e2 tp2

      return tr
    _ -> throwError $ T.concat ["Operator not defined: " , op]
  where
    findOperator o = Map.lookup o builtinInfix
    checkOperand e t =
      case e of
        Id iden -> do
          b <- checkIden iden t
          unless b (throwError $ T.concat ["Type mismatch: ", iden, " not of type ", T.pack $ show t])
        _ -> do
          et <- typeExpr e
          unless (t == et) (throwError "Operand type mismatch")

typeExpr e = throwError $ T.concat ["Failed to type ", T.pack $ show e]

-- | Check identifier has the right type
checkIden :: T.Text -> Type -> TC Bool
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
    _ -> throwError $ T.concat ["Name not defined: ", iden]

checkPredicate :: Type -> TC Type
checkPredicate t =
  case t of
    TpUnresolved iden _ -> do
      b <- checkIden iden TpBool
      if b
        then return TpBool
        else throwError "Type of predicate must be bool"
    x -> return x

-- | Execute Type Check monad
execTC :: EnvMap -> Env -> Either Error EnvMap
execTC env dec = (flip execStateT env . runWriterT) (unTC (typeDec dec))

-- | Take an untyped env and deduce all types
deduceTypes :: [Env] -> Either Error EnvMap
deduceTypes env = do
  let em = makeMap env
  foldM execTC em env
