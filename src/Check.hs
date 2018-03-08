{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Check (typeCheck) where

import           AST
import           Control.Monad.Writer
import           Data.List            (lookup)
import qualified Data.Map             as Map
import           Env
import           Error
import           Formatting
import           Protolude            hiding (Infix, Type, Constraint)
import           Type
import           WasmParse
import           Prelude (last)

newtype Check a = Check { unCheck :: StateT EnvStack ErrWarn a }
  deriving (Functor, Applicative, Monad, MonadState EnvStack, MonadWriter [Warning], MonadError Error, MonadReader Wasm)

-- | Execute Type Check monad
execCheck :: Binding -> Text -> EnvStack -> ErrWarn EnvStack
execCheck b t = execStateT (unCheck (typeDec t b))

-- | Main type checking function
typeCheck :: AST -> ErrWarn Env
typeCheck ast = pure ast >>= createEnv >>= deduceTypes

-- | Take an untyped env and deduce all types
deduceTypes :: Env -> ErrWarn Env
deduceTypes env = do
  res <- foldM (\e (n, b) -> execCheck b n e) [[]] env
  case head res of
    Just x  -> return x
    Nothing -> throwError $ OtherError "Failed to type program"

-- | Built in infix operators
builtinInfix :: Map.Map Text TyExpr
builtinInfix = Map.fromList
  [ ("+", aop)
  , ("-", aop)
  , ("*", aop)
  , ("/", aop)
  , ("%", aop)

  , ("==", cmp)
  , ("!=", cmp)
  , (">=", cmp)
  , ("<=", cmp)
  , ("<" , cmp)
  , (">" , cmp)

  , ("||", bop)
  , ("&&", bop)
  ]
  where
    aop = tFun tNumber (tFun tNumber tNumber)
    cmp = tFun tNumber (tFun tNumber tBool)
    bop = tFun tBool (tFun tBool tBool)

-- | Get the type of a declaration
typeDec :: Text -> Binding -> Check TyExpr
typeDec name (BdVal _ expr) = do
  env <- get
  (t, c) <- typeExpr expr
  put $ putEnv name (BdVal t expr) env
  return t

typeDec name (BdFun t args expr) = do
  env <- get
  -- Create scope with arguments
  let arg_t = map (\(name, n) -> (name, TVarExpr $ freshTyVar n)) $ zip args [0..]
  let localEnv = map (\(name, t) -> (name, BdLocal t)) arg_t

  -- -- Get types with this scope as the environment
  put (localEnv : env)
  (e_t, e_c) <- typeExpr expr

  subst <- either throwError (return . Map.toList) $ solve e_c

  let argTypes = map (matchArg subst) (map snd arg_t)
  {-let result_t = fromMaybe e_t (lookup e_t subst)-}
  let result_t = case e_t of
                   TVarExpr v -> fromMaybe e_t (lookup v subst)
                   _ -> e_t

  let fun_t = makeFunType (argTypes ++ [result_t])

  put $ putEnv name (BdFun fun_t args expr) env

  return fun_t
  where
    matchArg l t@(TVarExpr v) = fromMaybe t (lookup v l)

-- | Get the type of an expression
typeExpr :: Expr -> Check (TyExpr, Constraint)
typeExpr (Con c) = case c of
  (Number _) -> return (tNumber, Trivial)
  (Bool _)   -> return (tBool, Trivial)

typeExpr (If p e1 e2) = do
  (p_t, p_c) <- typeExpr p
  (e1_t, e1_c) <- typeExpr e1
  (e2_t, e2_c) <- typeExpr e2

  let constraints = (p_t =~= tBool) "Predicate must be boolean" &&& (e1_t =~= e2_t) "Clauses must be same type"
  return (e1_t, constraints &&& p_c &&& e1_c &&& e2_c)

typeExpr (While p e) = do
  (p_t, p_c) <- typeExpr p
  (e_t, e_c) <- typeExpr e
  let constraint = (p_t =~= tBool) "Predicate must be boolean"
  return (e_t, constraint &&& e_c &&& p_c)

typeExpr (Tuple es) = do
  tc <- mapM typeExpr es
  return (tTuple (map fst tc), foldr (&&&) Trivial (map snd tc))

typeExpr (Id iden) = do
  env <- get
  case findEnv iden env of
    Just (BdLocal t) -> return (t, Trivial)
    Just (BdVal t _) -> return (t, Trivial)
    Just (BdFun t _ _) -> return (t, Trivial)
    Nothing -> throwError $ NotDefined iden

typeExpr (Infix op e1 e2) =
  case findOperator op of
    Just fun -> do
        (e1_t, e1_c) <- typeExpr e1
        (e2_t, e2_c) <- typeExpr e2
        let result_t = last $ flattenTypeAppChain (last $ flattenTypeAppChain fun)
        let con = (tFun e1_t (tFun e2_t result_t) =~= fun) "Infix operator mismatch"
        return (result_t, con &&& e1_c &&& e2_c)
  where
    findOperator o = Map.lookup o builtinInfix

typeExpr (Call fname args) = do
  env <- get

  case findEnv fname env of
    Just (BdFun t params _) -> do
      args_tc <- mapM typeExpr args
      let tyVar = TVarExpr $ freshTyVar 100
      let fun = makeFunType $ map fst args_tc ++ [tyVar]
      let constraint = (fun =~= t) "Function args don't match"
      return (tyVar, constraint &&& foldr (&&&) Trivial (map snd args_tc))
    _ -> throwError $ NotDefined fname
