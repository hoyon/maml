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

data CheckState = CheckState{ csStack :: EnvStack, csCount :: Int, csPrefix :: Text }

newtype Check a = Check { unCheck :: StateT CheckState ErrWarn a }
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadWriter [Warning], MonadError Error, MonadReader Wasm)

-- | Execute Type Check monad
execCheck :: Binding -> Text -> CheckState -> ErrWarn CheckState
execCheck b t = execStateT (unCheck (typeDec t b))

-- | Main type checking function
typeCheck :: AST -> ErrWarn Env
typeCheck ast = pure ast >>= createEnv >>= deduceTypes

-- | Take an untyped env and deduce all types
deduceTypes :: Env -> ErrWarn Env
deduceTypes env = do
  res <- foldM (\s (n, b) -> execCheck b n s) (CheckState [[]] 0 "t") env
  case head $ csStack res of
    Just e  -> return e
    Nothing -> throwError $ OtherError "Failed to type program"

freshVar :: Check TyVar
freshVar = do
  cs <- get
  put cs{csCount = csCount cs + 1}
  return $ freshTyVar $ csPrefix cs <> show (csCount cs)

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
  cs <- get
  put cs{csPrefix = name}
  (t, c) <- typeExpr expr

  subst <- either throwError pure $ solve c

  let result_t = case t of
                   TVarExpr v -> fromMaybe t (Map.lookup v subst)
                   _ -> t
  unless (result_t == tNumber || result_t == tBool) (throwError $ OtherError "Function not fully applied")

  put cs{csStack = putEnv name (BdVal result_t expr) $ csStack cs}
  return result_t

typeDec name dec@(BdFun t args expr) = do
  cs <- get
  put cs{csPrefix = name}
  -- Create scope with arguments
  arg_t <- mapM makeArgs args
  let localEnv = (name, dec) : map (\(name, t) -> (name, BdLocal t)) arg_t

  -- Put args into scope
  put cs{csStack = localEnv : csStack cs}

  -- Get type and constraints of function
  (e_t, e_c) <- typeExpr expr

  -- Solve types
  subst <- either throwError pure $ solve e_c

  let argTypes = map (matchArg subst) (map snd arg_t)
  let result_t = case e_t of
                   TVarExpr v -> fromMaybe e_t (Map.lookup v subst)
                   _ -> e_t

  let fun_t = makeFunType (argTypes ++ [result_t])

  put cs{csStack = putEnv name (BdFun fun_t args expr) $ csStack cs}

  return fun_t
  where
    makeArgs name = do
      tv <- TVarExpr <$> freshVar
      return (name, tv)

    matchArg l t@(TVarExpr v) = fromMaybe t (Map.lookup v l)

-- | Get the type of an expression
typeExpr :: Expr -> Check (TyExpr, Constraint)
typeExpr (Con c) = case c of
  (Number _) -> return (tNumber, Trivial)
  (Bool _)   -> return (tBool, Trivial)

typeExpr (If p e1 e2) = do
  (p_t, p_c) <- typeExpr p
  (e1_t, e1_c) <- typeExpr e1
  (e2_t, e2_c) <- typeExpr e2

  let constraints = (tBool =~= p_t) "Predicate must be boolean" &&& (e1_t =~= e2_t) "Clauses must be same type"
  return (e1_t, constraints &&& p_c &&& e1_c &&& e2_c)

typeExpr (Id iden) = do
  cs <- get
  let env = csStack cs
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

typeExpr (App a b) = do
  tyVar <- TVarExpr <$> freshVar
  (a_t, a_c) <- typeExpr a
  (b_t, b_c) <- typeExpr b
  let fun = tFun b_t tyVar
  let constraint = (a_t =~= fun) "Application invalid"
  return (tyVar, constraint &&& a_c &&& b_c)
