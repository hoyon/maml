module Type where

import           Protolude hiding (Type, Constraint)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import           Error
import           Prelude (last)

-- Taken from http://www.calebh.io/Type-Inference-by-Solving-Constraints/

data Kind = Star | KFun Kind Kind
  deriving (Show, Eq, Ord)

data BaseTyCon = TyConNumber
               | TyConBool
               | TyConUnit
               | TyConList
               | TyConFun
               | TyConTuple
               | TyConUserDefined Text
               deriving (Show, Eq)

data TyCon = TyCon BaseTyCon Kind
           deriving (Show, Eq)

data TyVar = TyVar Text Kind
           deriving (Show, Eq, Ord)

data TyExpr = TVarExpr TyVar
            | TConExpr TyCon
            | TApExpr TyExpr TyExpr
            deriving (Show, Eq)

data Scheme = ForAll [TyVar] TyExpr

kindStack :: Int -> Kind
kindStack 0 = Star
kindStack n = KFun Star (kindStack (n - 1))

tNumber, tBool, tUnit, tArrow :: TyExpr
tNumber = TConExpr (TyCon TyConNumber Star)
tBool = TConExpr (TyCon TyConBool Star)
tUnit = TConExpr (TyCon TyConUnit Star)

tArrow = TConExpr (TyCon TyConFun (KFun Star (KFun Star Star)))

tFun :: TyExpr -> TyExpr -> TyExpr
tFun tArg = TApExpr (TApExpr tArrow tArg)

tComma :: Int -> TyExpr
tComma numArgs = TConExpr (TyCon TyConTuple (kindStack numArgs))

tTuple :: [TyExpr] -> TyExpr
tTuple tArgs = let numArgs = length tArgs
               in foldl TApExpr (tComma numArgs) tArgs

baseTyConString :: BaseTyCon -> Text
baseTyConString TyConNumber = "Number"
baseTyConString TyConBool = "Bool"
baseTyConString TyConUnit = "()"
baseTyConString (TyConUserDefined name) = name
baseTyConString t = show t

flattenKindChain :: Kind -> [Kind]
flattenKindChain (KFun l r) = l : flattenKindChain r
flattenKindChain Star = [Star]

kindString :: Kind -> Text
kindString k = case flattenKindChain k of
                 [Star] -> "*"
                 chain -> T.intercalate " -> " (map kindString chain)

tyVarString :: TyVar -> Text
tyVarString (TyVar name _) = "'" <> name

tyConString :: TyCon -> Text
tyConString (TyCon baseTyCon _) = baseTyConString baseTyCon

flattenTypeAppChain :: TyExpr -> [TyExpr]
flattenTypeAppChain e = flattenTypeAppChain' e []
  where
    flattenTypeAppChain' (TApExpr l r) acc = flattenTypeAppChain' l (r : acc)
    flattenTypeAppChain' e acc = e : acc

tyExprString :: TyExpr -> Text
tyExprString e = case flattenTypeAppChain e of
                     [TConExpr (TyCon TyConList _), elementTy] ->
                        "[" <> tyExprString elementTy <> "]"
                     TConExpr (TyCon TyConFun _) : args ->
                        T.intercalate " -> " (map tyExprString args)
                     TConExpr (TyCon TyConTuple _) : args ->
                        "(" <> T.intercalate ", " (map tyExprString args) <> ")"
                     TVarExpr v : args ->
                        T.intercalate " " $ tyVarString v : map tyExprString args
                     TConExpr (TyCon (TyConUserDefined name) _) : args ->
                        T.intercalate " " $ name : map tyExprString args
                     [TConExpr (TyCon baseTyCon _)] ->
                        baseTyConString baseTyCon
                     _ ->
                        show e

tySubst :: Map TyVar TyExpr -> TyExpr -> TyExpr
tySubst theta t@(TVarExpr u) = fromMaybe t (Map.lookup u theta)
tySubst theta (TApExpr l r) = TApExpr (tySubst theta l) (tySubst theta r)
tySubst _ t = t

fresh :: Text -> TyVar
fresh n = TyVar n Star

freshTyVar :: Text -> TyVar
freshTyVar t = TyVar t Star

instantiate :: Scheme -> [TyExpr] -> TyExpr
instantiate (ForAll formals tau) actuals = tySubst (Map.fromList (zip formals actuals)) tau

freevars :: TyExpr -> Set TyVar
freevars (TVarExpr u) = Set.singleton u
freevars (TApExpr l r) = Set.union (freevars l) (freevars r)
freevars (TConExpr _) = Set.empty

generalise :: Set TyVar -> TyExpr -> Scheme
generalise skipTyVars tau = ForAll (Set.toList (Set.difference ts skipTyVars)) tau
    where
        ts = freevars tau

type ErrorMessage = Text

data Constraint = Equal TyExpr TyExpr ErrorMessage
                | And Constraint Constraint
                | Trivial
                deriving Show

(=~=) :: TyExpr -> TyExpr -> ErrorMessage -> Constraint
(a =~= b) err = Equal a b err

(&&&) :: Constraint -> Constraint -> Constraint
a &&& b = And a b

conjoinConstraints :: [Constraint] -> Constraint
conjoinConstraints [] = Trivial
conjoinConstraints [c] = c
conjoinConstraints (c:cs) = c &&& conjoinConstraints cs

type Subst = Map TyVar TyExpr

idSubst :: Subst
idSubst = Map.empty

varsubst :: Subst -> TyVar -> TyExpr
varsubst theta v = fromMaybe (TVarExpr v) (Map.lookup v theta)

compose :: Subst -> Subst -> Subst
compose theta1 theta2 = fromKeys replaceTheta domain
    where
        domain = Set.union (Set.fromList (Map.keys theta1)) (Set.fromList (Map.keys theta2))
        replaceTheta = tySubst theta2 . varsubst theta1
        fromKeys f = foldl (\acc k -> Map.insert k (f k) acc) Map.empty

consubst :: Subst -> Constraint -> Constraint
consubst theta (Equal tau1 tau2 err) = Equal (tySubst theta tau1) (tySubst theta tau2) err
consubst theta (And c1 c2) = And (consubst theta c1) (consubst theta c2)
consubst _ Trivial = Trivial

(|--->) :: TyVar -> TyExpr -> Maybe Subst
a |---> tau@(TVarExpr b) = if a == b
                               then Just idSubst
                               else let (TyVar _ kindA) = a
                                        (TyVar _ kindB) = b
                                        in if kindA == kindB
                                               then Just (Map.singleton a tau)
                                               else Nothing

a |---> tau = if Set.member a (freevars tau)
                  then Nothing
                  else Just (Map.singleton a tau)

solve :: Constraint -> Either Error Subst
solve Trivial = Right idSubst
solve (And l r) = do
        theta1 <- solve l
        theta2 <- solve (consubst theta1 r)
        Right $ compose theta1 theta2
solve (Equal tau1 tau2 err) = case (tau1, tau2) of
                                  (TVarExpr a, tau') -> tvarexpr a tau'
                                  (tau', TVarExpr a) -> tvarexpr a tau'
                                  (TConExpr mu, TConExpr mu') -> if mu == mu'
                                                                     then Right idSubst
                                                                     else Left failMsg
                                  (TApExpr l r, TApExpr l' r') ->
                                        solve $ (l =~= l') err &&& (r =~=r') err
                                  _ -> Left failMsg
  where
    tvarexpr a tau' = case a |---> tau' of
                        Just answer -> Right answer
                        Nothing -> Left failMsg
    failMsg = Mismatch (tyExprString tau1) (tyExprString tau2)

makeFunType [t] = t
makeFunType (t:ts) = tFun t (makeFunType ts)
