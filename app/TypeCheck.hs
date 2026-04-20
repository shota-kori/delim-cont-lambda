module TypeCheck where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (join)
import Control.Arrow ((***))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set ((\\))
import Ast

data LamType = TInt | TBool | TAbsAT LamType LamType LamType LamType | TAbs LamType LamType | TVar Int deriving Eq

instance Show LamType where
    show TInt = "Int"
    show TBool = "Bool"
    show (TAbsAT t1 a t2 b) = "(" ++ show t1 ++ " / " ++ show a ++ " -> " ++ show t2 ++ " / " ++ show b ++ ")"
    show (TAbs t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (TVar n) = "t" ++ show n

data Scheme = Forall [Int] LamType deriving Show

type TypeEnv = M.Map Name Scheme
type Substs = M.Map Int LamType
type Constraint = (LamType, LamType)

data DebugConstraint = DC Constraint String
undc :: DebugConstraint -> Constraint
undc (DC c _) = c

instance Show DebugConstraint where
    show (DC c message) = show c ++ " #" ++ message

class TypeRep a where
    freeTVars :: a -> S.Set Int
    appSubsts :: Substs -> a -> a

instance TypeRep LamType where
    freeTVars (TVar n) = S.singleton n
    freeTVars (TAbs t1 t2) = freeTVars t1 `S.union` freeTVars t2
    freeTVars (TAbsAT t1 a t2 b) = S.unions $ freeTVars <$> [t1, a, t2, b]
    freeTVars _ = S.empty

    appSubsts sub t@(TVar n) = M.findWithDefault t n sub
    appSubsts sub (TAbs t1 t2) = TAbs (appSubsts sub t1) (appSubsts sub t2)
    appSubsts sub (TAbsAT t1 a t2 b) = TAbsAT (appSubsts sub t1) (appSubsts sub a) (appSubsts sub t2) (appSubsts sub b)
    appSubsts _ TInt = TInt
    appSubsts _ TBool = TBool

instance TypeRep Scheme where
    freeTVars (Forall vs t) = freeTVars t \\ S.fromList vs
    appSubsts sub (Forall vs t) = Forall vs (appSubsts (foldr M.delete sub vs) t)

instance TypeRep Constraint where
    freeTVars (t1, t2) = freeTVars t1 `S.union` freeTVars t2
    appSubsts sub = join (***) (appSubsts sub)

instance TypeRep v => TypeRep (M.Map k v) where
    freeTVars tenv = freeTVars $ M.elems tenv
    appSubsts sub = M.map $ appSubsts sub

instance TypeRep a => TypeRep [a] where
    freeTVars xs = S.unions $ freeTVars <$> xs
    appSubsts sub = map (appSubsts sub)

newtype TVGen a = TVGen { runTVGen :: ExceptT String (ReaderT TypeEnv (State Int)) a }
    deriving (Functor, Applicative, Monad, MonadError String,
        MonadState Int, MonadReader TypeEnv)

evalTVGen :: TVGen a -> TypeEnv -> Int -> Either String a
evalTVGen x tenv = evalState (runReaderT (runExceptT (runTVGen x)) tenv)

freshVar :: TVGen LamType
freshVar = do
    n <- get
    put (n + 1)
    return (TVar n)

generalize :: LamType -> TVGen Scheme
generalize t = do
    tenv <- ask
    let ftvs = S.difference (freeTVars t) (freeTVars tenv)
    return $ Forall (S.toList ftvs) t

instantiate :: Scheme -> TVGen LamType
instantiate (Forall vs t) = do
    newVars <- mapM (const freshVar) vs
    let sub = M.fromList $ zip vs newVars
    return $ appSubsts sub t

genConstsAT :: Expr -> TVGen (LamType, (LamType, LamType), [DebugConstraint])
genConstsAT (Val (VInt _)) = do
    alpha <- freshVar
    return (TInt, (alpha, alpha), [])

genConstsAT (Val (VBool _)) = do
    alpha <- freshVar
    return (TBool, (alpha, alpha), [])

genConstsAT (Val (Var nm@(Name s _))) = do
    alpha <- freshVar
    tenv <- ask
    case M.lookup nm tenv of
        Just sch -> do
            t <- instantiate sch
            return (t, (alpha, alpha), [])
        Nothing -> throwError $ "Unbound variable: " ++ s

genConstsAT (Val (Abs arg body)) = do
    p <- freshVar
    tv <- freshVar
    (bodyType, (alpha, beta), bodyConstraints) <- local (M.insert arg (Forall [] tv)) (genConstsAT body)
    return (TAbsAT tv alpha bodyType beta, (p, p), bodyConstraints)

genConstsAT (Val (Builtin (BinOp op))) = do
    p <- freshVar
    tv <- freshVar
    alpha <- freshVar
    beta <- freshVar
    case op of
        Eq -> return (TAbsAT tv alpha (TAbsAT tv beta TBool beta) alpha, (p, p), [])
        o | o `elem` [Add, Sub, Mul] -> return (TAbsAT TInt alpha (TAbsAT TInt beta TInt beta) alpha, (p, p), [])
        o | o `elem` [And, Or] -> return (TAbsAT TBool alpha (TAbsAT TBool beta TBool beta) alpha, (p, p), [])
        _ -> undefined

genConstsAT (Val (Builtin (PartialBinOp op _))) = do
    p <- freshVar
    tv <- freshVar
    alpha <- freshVar
    case op of
        Eq -> return (TAbsAT tv alpha TBool alpha, (p, p), [])
        o | o `elem` [Add, Sub, Mul] -> return (TAbsAT TInt alpha TInt alpha, (p, p), [])
        o | o `elem` [And, Or] -> return (TAbsAT TBool alpha TBool alpha, (p, p), [])
        _ -> undefined

genConstsAT (App fun arg) = do
    alpha <- freshVar
    (funType, (gamma, delta), funConstraints) <- genConstsAT fun
    (argType, (beta, gamma'), argConstraints) <- genConstsAT arg
    retType <- freshVar
    let newConstraints = [DC (funType, TAbsAT argType alpha retType beta) "app1", DC (gamma, gamma') "app2"]
    let allConstraints = funConstraints ++ argConstraints ++ newConstraints
    return (retType, (alpha, delta), allConstraints)

genConstsAT (Let nm e1 e2) = do
    (t1, (p, p'), con1) <- genConstsAT e1
    sub1 <- case unify $ undc <$> con1 of
        Left err -> throwError err
        Right sub -> return sub
    tenv <- ask
    let tenv' = appSubsts sub1 tenv
    let t1' = appSubsts sub1 t1
    sch <- local (const tenv') (generalize t1')
    (t2, (alpha, beta), con2) <- local (M.insert nm sch) (genConstsAT e2)
    return (t2, (alpha, beta), con1 ++ con2 ++ [DC (p, p') "let"])

genConstsAT (IfElse e1 e2 e3) = do
    (t1, (alpha1, beta1), con1) <- genConstsAT e1
    (t2, (alpha2, beta2), con2) <- genConstsAT e2
    (t3, (alpha3, beta3), con3) <- genConstsAT e3
    let newConstraints = [
            DC (t1, TBool) "if1",
            DC (beta1, alpha2) "if2",
            DC (beta1, alpha3) "if3",
            DC (t2, t3) "if4",
            DC (beta2, beta3) "if5"]
    let allConstraints = con1 ++ con2 ++ con3 ++ newConstraints
    return (t2, (alpha1, beta2), allConstraints)

genConstsAT (Shift cont e) = do
    t <- freshVar
    tau <- freshVar
    alpha <- freshVar
    (gamma, (gamma', beta), con) <- local (M.insert cont (Forall (S.toList (freeTVars t)) (TAbsAT tau t alpha t))) (genConstsAT e)
    return (tau, (alpha, beta), con ++ [DC (gamma, gamma') "shift"])

genConstsAT (Reset e) = do
    alpha <- freshVar
    (gamma, (gamma', tau), con) <- genConstsAT e
    return (tau, (alpha, alpha), con ++ [DC (gamma, gamma') "reset"])

{--
genConsts :: Expr -> TVGen (LamType, [Constraint])
genConsts (Val (VInt _)) = return (TInt, [])

genConsts (Val (VBool _)) = return (TBool, [])

genConsts (Val (Var nm@(Name s _))) = do
    tenv <- ask
    case M.lookup nm tenv of
        Just sch -> do
            t <- instantiate sch
            return (t, [])
        Nothing -> throwError $ "Unbound variable: " ++ s

genConsts (Val (Abs arg body)) = do
    tv <- freshVar
    (bodyType, bodyConstraints) <- local (M.insert arg (Forall [] tv)) (genConsts body)
    return (TAbs tv bodyType, bodyConstraints)

genConsts (Val (Builtin (BinOp op))) =
    case op of
        Eq -> return (TAbs TInt (TAbs TInt TBool), [])
        _ -> return (TAbs TInt (TAbs TInt TInt), [])

genConsts (Val (Builtin (PartialBinOp op _))) =
    case op of
        Eq -> return (TAbs TInt TBool, [])
        _ -> return (TAbs TInt TInt, [])

genConsts (App fun arg) = do
    (funType, funConstraints) <- genConsts fun
    (argType, argConstraints) <- genConsts arg
    retType <- freshVar
    let newConstraint = (funType, TAbs argType retType)
    let allConstraints = funConstraints ++ argConstraints ++ [newConstraint]
    return (retType, allConstraints)

genConsts (Let nm e1 e2) = do
    (t1, con1) <- genConsts e1
    sub1 <- case unify con1 of
        Left err -> throwError err
        Right sub -> return sub
    tenv <- ask
    let tenv' = appSubsts sub1 tenv
    let t1' = appSubsts sub1 t1
    sch <- local (const tenv') (generalize t1')
    (t2, con2) <- local (M.insert nm sch) (genConsts e2)
    return (t2, con1 ++ con2)

genConsts (IfElse e1 e2 e3) = do
    (t1, con1) <- genConsts e1
    (t2, con2) <- genConsts e2
    (t3, con3) <- genConsts e3
    let newConstraints = [(t1, TBool), (t2, t3)]
    let allConstraints = con1 ++ con2 ++ con3 ++ newConstraints
    return (t2, allConstraints)--}

occursIn :: Int -> LamType -> Bool
occursIn n (TVar m) = n == m
occursIn n (TAbs t1 t2) = occursIn n t1 || occursIn n t2
occursIn n (TAbsAT t1 a t2 b) = occursIn n t1 || occursIn n a || occursIn n t2 || occursIn n b
occursIn _ TInt = False
occursIn _ TBool = False

unifyOnce :: Constraint -> Either String Substs
unifyOnce (t1, t2) | t1 == t2 = return M.empty

unifyOnce (TVar n, t) =
    if occursIn n t then
        Left $ "Type variable: t" ++ show n ++ " occurs inside " ++ show t
    else
        return $ M.singleton n t

unifyOnce (t, TVar n) = unifyOnce (TVar n, t)

unifyOnce (TAbs l1 l2, TAbs r1 r2) = unify [(l1, r1), (l2, r2)]

unifyOnce (TAbsAT l1 la l2 lb, TAbsAT r1 ra r2 rb) = unify [(l1, r1), (la, ra), (l2, r2), (lb, rb)]

unifyOnce (t1, t2) = Left $ "Cannot unify " ++ show t1 ++ " and " ++ show t2

unify :: [Constraint] -> Either String Substs
unify [] = return M.empty

unify (c:cs) = do
    sub1 <- unifyOnce c
    let cs' = appSubsts sub1 cs
    sub2 <- unify cs'
    let retSub = appSubsts sub2 sub1 `M.union` sub2
    return retSub

typeInfer :: Expr -> Either String LamType
typeInfer expr = do
    (result, (alpha, beta), constraints) <- evalTVGen (genConstsAT expr) M.empty 0
    sub <- unify $ let x = constraints ++ [DC (alpha, beta) "top"] in undc <$> x
    case appSubsts sub alpha of
        TVar _ -> return ()
        _ -> Left "This expr is not pure"
    return $ appSubsts sub result
