module Reduce where

import Control.Monad.Reader (Reader, asks, local, runReader)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Generics.Uniplate.Data (descendM, transform)
import Ast
import Context

data RuntimeError = DecomposeError | SimpleError

instance Show RuntimeError where
    show DecomposeError = "Can't decompose some expr."
    show SimpleError = "Can't simple some redex."

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

type UsedVars = M.Map String Int

freeVars :: Expr -> S.Set Name
freeVars (Val (Var nm)) = S.singleton nm
freeVars (Val (Builtin _)) = S.empty
freeVars (Val (Abs nm e)) = S.delete nm (freeVars e)
freeVars (Val _) = S.empty
freeVars (Let nm e1 e2) = S.union (S.delete nm (freeVars e2)) (freeVars e1)
freeVars (IfElse e1 e2 e3) = S.union (freeVars e1) (S.union (freeVars e2) (freeVars e3))
freeVars (App f x) = S.union (freeVars f) (freeVars x)
freeVars (Shift cont e) = S.delete cont (freeVars e)
freeVars (Reset e) = freeVars e

newScope :: String -> (Int -> Reader UsedVars a) -> Reader UsedVars a
newScope s action = do
    currentIdx <- asks $ M.findWithDefault (-1) s
    let newIdx = currentIdx + 1
    local (M.insert s newIdx) (action newIdx)

alphaConvM :: Expr -> Reader UsedVars Expr
alphaConvM (Val (Var (Name s _))) = do
    idx <- asks $ M.findWithDefault 0 s
    return $ Val $ Var $ Name s idx

alphaConvM (Val (Abs (Name s _) e)) =
    newScope s $ \idx -> do
        e' <- alphaConvM e
        return $ Val $ Abs (Name s idx) e'

alphaConvM (Let (Name s _) e1 e2) = do
    e1' <- alphaConvM e1
    newScope s $ \idx -> do
        e2' <- alphaConvM e2
        return $ Let (Name s idx) e1' e2'

alphaConvM (Shift (Name s _) e) = do
    newScope s $ \idx -> do
        e' <- alphaConvM e
        return $ Shift (Name s idx) e'

alphaConvM e = descendM alphaConvM e

alphaConv :: Expr -> Expr
alphaConv e = runReader (alphaConvM e) initFreeVars
    where
        initFreeVars = M.fromSet (const 0) $ string `S.map` freeVars e

subst :: Name -> Value -> Expr -> Expr
subst nm v = transform replace
  where
    replace (Val (Var vnm)) | vnm == nm = Val v
    replace x = x

simple :: Redex -> Int -> Maybe Expr
simple (RApp (Abs nm e) v) _ = return $ alphaConv $ subst nm v e
simple (RApp (Builtin (BinOp op)) v) _ = return $ Val $ Builtin $ PartialBinOp op v
simple (RApp (Builtin (PartialBinOp Eq v1)) v2) _ =
    return $ Val $ VBool (v1 == v2)
simple (RApp (Builtin (PartialBinOp op (VInt n))) (VInt m)) _ = do
    f <- case op of
        Add -> return (+)
        Sub -> return (-)
        Mul -> return (*)
        _ -> Nothing
    return $ Val $ VInt $ f n m
simple (RApp (Builtin (PartialBinOp op (VBool p))) (VBool q)) _ = do
    f <- case op of
        And -> return (&&)
        Or -> return (||)
        _ -> Nothing
    return $ Val $ VBool $ f p q
simple (RLet nm v e) _ = return $ alphaConv $ subst nm v e
simple (RIfElse (VBool b) e1 e2) _ =
    return $ if b then e1 else e2
simple (RReset v) _ = return $ Val v
simple (RShift f nm e) n = do
    let argn = Name "_arg" n
    return $ Reset (App (Val (Abs nm e)) (Val (Abs argn (Reset (plug f (Val (Var argn)))))))
simple _ _ = Nothing

evalStepN :: Expr -> Int -> Either RuntimeError Expr
evalStepN e n = do
    (ctx, rdx) <- note DecomposeError $ decompose e
    e' <- note SimpleError $ simple rdx n
    return $ plug ctx e'

evalN :: Expr -> Int -> Either RuntimeError Value
evalN (Val v) _ = return v

evalN e n = do
    e' <- evalStepN e n
    evalN e' (n+1)

eval :: Expr -> Either RuntimeError Value
eval e = evalN (alphaConv e) 0
