module Context where

import Ast
import Control.Applicative ((<|>))
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT (runStateT), modify)
import Data.Tuple (swap)

data Frame = FAppL Expr
    | FAppR Value
    | FLet Name Expr
    | FReset
    | FIfElse Expr Expr
    deriving (Eq, Show)

type Context = [Frame]

data Redex = RApp Value Value
    | RLet Name Value Expr
    | RReset Value
    | RShift Context Name Expr
    | RIfElse Value Expr Expr
    deriving (Eq, Show)

data ShiftAbs = ShiftAbs Name Expr

plug :: Context -> Expr -> Expr
plug ctx e = foldl applyFrame e ctx

applyFrame :: Expr -> Frame -> Expr
applyFrame e (FAppL e2) = App e e2
applyFrame e (FAppR v) = App (Val v) e
applyFrame e (FLet nm e2) = Let nm e e2
applyFrame e FReset = Reset e
applyFrame e (FIfElse t f) = IfElse e t f

type DecompResult a = StateT Context Maybe a

pushFrame :: Frame -> DecompResult ()
pushFrame fr = modify ([fr] ++)

decomposeM :: Expr -> DecompResult Redex
decomposeM (App (Val (Builtin (BinOp op))) (Val v)) =
    return (RApp (Builtin (BinOp op)) v)

decomposeM (App (Val (Builtin (PartialBinOp op v1))) (Val v2)) =
    return (RApp (Builtin (PartialBinOp op v1)) v2)

decomposeM (App (Val v1) (Val v2)) =
    return (RApp v1 v2)

decomposeM (App (Val v) e2) = do
    pushFrame (FAppR v)
    decomposeM e2

decomposeM (App e1 e2) = do
    pushFrame (FAppL e2)
    decomposeM e1

decomposeM (Let nm (Val v) e) =
    return (RLet nm v e)

decomposeM (Let nm e1 e2) = do
    pushFrame (FLet nm e2)
    decomposeM e1

decomposeM (IfElse (Val v) e1 e2) =
    return (RIfElse v e1 e2)

decomposeM (IfElse c e1 e2) = do
    pushFrame (FIfElse e1 e2)
    decomposeM c

decomposeM (Reset (Val v)) =
    return (RReset v)

decomposeM (Reset e) = do
    pushFrame FReset
    decomposeM e
    <|> do
        (ctx, ShiftAbs nm body) <- lift (decomposePure e)
        return (RShift ctx nm body)

decomposeM _ = lift Nothing

decomposePureM :: Expr -> DecompResult ShiftAbs

decomposePureM (Shift nm e) =
    return (ShiftAbs nm e)

decomposePureM (App (Val v1) e2) = do
    pushFrame (FAppR v1)
    decomposePureM e2

decomposePureM (App e1 e2) = do
    pushFrame (FAppL e2)
    decomposePureM e1

decomposePureM (Let nm e1 e2) = do
    pushFrame (FLet nm e2)
    decomposePureM e1

decomposePureM (IfElse c e1 e2) = do
    pushFrame (FIfElse e1 e2)
    decomposePureM c

decomposePureM _ = lift Nothing

decompose :: Expr -> Maybe (Context, Redex)
decompose e = swap <$> runStateT (decomposeM e) []

decomposePure :: Expr -> Maybe (Context, ShiftAbs)
decomposePure e = swap <$> runStateT (decomposePureM e) []
