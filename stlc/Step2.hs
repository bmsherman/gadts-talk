{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}

data Ty = IntTy | BoolTy | Arr Ty Ty

data STy :: Ty -> * where
  SIntTy :: STy IntTy
  SBoolTy :: STy BoolTy
  SArrTy :: STy a -> STy b -> STy (Arr a b)

data Value :: Ty -> * where
  Int :: Int -> Value IntTy
  Bool :: Bool -> Value BoolTy
  Func :: (Value a -> Value b) -> Value (Arr a b)

instance Show (Value a) where
  show v = case v of
    Int i -> show i
    Bool b -> show b
    Func _ -> "<<function>>"

data VarIdx :: [Ty] -> Ty -> * where
  Z :: VarIdx (a ': as) a
  S :: VarIdx as b -> VarIdx (a ': as) b

data Expr :: [Ty] -> Ty -> * where
  Const :: Value ty -> Expr as ty
  Lam   :: STy lamTy -> Expr (lamTy ': as) resTy -> Expr as (Arr lamTy resTy)
  Var   :: VarIdx as b -> Expr as b
  App   :: Expr as (Arr a b) -> Expr as a -> Expr as b

data Env :: [Ty] -> * where
  Nil  :: Env '[]
  (:.) :: Value a -> Env as -> Env (a ': as)

index :: Env tys -> VarIdx tys ty -> Value ty
index (x :. _ ) Z     = x
index (_ :. xs) (S n) = index xs n

eval :: Expr '[] ty -> Value ty
eval e = eval' Nil e
  where
  eval' :: Env env -> Expr env ty  -> Value ty
  eval' env e = case e of
    Const v -> v
    Lam ty e -> Func (\x -> eval' (x :. env) e)
    Var i -> index env i
    App f x -> case eval' env f of
      Func ff -> ff (eval' env x)

test :: Expr '[] IntTy
test = App add3 (Const (Int 5))
  where
  plus :: Expr env (Arr IntTy (Arr IntTy IntTy))
  plus = Const (Func (\(Int x) -> Func (\(Int y) -> Int (x + y)))) 
  add3 = Lam SIntTy (App (App plus (Const (Int 3))) (Var Z))
