{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Ty = IntTy | BoolTy | Arr Ty Ty

data Value =
    Int Int
  | Bool Bool
  | Func (Value -> Value)

instance Show Value where
  show v = case v of
    Int i -> show i
    Bool b -> show b
    Func _ -> "<<function>>"

data Expr = 
    Const Value
  | Lam Ty Expr
  | Var Int
  | App Expr Expr

eval :: Expr -> Value
eval e = eval' [] e
  where
  eval' :: [Value] -> Expr -> Value
  eval' env e = case e of
    Const v -> v
    Lam ty e -> Func (\x -> eval' (x : env) e)
    Var i -> env !! i
    App f x -> case eval' env f of
      Func ff -> ff (eval' env x)

test :: Expr
test = App add3 (Const (Int 5))
  where
  plus = Const (Func (\(Int x) -> Func (\(Int y) -> Int (x + y)))) 
  add3 = Lam IntTy (App (App plus (Const (Int 3))) (Var 0))
