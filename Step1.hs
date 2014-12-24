{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Value = Int Int
  | Bool Bool
  | Tuple Value Value
  deriving Show

data Expr = V Value
  | IfThenElse Expr Expr Expr
  | MkTuple Expr Expr
  | First Expr
  | Second Expr
  | Equal Expr Expr

eqValue :: Value -> Value -> Bool
eqValue (Int i) (Int j) = i == j
eqValue (Bool b1) (Bool b2) = b1 == b2
eqValue (Tuple u1 u2) (Tuple v1 v2) = u1 `eqValue` v1 && u2 `eqValue` v2

eval :: Expr -> Value
eval (V v) = v
eval (IfThenElse e t f) = case eval e of
  Bool tf -> if tf then eval t else eval f
eval (MkTuple e1 e2) = Tuple (eval e1) (eval e2)
eval (First e) = case eval e of Tuple v1 v2 -> v1
eval (Second e) = case eval e of Tuple v1 v2 -> v2
eval (Equal e1 e2) = Bool (eqValue (eval e1) (eval e2))


test :: Expr
test = IfThenElse (V (Bool True)) (V (Int 13)) (V (Tuple (Int 13) (Bool False)))
