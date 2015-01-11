{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, StandaloneDeriving #-}

data Value a where
  Int   :: Int  -> Value Int
  Bool  :: Bool -> Value Bool
  Tuple :: Value a -> Value b -> Value (a, b)

deriving instance Show (Value a)

data Expr a where
  V          :: Value a -> Expr a
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a
  MkTuple    :: Expr a -> Expr b -> Expr (a, b)
  First      :: Expr (a, b) -> Expr a
  Second     :: Expr (a, b) -> Expr b
  Equal      :: Expr a -> Expr a -> Expr Bool

eqValue :: Value a -> Value a -> Bool
eqValue (Int i) (Int j) = i == j
eqValue (Bool b1) (Bool b2) = b1 == b2
eqValue (Tuple u1 u2) (Tuple v1 v2) = u1 `eqValue` v1 && u2 `eqValue` v2

eval :: Expr a -> Value a
eval (V v) = v
eval (IfThenElse e t f) = case eval e of
  Bool tf -> if tf then eval t else eval f
eval (MkTuple e1 e2) = Tuple (eval e1) (eval e2)
eval (First e) = case eval e of
  Tuple v1 v2 -> v1
eval (Second e) = case eval e of
  Tuple v1 v2 -> v2
eval (Equal e1 e2) = Bool (eqValue (eval e1) (eval e2))


test :: Expr Int
test = IfThenElse (V (Bool True)) (V (Int 13)) (V (Int 0))
