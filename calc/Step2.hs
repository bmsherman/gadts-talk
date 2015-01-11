{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}

data Nat = Z | S Nat

infixl 5 :.
data Stack :: Nat -> * -> * where
  Nil :: Stack Z a
  (:.) :: Stack n a -> a -> Stack (S n) a

data BinOpTy = Plus | Minus | Times | Divide
  deriving Show

data Expr :: Nat -> * where
  Begin :: Expr Z
  Int :: Int -> Expr n -> Expr (S n)
  BinOp :: BinOpTy -> Expr (S (S n)) -> Expr (S n)

data ValidExpr = Valid (Expr (S Z))

eval :: ValidExpr -> Int
eval (Valid e) = case eval' e of
  Nil :. i -> i 
  where
  eval' :: Expr n -> Stack n Int
  eval' Begin = Nil
  eval' (BinOp f xs) = case eval' xs of
     zs :. y :. x -> zs :. op f y x
  eval' (Int x xs) = eval' xs :. x
  op :: BinOpTy -> Int -> Int -> Int
  op Plus = (+)
  op Minus = (-)
  op Times = (*)
  op Divide = div

test :: ValidExpr
test = Valid $ Begin .: Int 10 .: Int 4 .: Int 3 .: BinOp Plus .: Int 2
                     .: BinOp Times .: BinOp Minus
  where
  (.:) = flip ($)
