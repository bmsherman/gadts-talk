{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Value = Int Int
  | BinOp BinOpTy
  | Accum AccumTy
  deriving Show

data BinOpTy = Plus | Minus | Times | Divide
  deriving Show

data AccumTy = Sum | Product
  deriving Show

type Expr = [Value]

eval :: Expr -> Int
eval e = head (eval' e) where
  eval' :: Expr -> [Int]
  eval' (BinOp f : xs) = case eval' xs of
     x : y : zs -> op f y x : zs
  eval' (Accum acc : xs) = [ accum acc (eval' xs) ]
  eval' (Int i : xs) = i : eval' xs
  eval' [] = []
  accum :: AccumTy -> [Int] -> Int
  accum Sum = sum
  accum Product = product
  op :: BinOpTy -> Int -> Int -> Int
  op Plus = (+)
  op Minus = (-)
  op Times = (*)
  op Divide = div

(.:) :: [a] -> a -> [a]
(.:) = flip (:)

test :: Expr
test = [] .: Int 10 .: Int 4 .: Int 3 .: BinOp Plus .: Int 2
          .: BinOp Times .: BinOp Minus
