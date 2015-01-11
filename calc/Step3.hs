{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}

import Text.Parsec
import Control.Applicative hiding ((<|>))

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

data SNat :: Nat -> * where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data TaggedExpr where
  Tag :: SNat n -> Expr n -> TaggedExpr

addInt :: Int -> TaggedExpr -> TaggedExpr
addInt i (Tag n e) = Tag (SS n) (Int i e)

addBinOp :: BinOpTy -> TaggedExpr -> Maybe TaggedExpr
addBinOp op (Tag n e) = case n of
  SS (SS n) -> Just (Tag (SS n) (BinOp op e))
  _ -> Nothing

validexpr :: Parsec String m ValidExpr
validexpr = do
  Tag n e <- expr
  case n of
    SS SZ -> return (Valid e)
    _ -> fail "Expression doesn't result in a single value"

expr :: Parsec String m TaggedExpr
expr = go (Tag SZ Begin) where
  go :: TaggedExpr -> Parsec String m TaggedExpr
  go sofar = do
      i <- read <$> many1 digit
      spaces
      go (addInt i sofar)
    <|> do
      op <-  (char '+' *> return Plus)
         <|> (char '-' *> return Minus)
         <|> (char '*' *> return Times)
         <|> (char '/' *> return Divide)
      spaces
      case addBinOp op sofar of
        Nothing -> fail "Not enough operands on the stack"
        Just e -> go e
    <|> do
       eof
       return sofar
    

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

run :: String -> Either ParseError Int
run = fmap eval . parse (spaces >> validexpr) "(input)"

test :: ValidExpr
test = Valid $ Begin .: Int 10 .: Int 4 .: Int 3 .: BinOp Plus .: Int 2
                     .: BinOp Times .: BinOp Minus
  where
  (.:) = flip ($)

testStr :: String
testStr = "10 4 3 + 2 * -"
