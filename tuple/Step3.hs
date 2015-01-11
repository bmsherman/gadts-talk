{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative hiding ((<|>))
import Data.Type.Equality ((:~:) (..))
import Text.Parsec

data Tag a where
  Int :: Tag Int
  Bool :: Tag Bool
  Tuple :: Tag a -> Tag b -> Tag (a, b)

deriving instance Show (Tag a)

data Value a = V (Tag a) a

instance Show (Value a) where
  show (V Int i) = show i
  show (V Bool b) = show b
  show (V (Tuple t1 t2) (u, v)) = "(" ++ show (V t1 u) 
    ++ ", " ++ show (V t2 v) ++ ")"

data Expr a where
  Val        :: Value a -> Expr a
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a
  MkTuple    :: Expr a -> Expr b -> Expr (a, b)
  First      :: Expr (a, b) -> Expr a
  Second     :: Expr (a, b) -> Expr b
  Equal      :: Expr a -> Expr a -> Expr Bool

eqValue :: Value a -> Value a -> Bool
eqValue (V Int i) (V Int j) = i == j
eqValue (V Bool b1) (V Bool b2) = b1 == b2
eqValue (V (Tuple t1 t2) (u1, u2)) (V (Tuple t1' t2') (v1, v2)) = 
  V t1 u1 `eqValue` V t1' v1 && V t2 u2 `eqValue` V t2' v2

eval :: Expr a -> Value a
eval (Val v) = v
eval (IfThenElse e t f) = case eval e of
  V Bool tf -> if tf then eval t else eval f
eval (MkTuple e1 e2) = case (eval e1, eval e2) of
  (V t1 v1, V t2 v2) -> V (Tuple t1 t2) (v1, v2)
eval (First e) = case eval e of
  V (Tuple t1 t2) (v1, v2) -> V t1 v1
eval (Second e) = case eval e of
  V (Tuple t1 t2) (v1, v2) -> V t2 v2
eval (Equal e1 e2) = V Bool (eqValue (eval e1) (eval e2))

data Tagged f where
  Tag :: Tag a -> f a -> Tagged f

lexeme :: Parsec String m a -> Parsec String m a
lexeme p = p <* spaces

tok :: String -> Parsec String m String
tok = lexeme . string

tagDecEq :: Tag a -> Tag b -> Maybe (a :~: b)
tagDecEq Int Int = Just Refl
tagDecEq Bool Bool = Just Refl
tagDecEq (Tuple t1 t2) (Tuple t1' t2') = 
  case (tagDecEq t1 t1', tagDecEq t2 t2') of
    (Just Refl, Just Refl) -> Just Refl
    _                      -> Nothing
tagDecEq _ _ = Nothing


value :: Parsec String m (Tagged Value)
value = 
      Tag Int . V Int . read <$> many1 digit
  <|> pure (Tag Bool (V Bool True)) <* try (string "true")
  <|> pure (Tag Bool (V Bool False)) <* try (string "false")

expr :: Parsec String m (Tagged Expr)
expr = do
    v <- value
    return $ case v of
      Tag t v -> Tag t (Val v)
  <|> do
    tok "if"
    cond <- lexeme expr
    case cond of
      Tag Bool b -> do
        tok "then"
        t <- lexeme expr
        tok "else"
        f <- expr
        case (t, f) of
          (Tag t1 e1, Tag t2 e2) -> 
            case tagDecEq t1 t2 of
              Just Refl -> return $ Tag t1 (IfThenElse b e1 e2)
              Nothing   -> fail ("then branch has type " ++ show t1 ++
                 ", but else branch has type " ++ show t2)
      Tag ty   _ -> fail ("if-then-else requires boolean, got " ++ show ty)
  <|> do
    tok "Tuple"
    Tag t1 e1 <- lexeme expr
    Tag t2 e2 <- lexeme expr
    return (Tag (Tuple t1 t2) (MkTuple e1 e2))
  <|> do 
    tok "first"
    Tag t e <- expr
    case t of
      Tuple t1 t2 -> return (Tag t1 (First e))
      _ -> fail ("Can't apply `first` to type " ++ show t)
  <|> do 
    tok "second"
    Tag t e <- expr
    case t of
      Tuple t1 t2 -> return (Tag t2 (Second e))
      _ -> fail ("Can't apply `second` to type " ++ show t)
  <|> do
    tok "equal"
    Tag t1 e1 <- lexeme expr
    Tag t2 e2 <- expr
    case tagDecEq t1 t2 of
      Just Refl -> return (Tag Bool (Equal e1 e2))
      Nothing -> fail ("Can't compare value of type " ++ show t1 ++
        " with value of type " ++ show t2)
  <|> tok "(" *> lexeme expr <* string ")"

instance Show (Tagged Value) where
  show (Tag t v) = show v

run :: String -> Either ParseError (Tagged Value)
run = fmap evalTagged . parse (lexeme expr) "(input)"
  where
  evalTagged :: Tagged Expr -> Tagged Value
  evalTagged (Tag t e) = Tag t (eval e)

test :: Either ParseError (Tagged Value)
test = run $ "if equal (Tuple 1 false) (Tuple 15 true) "
   ++ "then 12 else second (Tuple false 0)"
