{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}

import Text.Parsec
import Control.Applicative hiding ((<|>), Const)
import Data.Type.Equality ((:~:) (..))

data Ty = IntTy | BoolTy | Arr Ty Ty

data STy :: Ty -> * where
  SIntTy :: STy IntTy
  SBoolTy :: STy BoolTy
  SArrTy :: STy a -> STy b -> STy (Arr a b)

deriving instance Show (STy ty)

eqTy :: STy a -> STy b -> Maybe (a :~: b)
eqTy SIntTy SIntTy = Just Refl
eqTy SBoolTy SBoolTy = Just Refl
eqTy (SArrTy a b) (SArrTy a' b') = case (a `eqTy` a', b `eqTy` b') of
  (Just Refl, Just Refl) -> Just Refl
  _ -> Nothing
eqTy _ _ = Nothing

data Value :: Ty -> * where
  Int :: Int -> Value IntTy
  Bool :: Bool -> Value BoolTy
  Func :: (Value a -> Value b) -> Value (Arr a b)

data TaggedValue where
  TagV :: STy ty -> Value ty -> TaggedValue

deriving instance Show TaggedValue

data Some f where
  Some :: f a -> Some f

instance Show (Value a) where
  show v = case v of
    Int i -> show i
    Bool b -> show b
    Func _ -> "<<function>>"

data VarIdx :: [Ty] -> Ty -> * where
  Z :: VarIdx (a ': as) a
  S :: VarIdx as b -> VarIdx (a ': as) b
deriving instance Show (VarIdx env a)

data Expr :: [Ty] -> Ty -> * where
  Const :: Value ty -> Expr as ty
  Lam   :: STy lamTy -> Expr (lamTy ': as) resTy -> Expr as (Arr lamTy resTy)
  Var   :: VarIdx as b -> Expr as b
  App   :: Expr as (Arr a b) -> Expr as a -> Expr as b

deriving instance Show (Expr env a)

data TyList :: (Ty -> *) -> [Ty] -> * where
  Nil  :: TyList f '[]
  (:.) :: f a -> TyList f as -> TyList f (a ': as)

type Env = TyList Value

data Tagged f env where
  Tag :: STy a -> f env a -> Tagged f env

deriving instance Show (Tagged Expr env)

index :: Env tys -> VarIdx tys ty -> Value ty
index (x :. _ ) Z     = x
index (_ :. xs) (S n) = index xs n

mkVarIdx :: TyList STy as -> Int -> Maybe (Tagged VarIdx as)
mkVarIdx Nil _ = Nothing
mkVarIdx _  i | i < 0 = Nothing
mkVarIdx (a :. _  ) 0 = Just (Tag a Z)
mkVarIdx (a :. as) i = case mkVarIdx as (i - 1) of
  Just (Tag ty n) -> Just (Tag ty (S n))
  Nothing -> Nothing

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

lexeme :: Parsec String m a -> Parsec String m a
lexeme p = p <* spaces

tok :: String -> Parsec String m String
tok = lexeme . string

value :: Parsec String m TaggedValue
value = (TagV SIntTy . Int . read <$> many1 digit)
  <|>   (TagV SBoolTy . Bool      <$> (     (tok "true" *> return True)
                                  <|> (tok "false" *> return False)))


tyExpr :: Parsec String m (Some STy)
tyExpr = tok "Int"  *> return (Some SIntTy)
  <|>    tok "Bool" *> return (Some SBoolTy)
  <|> do
    tok "->"
    Some a <- tyExpr
    Some b <- tyExpr
    return (Some (SArrTy a b))
  <|> tok "(" *> lexeme tyExpr <* string ")"

expr :: TyList STy env -> Parsec String m (Tagged Expr env)
expr env = do
    TagV ty v <- value
    return $ Tag ty (Const v)
  <|> do
    tok "\\"
    Some argTy <- tyExpr
    Tag retTy e <- expr (argTy :. env)
    return $ Tag (SArrTy argTy retTy) (Lam argTy e)
  <|> do
    tok "app"
    Tag fTy f <- lexeme (expr env)
    case fTy of
      SArrTy a b -> do
        Tag xTy x <- lexeme (expr env)
        case a `eqTy` xTy of
          Just Refl -> return $ Tag b (App f x)
          Nothing -> fail "Can't match types in function application"
      _ -> fail "Can't apply non-function type as function"
  <|> do
    char 'v'
    i <- read <$> many1 digit
    case mkVarIdx env i of
      Just (Tag ty n) -> return (Tag ty (Var n))
      Nothing -> fail "Invalid index"
  <|> do
    op <- lexeme (char '+') *> return Plus
      <|> lexeme (char '-') *> return Minus
      <|> lexeme (char '*') *> return Times
      <|> lexeme (char '/') *> return Divide
    return (Tag intBinOpTy (intBinOp op))
  <|> tok "(" *> lexeme (expr env) <* string ")"

parseExpr :: String -> Either ParseError (Tagged Expr '[])
parseExpr = parse (expr Nil) "(input)"

evalTagged :: Tagged Expr '[] -> TaggedValue
evalTagged (Tag ty e) = TagV ty (eval e)

intBinOpTy :: STy (Arr IntTy (Arr IntTy IntTy))
intBinOpTy = SArrTy SIntTy (SArrTy SIntTy SIntTy)

data IntBinOp = Plus | Minus | Times | Divide

intBinOp :: IntBinOp -> Expr env (Arr IntTy (Arr IntTy IntTy))
intBinOp o = Const (Func (\(Int x) -> Func (\(Int y) -> Int (op o x y)))) 
  where
  op Plus = (+)
  op Minus = (-)
  op Times = (*)
  op Divide = div

test :: Expr '[] IntTy
test = App add3 (Const (Int 5))
  where
  add3 = Lam SIntTy (App (App (intBinOp Plus) (Const (Int 3))) (Var Z))

testStr :: String
testStr = "app (\\Int (app (app * 3) v0)) 5"
