{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Text.Parsec
import Control.Applicative hiding ((<|>), Const, many)
import Data.Type.Equality ((:~:) (..))
import Data.Proxy (Proxy (..))

import GHC.TypeLits (Symbol (..), sameSymbol, KnownSymbol (..)
  , someSymbolVal, SomeSymbol (..))

import Unsafe.Coerce (unsafeCoerce)

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

data VarIdx :: [(Symbol, Ty)] -> Ty -> * where
  V :: KnownSymbol s => Proxy s -> VarIdx env (Get s env)
deriving instance Show (VarIdx env a)

data Expr :: [(Symbol, Ty)] -> Ty -> * where
  Const :: Value ty -> Expr as ty
  Lam   :: KnownSymbol s => Proxy s -> STy lamTy -> Expr ('(s, lamTy) ': as) resTy -> Expr as (Arr lamTy resTy)
  Var   :: VarIdx as b -> Expr as b
  App   :: Expr as (Arr a b) -> Expr as a -> Expr as b

deriving instance Show (Expr env a)

data TyList :: (Ty -> *) -> [(Symbol, Ty)] -> * where
  Nil  :: TyList f '[]
  (:.) :: KnownSymbol str => (Proxy str, f a) -> TyList f as -> TyList f ('(str, a) ': as)

type family Get (k :: Symbol) (xs :: [(Symbol, Ty)]) :: Ty where
  Get k ( '(k, ty) ': xs) = ty
  Get k ( '(l, ty) ': xs) = Get k xs

data Gotten :: Symbol -> (Ty -> *) -> [(Symbol, Ty)] -> * where
  Gotten :: Get k env :~: gotten -> f gotten -> Gotten k f env


-- We need to use unsafeCoerce here because of Haskell's inability
-- to deal with disequality constraints
doGet :: KnownSymbol k => Proxy k -> TyList f env 
      -> Maybe (Gotten k f env)
doGet s Nil = Nothing
doGet s ((s', v) :. xs) = case sameSymbol s s' of
  Just Refl -> Just (Gotten Refl v)
  Nothing -> case doGet s xs of
    Just (Gotten Refl v') -> Just (Gotten (unsafeCoerce Refl) v')
    Nothing -> Nothing

type Env = TyList Value

data Tagged f env where
  Tag :: STy a -> f env a -> Tagged f env

deriving instance Show (Tagged Expr env)

index :: Env tys -> VarIdx tys ty -> Value ty
index env (V s) = case doGet s env of
  Just (Gotten Refl val) -> val
  Nothing -> undefined

eval :: Expr '[] ty -> Value ty
eval e = eval' Nil e
  where
  eval' :: Env env -> Expr env ty  -> Value ty
  eval' env e = case e of
    Const v -> v
    Lam s ty e -> Func (\x -> eval' ((s, x) :. env) e)
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
    name <- lexeme variable
    tok ":"
    Some argTy <- tyExpr
    case someSymbolVal name of
      SomeSymbol s -> do
        Tag retTy e <- expr ((s, argTy) :. env)
        return $ Tag (SArrTy argTy retTy) (Lam s argTy e)
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
    n <- lexeme variable
    case someSymbolVal n of
      SomeSymbol s -> case doGet s env of
        Just (Gotten Refl ty) -> return $ Tag ty (Var (V s))
        Nothing -> fail "variable name not in scope"
  <|> do
    op <- lexeme (char '+') *> return Plus
      <|> lexeme (char '-') *> return Minus
      <|> lexeme (char '*') *> return Times
      <|> lexeme (char '/') *> return Divide
    return (Tag intBinOpTy (intBinOp op))
  <|> tok "(" *> lexeme (expr env) <* string ")"
  where
  variable = (:) <$> lower <*> many alphaNum

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
  add3 = Lam varname SIntTy (App (App (intBinOp Plus) (Const (Int 3))) (Var (V varname2)))
  varname :: Proxy "x"
  varname = Proxy
  varname2 :: Proxy "x"
  varname2 = Proxy

testStr :: String
testStr = "app (\\x:Int (app (app * 3) x)) 5"
