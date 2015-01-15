{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Adapted from Tim Sheard's AdvancedFP class
-- http://web.cecs.pdx.edu/~sheard/course/AdvancedFP/code/GADTexamples.hs

import Data.Ratio

data Nat = Z | S Nat

data Balance :: Nat -> Nat -> Nat -> * where
  Same :: Balance n n n
  Less :: Balance n (S n) (S n)
  More :: Balance (S n) n (S n)

deriving instance Show (Balance l r c)

data Avl :: Nat -> * -> * where
  Tip  :: Avl Z a
  Node :: Balance l r c -> Avl l a -> a -> Avl r a -> Avl (S c) a

deriving instance Show a => Show (Avl n a)

data AVL :: * -> * where 
  AVL :: Avl h a -> AVL a

deriving instance Show a => Show (AVL a)

unreachable = error "Code that should never be executed was pulled on."  

insert :: Ord a => a -> AVL a -> AVL a
insert x (AVL t) = case ins x t of Left t -> AVL t; Right t -> AVL t

ins :: Ord a => a -> Avl n a -> Either (Avl n a) (Avl (S n) a)
ins x Tip = Right (Node Same Tip x Tip)
ins x (Node bal lc y rc)
  | x == y = Left (Node bal lc y rc)
  | x < y  = case ins x lc of
               Left  lc -> Left (Node bal lc y rc)
               Right lc ->
                 case bal of
                   Same -> Right (Node More lc y rc)
                   Less -> Left  (Node Same lc y rc)
                   More -> rotr lc y rc -- rebalance
  | otherwise  = case ins x rc of
               Left  rc -> Left (Node bal lc y rc)
               Right rc -> 
                 case bal of
                   Same -> Right (Node Less lc y rc)
                   More -> Left  (Node Same lc y rc)
                   Less -> rotl lc y rc -- rebalance

rotr :: Avl (S (S n)) a -> a -> Avl n a
     -> Either (Avl (S (S n)) a) (Avl (S (S (S n))) a)
-- rotr Tip u a = unreachable
rotr (Node Same b v c) u a = Right (Node Less b v (Node More c u a))
rotr (Node More b v c) u a = Left  (Node Same b v (Node Same c u a))
-- rotr (Node Less b v Tip) u a = unreachable
rotr (Node Less b v (Node Same x m y)) u a = 
  Left (Node Same (Node Same b v x) m (Node Same y u a))
rotr (Node Less b v (Node Less x m y)) u a =
  Left (Node Same (Node More b v x) m (Node Same y u a))
rotr (Node Less b v (Node More x m y)) u a =
  Left (Node Same (Node Same b v x) m (Node Less y u a))

rotl :: Avl n a -> a -> Avl (S (S n)) a
     -> Either (Avl (S (S n)) a) (Avl (S (S (S n))) a)
-- rotl a u Tip = unreachable
rotl a u (Node Same b v c) = Right (Node More (Node Less a u b) v c)
rotl a u (Node Less b v c) = Left  (Node Same (Node Same a u b) v c)
-- rotl a u (Node More Tip v c) = unreachable
rotl a u (Node More (Node Same x m y) v c) =
  Left (Node Same (Node Same a u x) m (Node Same y v c))
rotl a u (Node More (Node Less x m y) v c) =
  Left (Node Same (Node More a u x) m (Node Same y v c))
rotl a u (Node More (Node More x m y) v c) = 
  Left (Node Same (Node Same a u x) m (Node Less y v c))


delMin :: Avl (S n) a -> (a, Either (Avl n a) (Avl (S n) a))
-- delMin Tip = unreachable
delMin (Node Less Tip x r) = (x,Left r)
delMin (Node Same Tip x r) = (x,Left r)
-- delMin (Node More Tip x r) = unreachable
delMin (Node bal (l@(Node _ _ _ _)) x r) = 
      case delMin l of
        (y, Right l) -> (y, Right (Node bal l x r))
        (y, Left  l) ->
          case bal of
            Same -> (y, Right (Node Less l x r))
            More -> (y, Left  (Node Same l x r))
            Less -> (y, rotl l x r)

--
-- Validation code
--

data SNat :: Nat -> * where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
            
data TaggedAVL :: * -> * where
  Tag :: SNat h -> Avl h a -> TaggedAVL a

data Tree a = Nil | Branch (Tree a) a (Tree a)
  deriving Show

data Balanced :: Nat -> Nat -> * where
  Balanced :: SNat c -> Balance l r c -> Balanced l r

increase :: Balance l r c -> Balance (S l) (S r) (S c)
increase bal = case bal of
  Same -> Same
  Less -> Less
  More -> More

balance :: SNat l -> SNat r -> Maybe (Balanced l r)
balance     SZ      SZ  = Just (Balanced     SZ  Same)
balance     SZ  (SS SZ) = Just (Balanced (SS SZ) Less)
balance (SS SZ)     SZ  = Just (Balanced (SS SZ) More)
balance (SS l ) (SS r ) = case balance l r of
  Just (Balanced c bal) -> Just $ Balanced (SS c) (increase bal)
  Nothing               -> Nothing
balance     _       _   = Nothing

toAVL :: Tree a -> Maybe (AVL a)
toAVL = fmap convert . toAVL' where
  convert :: TaggedAVL a -> AVL a
  convert (Tag _ avl) = AVL avl

toAVL' :: Tree a -> Maybe (TaggedAVL a)
toAVL' Nil = return (Tag SZ Tip)
toAVL' (Branch l x r) = do
  Tag hl ll <- toAVL' l
  Tag hr rr <- toAVL' r
  Balanced cc bal <- balance hl hr
  return (Tag (SS cc) (Node bal ll x rr))

myTree :: Tree Rational
myTree = go (1 % 2) (1 % 2) where
  go x w = let w' = w / 2 in Branch (go (x - w') w') x (go (x + w') w')

takeHeight :: Int -> Tree a -> Tree a
takeHeight 0 _ = Nil
takeHeight n t = case t of
  Nil -> Nil
  Branch l x r -> Branch (takeHeight (n - 1) l) x (takeHeight (n - 1) r)
