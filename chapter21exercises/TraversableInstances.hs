{-# LANGUAGE FlexibleInstances #-}

module TraversableInstances where

import Test.QuickCheck

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = (Identity (f x))

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = (Identity (f x))

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary



newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap _ (Constant x) = (Constant x)

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance (Monoid a) => Applicative (Constant a) where
  pure _ = (Constant mempty)
  (<*>) (Constant x) (Constant y) = (Constant (x `mappend` y))

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary



data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = (Yep (f x))

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Applicative Optional where
  pure = Yep
  (<*>) Nada _ = Nada
  (<*>) _ Nada = Nada
  (<*>) (Yep f) (Yep x) = (Yep (f x))

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = (Yep <$> (f x))

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [(1, elements [Nada]), (5, Yep <$> arbitrary)]



data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = (Cons (f x) (f <$> xs))

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) `mappend` (foldMap f xs)

instance Applicative List where
  pure x = Cons x (pure x) 
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (Cons x xs) = (Cons (f x) (fs <*> xs))

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(1, elements [Nil]), (5, Cons <$> arbitrary <*> arbitrary)]



data Three a b c =
  Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = (Three x y (f z))

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three x y f) (Three x' y' z) = Three (x `mappend` x') (y `mappend` y') (f z)

instance Traversable (Three a b) where
  traverse f (Three x y z) = (Three x y) <$> (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary



data Three' a b = 
  Three' a b b
  deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z)  = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = (f y) `mappend` (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x f1 f2) (Three' x' y' z') = Three' (x `mappend` x') (f1 y') (f2 z')

instance Traversable (Three' a) where
  traverse f (Three' x y z) = (Three' x) <$> (f y) <*> (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary



data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n x) = S (f <$> n) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S n x) = (foldMap f n) `mappend` (f x)

instance Traversable n => Traversable (S n) where
  traverse f (S n x) = S <$> traverse f n <*> f x

instance Arbitrary a => Arbitrary (S [] a) where
  arbitrary = S <$> arbitrary <*> arbitrary
  


data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node tl x tr) = Node (f <$> tl) (f x) (f <$> tr)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node tl x tr) = (foldMap f tl) `mappend` (f x) `mappend` (foldMap f tr)

-- instance Foldable Tree where
--   foldr _ b Empty = b
--   foldr f b (Leaf x) = f x b
--   foldr f b (Node tl x tr) = foldr f acc tr
--                              where acc = foldr f (f x b) tl
--

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> (f x)
  traverse f (Node tl x tr) = Node <$> (traverse f tl) <*> (f x) <*> (traverse f tr)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency [(1, elements [Empty]), (2, Leaf <$> arbitrary), (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)]

