module MonadInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ x = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = elements [NopeDotJpg]

instance EqProp (Nope a) where (=-=) = eq



data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (MonadInstances.Right x) = (MonadInstances.Right x)
  fmap f (MonadInstances.Left y) = (MonadInstances.Left (f y))

instance Applicative (PhhhbbtttEither b) where
  pure x = (MonadInstances.Left x)
  (<*>) _ (MonadInstances.Right y) = (MonadInstances.Right y)
  (<*>) (MonadInstances.Right y) (MonadInstances.Left _) = (MonadInstances.Right y)
  (<*>) (MonadInstances.Left f) (MonadInstances.Left y) = (MonadInstances.Left (f y))

instance (Monoid b) => Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (MonadInstances.Right x) _ = (MonadInstances.Right x)
  (>>=) (MonadInstances.Left y) f = f y
 
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [(1, MonadInstances.Left <$> arbitrary), (1, MonadInstances.Right <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq



newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = (Identity (f x))

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = (Identity (f x))

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq



data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = (Cons (f x) (f <$> xs))

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil     = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure x = (Cons x Nil)
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) fs vs = flatMap (\f -> fmap f vs) fs

instance Monad List where
  return = pure
  (>>=) = flip flatMap

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(1, elements [Nil]), (5, Cons <$> arbitrary <*> arbitrary)]

instance (Eq a) => EqProp (List a) where (=-=) = eq



j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = m >>= (return . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m m' = (l1 f m) <*> m'

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs mf = foldl (\acc x -> (l2 (++) (acc) (l1 (pure :: a -> [a]) (mf x)))) (pure []) xs 

flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id

-- When talking in regards to the Monad instance,
-- "bind" will refer to having used >>= to lift a monadic
-- function over the structure.
--
-- Since I like thinking about the promise monad,
-- .then is a bind operation for the promise monad.
-- since it takes a function that returns a promise 
-- (there's some sugar to wrap a return value in a promise if its not already)
-- and monadically combines that function with the rest of the chain
-- the combined structure then returns a promise with the type of thing
-- within the promise returned by that last function within it.
--
-- I suppose join must not be implementable in javascript for the promise monad, 
-- because the value only arrives if you yield to the event loop, but there isn't
-- a way to do that with javascript single threaded model.
--
