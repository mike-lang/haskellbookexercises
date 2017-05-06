module ApplInstances where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = 
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools


-- unfortunate orphan instances. Try to avoid these
-- in code you're going to keep or release

-- this isn't going to work properly
instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(1, elements [Nil]),
                         (5, Cons <$> arbitrary <*> arbitrary)]

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = (Cons (f x) (fmap f xs))

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

instance (Eq a) => EqProp (List a) where (=-=) = eq

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = (Cons x (take' (n-1) xs))

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

listFromZipList (ZipList' l) = l

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList'  where
  pure x = (ZipList' (repeat' x))
  (<*>) _ (ZipList' Nil) = (ZipList' Nil)
  (<*>) (ZipList' Nil) _ = (ZipList' Nil)
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) = (ZipList' (Cons (f x) tail))
    where tail = (listFromZipList ((ZipList' fs) <*> (ZipList' xs)))
  

repeat' x = (Cons x (repeat' x))

head' (ZipList' (Cons x xs)) = x
tail' (ZipList' (Cons x xs)) = (ZipList' xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary


data Errors = 
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (ApplInstances.Failure e) = ApplInstances.Failure e
  fmap f (ApplInstances.Success a) = (ApplInstances.Success (f a))

instance Monoid e =>
         Applicative (Validation e) where
  pure x = (ApplInstances.Success x)
  (<*>) (ApplInstances.Failure e) (ApplInstances.Failure e') = (ApplInstances.Failure (e `mappend` e'))
  (<*>) (ApplInstances.Success _) (ApplInstances.Failure e) = (ApplInstances.Failure e)
  (<*>) (ApplInstances.Failure e) (ApplInstances.Success _) = (ApplInstances.Failure e)
  (<*>) (ApplInstances.Success f) (ApplInstances.Success x) = (ApplInstances.Success (f x))

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, ApplInstances.Failure <$> arbitrary),
                         (1, ApplInstances.Success <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = (Pair (f x) (f y))
 
instance Applicative Pair where
  pure x = (Pair x x)
  (<*>) (Pair f f') (Pair x x') = Pair (f x) (f' x')

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary


data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = (Two x (f y))

instance (Monoid a) => Applicative (Two a) where
  pure x = (Two mempty x)
  (<*>) (Two x f) (Two x' y') = (Two (mappend x x') (f y'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq



data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = (Three x y (f z))

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = (Three mempty mempty x)
  (<*>) (Three x y f) (Three x' y' z') = (Three (mappend x x') (mappend y y') (f z'))

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq



data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = (Three' x (f y) (f z))

instance (Monoid a) => Applicative (Three' a) where
  pure x = (Three' mempty x x)
  (<*>) (Three' x f f') (Three' x' y' z') = (Three' (mappend x x') (f y') (f' z'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq




data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = (Four a b c (f d))

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = (Four mempty mempty mempty x)
  (<*>) (Four w x y f) (Four w' x' y' z') = (Four (mappend w w') (mappend x x') (mappend y y') (f z'))

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq




data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = (Four' w x y (f z))

instance (Monoid a) => Applicative (Four' a) where
  pure x = (Four' mempty mempty mempty x)
  (<*>) (Four' w x y f) (Four' w' x' y' z') = (Four' (mappend w w') (mappend x x') (mappend y y') (f z'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (,,)
