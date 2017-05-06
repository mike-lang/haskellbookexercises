module FoldableInstances where

import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = getAny . foldMap (\x -> Any (x==e)) 


maybeLess :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeLess Nothing m' = m'
maybeLess m Nothing = m
maybeLess (Just x) (Just x') = if (x < x') then (Just x) else (Just x')

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = foldr (\x acc -> maybeLess (Just x) acc) Nothing xs

maybeMore :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMore Nothing m' = m'
maybeMore m Nothing = m
maybeMore (Just x) (Just x') = if (x > x') then (Just x) else (Just x')

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = foldr (\x acc -> maybeMore (Just x) acc) Nothing xs

null :: (Foldable t) => t a -> Bool
null = foldr (\x acc -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\x acc -> acc + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (\x acc -> x : acc) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x `mappend` acc) mempty


data Constant a b =
  Constant a
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f acc (Constant x) = acc

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two x y) = f y

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = (f y) `mappend` (f z)

data Four' a b =
  Four' a b b b 
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' x y z z') = mconcat [(f y), (f z), (f z')]

filterF :: (Applicative f, Foldable t, Monoid (f a))
       => (a -> Bool) -> t a -> f a



filterF p xs = foldMap (\x -> if (p x) then (pure x) else mempty) xs

