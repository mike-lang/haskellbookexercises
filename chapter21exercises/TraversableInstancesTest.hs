{-# LANGUAGE FlexibleInstances #-}

module TraversableInstancesTest where

import Test.QuickCheck.Property
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import TraversableInstances

instance (Eq a) => EqProp (Identity a) where (=-=) = eq
instance (Eq a) => EqProp (Constant a b) where (=-=) = eq
instance (Eq a) => EqProp (Optional a) where (=-=) = eq
instance (Eq a) => EqProp (List a) where 
  (=-=) l1 l2 = eq (take' 100 l1) (take' 100 l2)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Eq a) => EqProp (S [] a) where
  (=-=) (S l1 x) (S l2 x') = (eq (take 100 l1) (take 100 l2)) .&&. (eq x x')

instance (Eq a) => EqProp (Tree a) where (=-=) = eq

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = (Cons x (take' (n-1) xs))

main :: IO ()
main = do
  quickBatch (functor (undefined :: Identity (String, Char, Int))) 
  quickBatch (applicative (undefined :: Identity (String, Char, Int)))
  quickBatch (traversable (undefined :: Identity (String, Char, [Int])))
  quickBatch (functor (undefined :: Constant Int (String, Char, Int)))
  quickBatch (applicative (undefined :: Constant [Int] (String, Char, Int)))
  quickBatch (traversable (undefined :: Constant Int (String, Char, [Int])))
  quickBatch (functor (undefined :: Optional (String, Char, Int)))
  quickBatch (applicative (undefined :: Optional (String, Char, Int)))
  quickBatch (traversable (undefined :: Optional (String, Char, [Int])))
  quickBatch (functor (undefined :: List (String, Char, Int)))
  quickBatch (applicative (undefined :: List (String, Char, Int)))
  quickBatch (traversable (undefined :: List (String, Char, [Int])))
  quickBatch (functor (undefined :: Three [Char] [Int] (String, Char, Int)))
  quickBatch (applicative (undefined :: Three [Char] [Int] (String, Char, Int)))
  quickBatch (traversable (undefined :: Three [Char] [Int] (String, Char, [Int])))
  quickBatch (functor (undefined :: S [] (String, Char, [Int]))) 
  quickBatch (traversable (undefined :: S [] (String, Char, [Int])))
  quickBatch (functor (undefined :: Tree (String, Char, [Int])))
  quickBatch (traversable (undefined :: Tree (String, Char, [Int])))
