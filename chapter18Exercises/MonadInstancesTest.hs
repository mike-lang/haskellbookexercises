module MonadInstancesTest where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import MonadInstances

main :: IO ()
main = do
  quickBatch (functor (undefined :: Nope (String, Char, Int)))
  quickBatch (applicative (undefined :: Nope (String, Char, Int)))
  quickBatch (monad (undefined :: Nope (String, Char, Int)))
  quickBatch (functor (undefined :: PhhhbbtttEither String (String, Char, Int)))
  quickBatch (applicative (undefined :: PhhhbbtttEither String (String, Char, Int)))
  quickBatch (monad (undefined :: PhhhbbtttEither String (String, Char, Int)))
  quickBatch (functor (undefined :: Identity (String, Char, Int)))
  quickBatch (applicative (undefined  :: Identity (String, Char, Int)))
  quickBatch (monad (undefined :: Identity (String, Char, Int)))
  quickBatch (functor (undefined :: List (String, Char, Int)))
  quickBatch (applicative (undefined :: List (String, Char, Int)))
  quickBatch (monad (undefined :: List (String, Char, Int)))
