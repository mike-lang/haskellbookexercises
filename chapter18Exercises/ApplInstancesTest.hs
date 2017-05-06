module ApplInstancesTest where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import ApplInstances

instance EqProp Bull where (=-=) = eq

main :: IO ()
main = do 
--  quickBatch (monoid Twoo)
  quickBatch (applicative (undefined :: ZipList' (String, Char, Int)))
  quickBatch (applicative (undefined :: Validation String (String, Char, Int)))
  quickBatch (applicative (undefined :: Pair (String, Char, Int)))
  quickBatch (applicative (undefined :: Two String (String, Char, Int)))
  quickBatch (applicative (undefined :: Three String [Int] (String, Char, Int)))
  quickBatch (applicative (undefined :: Three' String (String, Char, Int)))
  quickBatch (applicative (undefined :: Four String [Int] [Char] (String, Char, Int)))
  quickBatch (applicative (undefined :: Four' String (String, Char, Int)))
