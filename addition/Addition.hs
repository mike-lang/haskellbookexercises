-- Addition.hs
module Addition where


import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      1 + 1 > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Multiplication" $ do
    it "2 times 3 is 6" $ do
      rMult 2 3 `shouldBe` 6
    it "9 times 6" $ do
      rMult 9 6 `shouldBe` 54


sayHello :: IO ()
sayHello = putStrLn "hello!"


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)


rMult :: (Eq a, Num a) => a -> a -> a
rMult m n = go m n 0
  where go m n prod
         | n == 0 = prod
         | otherwise = go m (n - 1) (prod + m)


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
