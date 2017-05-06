{-# LANGUAGE TypeSynonymInstances #-}

module WordNumberTest where


import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)
import Test.QuickCheck
import Data.List (sort)


half :: Double -> Double
half x = x / 2

halfIdentity = (*2) . half

prop_halfComposedWithInverseCreatesIdentity =
  \x -> x == (halfIdentity x)


-- for any list you apply sort to 
-- this property should hold

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


prop_listOrderedAfterSort :: [Char] -> Bool
prop_listOrderedAfterSort =
  \x -> listOrdered (sort x)

plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y = 
  x + y == y + x


prop_intAdditionAssociates :: Int -> Int -> Int -> Bool
prop_intAdditionAssociates = plusAssociative

prop_intAdditionCommutes :: Int -> Int -> Bool
prop_intAdditionCommutes = plusCommutative




--prop_quotRem :: Int -> Int -> Bool
--prop_quotRem x y = (quot x y)*y + (rem x y) == x

nonZeroQuotRemWorks :: Integer -> NonZero Integer -> Bool
nonZeroQuotRemWorks n (NonZero d) = (quot n d)*d + (rem n d) == n 


prop_quotRem :: Property
prop_quotRem =
  forAll (arbitrary :: (Gen Integer)) 
    ( \n -> (
      forAll (arbitrary :: Gen (NonZero Integer)) 
        (\d -> nonZeroQuotRemWorks n d)
      )
    )

nonZeroDivModWorks :: Integer -> NonZero Integer -> Bool
nonZeroDivModWorks n (NonZero d) = (div n d)*d + (mod n d) == n

prop_divMod :: Property
prop_divMod =
  forAll (arbitrary :: (Gen Integer))
    ( \n -> (
      forAll (arbitrary :: Gen (NonZero Integer))
        (\d -> nonZeroDivModWorks n d)
      )
    )

multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative x y =
  x * y == y * x

prop_intMultAssociates :: Int -> Int -> Int -> Bool
prop_intMultAssociates = multAssociative

prop_intMultCommutes :: Int -> Int -> Bool
prop_intMultCommutes = multCommutative

expAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_intExpAssociates :: Int -> Int -> Int -> Bool
prop_intExpAssociates = expAssociative

expCommutative x y =
  x ^ y == y ^ x

prop_intExpCommutes :: Int -> Int -> Bool
prop_intExpCommutes = expCommutative

prop_reverseSelfComposesToIdentity :: [Int] -> Bool
prop_reverseSelfComposesToIdentity = \x -> (reverse . reverse) x == id x

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]


  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100
        `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001
        `shouldBe` "nine-zero-zero-one"


doQuickCheck :: IO ()
doQuickCheck = do
  quickCheck prop_halfComposedWithInverseCreatesIdentity
  quickCheck prop_listOrderedAfterSort 
  quickCheck prop_intAdditionAssociates
  quickCheck prop_intAdditionCommutes 
  quickCheck prop_intMultAssociates 
  quickCheck prop_intMultCommutes 
  quickCheck prop_quotRem 
  quickCheck prop_divMod
--  quickCheck prop_intExpAssociates 
--  quickCheck prop_intExpCommutes 
  quickCheck prop_reverseSelfComposesToIdentity 
