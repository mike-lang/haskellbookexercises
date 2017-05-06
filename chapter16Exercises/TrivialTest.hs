module TrivialTest where

import Test.QuickCheck
import Data.Semigroup
import Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Rather than try to write an Eq instance for a -> b, 
-- I'll add another parameter to the property and check
-- that the functions produced by <> produce the same value
-- when applied regardless of how the <> associate
combineAssoc :: (Eq b, Semigroup b) => (Combine a b) -> (Combine a b) -> (Combine a b) -> a -> Bool
combineAssoc f g h x = ((unCombine (f <> (g <> h))) $ x) == ((unCombine ((f <> g) <> h)) $ x)

-- Again we'll do this like this instead of trying to 
-- deal with writing an Eq instance for functions
compAssoc :: (Eq a) => (Comp a) -> (Comp a) -> (Comp a) -> a -> Bool
compAssoc f g h x = ((unComp (f <> (g <> h))) $ x) == ((unComp ((f <> g) <> h)) $ x)

memAssoc :: (Eq s, Eq a, Semigroup a) => (Mem s a) -> (Mem s a) -> (Mem s a) -> s -> Bool
memAssoc f g h s = ((runMem (f <> (g <> h))) $ s) == ((runMem ((f <> g) <> h)) $ s)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mappend mempty a) == a

combineLeftIdentity :: (Eq b, Semigroup b, Monoid b) => (Combine a b) -> a -> Bool
combineLeftIdentity c x = (unCombine (mappend mempty c)) x == (unCombine c) x

compLeftIdentity :: (Eq a) => (Comp a) -> a -> Bool
compLeftIdentity c x = (unComp (mappend mempty c)) x == (unComp c) x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mappend a mempty) == a

combineRightIdentity :: (Eq b, Semigroup b, Monoid b) => (Combine a b) -> a -> Bool
combineRightIdentity c x = (unCombine (mappend c mempty)) x == (unCombine c) x

compRightIdentity :: (Eq a) => (Comp a) -> a -> Bool
compRightIdentity c x = (unComp (mappend mempty c)) x == (unComp c) x

memLeftIdentity :: (Eq s, Eq a, Semigroup a, Monoid a) => (Mem s a) -> s -> Bool
memLeftIdentity m s = (runMem (mappend mempty m)) s == (runMem m) s

memRightIdentity :: (Eq s, Eq a, Semigroup a, Monoid a) => (Mem s a) -> s -> Bool
memRightIdentity m s = (runMem (mappend m mempty)) s == (runMem m) s


type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool
type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool
type ThreeAssoc a b c = (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool
type FourAssoc a b c d = (Four a b c d) -> (Four a b c d) -> (Four a b c d) -> Bool
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool
type CombineAssoc a b = (Combine a b) -> (Combine a b) -> (Combine a b) -> a -> Bool
type CompAssoc a = (Comp a) -> (Comp a) -> (Comp a) -> a -> Bool
type ValidationAssoc a b = (Validation a b) -> (Validation a b) -> (Validation a b) -> Bool
type AccumulateRightAssoc a b = (AccumulateRight a b) -> (AccumulateRight a b) -> (AccumulateRight a b) -> Bool
type AccumulateBothAssoc a b = (AccumulateBoth a b) -> (AccumulateBoth a b) -> (AccumulateBoth a b) -> Bool
type MemAssoc s a = (Mem s a) -> (Mem s a) -> (Mem s a) -> s -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: (IdentityAssoc [Int]))
  quickCheck (monoidLeftIdentity :: (Identity [Int]) -> Bool)
  quickCheck (monoidRightIdentity :: (Identity [Int]) -> Bool)
  quickCheck (semigroupAssoc :: (TwoAssoc [Int] [String]))
  quickCheck (monoidLeftIdentity :: (Two [Int] [String]) -> Bool)
  quickCheck (monoidRightIdentity :: (Two [Int] [String]) -> Bool)
  quickCheck (semigroupAssoc :: (ThreeAssoc [Int] [Char] [String]))
  quickCheck (semigroupAssoc :: (FourAssoc [Int] [Ordering] [Char] [String]))
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: (OrAssoc Int String))
  quickCheck (combineAssoc :: (CombineAssoc Int (Sum Int)))
  quickCheck (combineLeftIdentity :: (Combine Int (Sum Int)) -> Int -> Bool)
  quickCheck (combineRightIdentity :: (Combine Int (Sum Int)) -> Int -> Bool)
  quickCheck (compAssoc :: (CompAssoc Int))
  quickCheck (compLeftIdentity :: (Comp Int) -> Int -> Bool)
  quickCheck (compRightIdentity :: (Comp Int) -> Int -> Bool)
  quickCheck (semigroupAssoc :: (ValidationAssoc [Int] String))
  quickCheck (semigroupAssoc :: (AccumulateRightAssoc [Int] String))
  quickCheck (semigroupAssoc :: (AccumulateBothAssoc [Int] String))
  quickCheck (memAssoc :: (MemAssoc Int String))
  quickCheck (memLeftIdentity :: (Mem Int String) -> Int -> Bool)
  quickCheck (memRightIdentity :: (Mem Int String) -> Int -> Bool)

f' :: (Mem Int String)
f' = Mem $ \s -> ("hi", s + 1)

simpleRunMemCheck :: IO ()
simpleRunMemCheck = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
