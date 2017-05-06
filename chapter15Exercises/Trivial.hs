module Trivial where

import Test.QuickCheck
import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial






newtype Identity a = Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = (Identity (x <> y))

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty 
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary






data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = (Two (x <> x') (y <> y'))

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
  mempty = (Two mempty mempty)
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = return Two <*> arbitrary <*> arbitrary






data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = (Three (x <> x') (y <> y') (z <> z'))

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = return Three <*> arbitrary <*> arbitrary <*> arbitrary






data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four v x y z) <> (Four v' x' y' z') = (Four (v <> v') (x <> x') (y <> y') (z <> z'))

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = return Four <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary





newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj arbitrary





newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary




data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = (Snd x)
  (Fst x) <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [fmap Fst arbitrary, fmap Snd arbitrary]

instance (Arbitrary a) => Arbitrary (Sum a) where
  arbitrary = fmap Sum arbitrary







newtype Combine a b = 
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (f) <> (g) = (Combine (\n -> ((unCombine f) n) <> ((unCombine g) n)))

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = (Combine (\n -> mempty))
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = fmap Combine arbitrary

instance Show (Combine a b) where
  show = \x -> "Some function in a Combine"






newtype Comp a = 
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  f <> g = (Comp ((unComp f) . (unComp g)))

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = fmap Comp arbitrary

instance Show (Comp a) where
  show = \x -> "Some function in a Comp"





data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Trivial.Failure x) <> _ = (Trivial.Failure x)
  _ <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [fmap Trivial.Failure arbitrary, fmap Trivial.Success arbitrary]





newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Trivial.Failure x)) <> _ = (AccumulateRight (Trivial.Failure x))
  _ <> (AccumulateRight (Trivial.Failure y)) = (AccumulateRight (Trivial.Failure y))
  (AccumulateRight (Trivial.Success x)) <> (AccumulateRight (Trivial.Success y)) = (AccumulateRight (Trivial.Success (x <> y)))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = fmap AccumulateRight arbitrary





newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Trivial.Failure x)) <> (AccumulateBoth (Trivial.Failure y)) = (AccumulateBoth (Trivial.Failure (x <> y)))
  l@(AccumulateBoth (Trivial.Failure x)) <> _ = l
  _ <> r@(AccumulateBoth (Trivial.Failure y)) = r
  (AccumulateBoth (Trivial.Success x)) <> (AccumulateBoth (Trivial.Success y)) = (AccumulateBoth (Trivial.Success (x <> y)))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = fmap AccumulateBoth arbitrary





newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  x <> y = Mem (\s -> 
    let memX = (runMem x) s
    in
      let memY = (runMem y) (snd memX)
       in ((fst memX) <> (fst memY), (snd memY)))


instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = fmap Mem arbitrary

instance Show (Mem s a) where
  show = \m -> "Some Mem function"


