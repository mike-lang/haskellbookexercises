module FuncInstancesTest where

import Test.QuickCheck
import Test.QuickCheck.Function
import FuncInstances

functorIdentity :: (Functor f, Eq (f a)) =>
                        f a
                     -> Bool

functorIdentity f =
  fmap id f == f


functorCompose :: (Eq (f c), Functor f) =>
                    f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool

functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)


type IdentityFunctorIdentity a = (Identity a) -> Bool
type IdentityAFC a = (Identity a) -> (Fun a a) -> (Fun a a) -> Bool
type PairFunctorIdentity a = (Pair a) -> Bool
type PairAFC a = (Pair a) -> (Fun a a) -> (Fun a a) -> Bool
type TwoFunctorIdentity a b = (Two a b) -> Bool
type TwoABFC a b = (Two a b) -> (Fun b b) -> (Fun b b) -> Bool
type ThreeFunctorIdentity a b c = (Three a b c) -> Bool
type ThreeABCFC a b c = (Three a b c) -> (Fun c c) -> (Fun c c) -> Bool
type ThreePrimeFunctorIdentity a b = (Three' a b) -> Bool
type ThreePrimeABFC a b = (Three' a b) -> (Fun b b) -> (Fun b b) -> Bool
type FourFunctorIdentity a b c d = (Four a b c d) -> Bool
type FourABCDFC a b c d = (Four a b c d) -> (Fun d d) -> (Fun d d) -> Bool
type FourPrimeFunctorIdentity a b = (Four' a b) -> Bool
type FourPrimeABFC a b = (Four' a b) -> (Fun b b) -> (Fun b b) -> Bool
type BoolAndSomethingElseFunctorIdentity a = (BoolAndSomethingElse a) -> Bool
type BoolAndSomethingElseAFC a b c = (BoolAndSomethingElse a) -> (Fun a b) -> (Fun b c) -> Bool
type BoolAndMaybeSomethingElseFI a = (BoolAndMaybeSomethingElse a) -> Bool
type BoolAndMaybeSomethingElseABCFC a b c = (BoolAndMaybeSomethingElse a) -> (Fun a b) -> (Fun b c) -> Bool
type FlipIdentity a b = (Flip K a b) -> Bool
type FlipABCDFC a b c d = (Flip K a b) -> (Fun b c) -> (Fun c d) -> Bool

type PossiblyFunctorIdentity a = (Possibly a) -> Bool
type PossiblyAFC a = (Possibly a) -> (Fun a a) -> (Fun a a) -> Bool
type SumFunctorIdentity a b = (Sum a b) -> Bool
type SumABFC a b c d = (Sum a b) -> (Fun b c) -> (Fun c d) -> Bool

main :: IO ()
main = do
  quickCheck (functorIdentity :: (IdentityFunctorIdentity Int))
  quickCheck (functorCompose :: (IdentityAFC Int))
  quickCheck (functorIdentity :: (PairFunctorIdentity Int))
  quickCheck (functorCompose :: (PairAFC Int))
  quickCheck (functorIdentity :: (TwoFunctorIdentity String Int))
  quickCheck (functorCompose :: (TwoABFC String Int))
  quickCheck (functorIdentity :: (ThreeFunctorIdentity String Int Char))
  quickCheck (functorCompose :: (ThreeABCFC String Int Char))
  quickCheck (functorIdentity :: (ThreePrimeFunctorIdentity String Int))
  quickCheck (functorCompose :: (ThreePrimeABFC String Int))
  quickCheck (functorIdentity :: (FourFunctorIdentity String Int Char [String]))
  quickCheck (functorCompose :: (FourABCDFC String Int Char [String]))
  quickCheck (functorIdentity :: (FourPrimeFunctorIdentity String Int))
  quickCheck (functorCompose :: (FourPrimeABFC String Int))
  quickCheck (functorIdentity :: (PossiblyFunctorIdentity Int))
  quickCheck (functorCompose :: (PossiblyAFC Int))
  quickCheck (functorIdentity :: (SumFunctorIdentity Int String))
  quickCheck (functorCompose :: (SumABFC Int String Char [Int]))
  quickCheck (functorIdentity :: (BoolAndSomethingElseFunctorIdentity Int))
  quickCheck (functorCompose :: (BoolAndSomethingElseAFC Int String Char))
  quickCheck (functorIdentity :: (BoolAndMaybeSomethingElseFI Int))
  quickCheck (functorCompose :: (BoolAndMaybeSomethingElseABCFC Int String Char))
  quickCheck (functorIdentity :: (FlipIdentity Int String))
  quickCheck (functorCompose :: (FlipABCDFC Int String Char [Int]))
