{-# LANGUAGE FlexibleInstances #-}


module FuncInstances where


import Test.QuickCheck
import Control.Monad

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = (Identity (f x))

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary





data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = (Pair (f x) (f y))

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary





data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = (Two x (f y))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary





data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = (Three x y (f z))

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary





data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = (Three' x (f y) (f z))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary




data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z zz) = (Four x y z (f zz))

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary






data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z zz) = (Four' x y z (f zz))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = liftM4 Four' arbitrary arbitrary arbitrary arbitrary





data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers x) = Yeppers (f x)
  fmap f LolNope = LolNope

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (3, liftM Yeppers arbitrary)]





data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = (First x)
  fmap f (Second y) = (Second (f y))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, liftM First arbitrary), (1, liftM Second arbitrary)]




data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)


-- Can a Functor instance be written for Bool?
--
-- No - Functor requires structure parameterized by a type.
-- Bool has no type parameter to operate on, so we can't 
-- even express this to Haskell

data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x)  = True'  (f x)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = frequency [(1, fmap False' arbitrary), (1, fmap True' arbitrary)]




data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = frequency [(1, elements [Falsish]), (5, fmap Truish arbitrary)]





data MySum b a =
    MyFirst a
  | MySecond b

instance Functor (MySum e) where
  fmap f (MyFirst a) = MyFirst (f a)
  fmap f (MySecond b) = MySecond b



data Company a c b = 
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c



data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'




data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = (Desk x)
  fmap f (Bloor y) = (Bloor (f y))



-- data K a b =
--   K a
--   deriving (Eq, Show)
-- 
-- instance Functor (K a) where
--   fmap _ x = x



newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = (Flip (K (f x)))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = fmap Flip (liftM K arbitrary)




data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = (GoatyConst (f b))





data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = (LiftItOut (fmap g x))

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa x y) = (DaWrappa (fmap h x) (fmap h y))



data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething x y) = (IgnoringSomething x (fmap h y))




data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap h (Notorious x y z) = (Notorious x y (fmap h z))




data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)


instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = (Cons (f x) (fmap f xs))




data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = (OneGoat (f x))
  fmap f (MoreGoats x y z) = (MoreGoats (fmap f x) (fmap f y) (fmap f z))





data TalkToMe a = 
    Halt
  | Print  String a
  | Read  (String -> a)


instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = (Print s (f x))
  fmap f (Read g) = (Read (f.g))
