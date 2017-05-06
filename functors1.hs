-- functors1.hs


data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

-- super NOT okay
-- to make ok, leave the n alone
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)


