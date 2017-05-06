import Control.Monad
import Data.Monoid
import Test.QuickCheck


class Semigroup a where
  (<>) :: a -> a -> a

semigroupAssoc :: (Eq g, Semigroup g) => g -> g -> g -> Bool
semigroupAssoc = (a <> b) <> c == a <> (b <> c)


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

  mappend (Only x) (Only y) = Only (mappend x y)
  mappend (Only x) Nada = Only x
  mappend Nada (Only x) = Only x


-- Don't forget to write an Arbitrary 
-- instance for First'. We won't always
-- stub that out explicitly for you

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)
  
instance Monoid (First' a) where
  mempty = (First' Nada)
  mappend (First' Nada) x = x
  mappend x (First' Nada) = x
  mappend x@(First' (Only _)) _ = x

genFirst' :: (Arbitrary a) => Gen (First' a)
genFirst' = do
  a <- arbitrary
  frequency [ (1, return (First' Nada)) 
            , (3, return (First' (Only a))) ]

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = genFirst'

firstMappend :: First' a
             -> First' a
             -> First' a

firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool
 
main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
