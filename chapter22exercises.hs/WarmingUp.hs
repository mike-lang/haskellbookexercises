{-# LANGUAGE InstanceSigs #-}

import Data.Char
import Control.Applicative (liftA2)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs


composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- cap
  b <- rev
  return (a, b)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = do
  a <- rev
  b <- cap
  return (a, b)

tupledMB :: [Char] -> ([Char], [Char])
tupledMB =
  cap >>= \a -> rev >>= \b -> return (a, b)

tupledMB' :: [Char] -> ([Char], [Char])
tupledMB' =
  rev >>= \a -> cap >>= \b -> return (a, b)

newtype Reader r a =
  Reader { runReader :: r -> a }

--instance Functor (Reader r) where
--  fmap f (Reader ra) =
--    Reader $ \r -> f (ra r)

instance Functor (Reader r) where
  fmap f (Reader ra) =
    Reader $ (f . ra)

ask :: Reader a a
ask = Reader id

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = 
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' =
  liftA2 Dog dogName address

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \x -> a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ (\r -> (runReader ((aRb . ra) r) r))

getDogRM' :: Person -> Dog
getDogRM' = dogName >>= \name -> address >>= \addy -> return (Dog name addy)
