module MyMonad where

import Control.Monad (join)


bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x


twiceWhenEven :: [Integer] -> [Integer]
--twiceWhenEven xs = do
--  x <- xs
--  if even x
--    then [x*x, x*x]
--    else [x*x]

twiceWhenEven xs = do
  x <- xs
  y <- if even x
    then [x*x, x*x]
    else []

  if even y
    then [y,y,y]
    else []

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow name' age' weight' =
--   case noEmpty name' of
--     Nothing -> Nothing
--     Just nammy ->
--       case noNegative age' of
--         Nothing -> Nothing 
--         Just agey -> 
--           case noNegative weight' of
--             Nothing -> Nothing
--             Just weighty ->
--               weightCheck (Cow nammy agey weighty)

-- mkSphericalCow name' age' weight' = do
--   name <- noEmpty name'
--   age <- noNegative age'
--   weight <- noNegative weight'
--   weightCheck (Cow name age weight)

mkSphericalCow name' age' weight' =
  noEmpty name' >>=
  \namey -> noNegative age' >>=
  \agey -> noNegative weight' >>=
  \weighty -> weightCheck (Cow namey agey weighty)

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i = 
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a,b,c)

type Founded = Int

type Coders = Int


data SoftwareShop =
  Shop {
      founded     :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0      = Left $ NegativeYears n
  | n > 500    = Left $ TooManyYears n
  | otherwise  = Right n

validateCoders n
  | n < 0      = Left $ NegativeCoders n
  | n > 5000   = Left $ TooManyCoders n
  | otherwise  = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers
    
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = (First x)
  fmap f (Second x) = (Second f x)

instance Applicative (Sum a) where
  pure x = (Second x)
  (<*>) (First x) _ = (First x)
  (<*>) _ (First x) = (First x)
  (<*>) (Second f) (Second x) = (Second (f x))

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = (First x)
  (>>=) (Second x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = Sum <$> arbitrary <*> arbitrary

--instance (Eq a, Eq b) => EqProp (Sum a b) where
--  (=-=) = eq
--
--
--  Identity Laws!
--
--  right identity
--  x >>= return = x 
--
--  left identity
--  return x >>= f = f x
--
--  associativity
--  (m >>= f) >>= g = m >>= (\x -> f x >>= g)
