newtype Identity a = Identity a
  deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity x) = (Identity (f x))


instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = (Identity (f x))



newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f x = (Constant (getConstant x))

instance (Monoid a) => Applicative (Constant a) where
  pure x = (Constant mempty)
  (<*>) fc xc = (Constant (mappend (getConstant fc) (getConstant xc)))



validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = 
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' -> 
      case mkAddress a of
        Nothing -> Nothing 
        Just a' -> 
          Just $ Person n' a'


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

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' = Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'


-- Applicative laws:
--
-- Identity law -
--   Applying id embedded in structure itself is the identity function
--
--   pure id <*> x == x
--
-- Composition commutes with application -
--   Applying an application of composed functions is the same
--   as composing applications
--
--   pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
--
-- Homomorphism -
--   Applying a function embedded in structure to a value embedded
--   in structure should be the same as embedding the result of passing
--   the value to the function into structure
--
--   In other words: structure embedding via pure should commute with application
--   
--   pure f <*> pure x == pure (f x)
--
-- Interchange -
--   
--   I'm not sure how to express this one as a paragraph, but the invariant is:
--
--   u <*> pure v == pure ($ v) <*> u
