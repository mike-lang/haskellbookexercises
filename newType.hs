{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
  tooMany (n, _) = n > 43

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 43

instance TooMany (Num a, TooMany a) => (a, a) where
  tooMany (x, y) = tooMany (x + y)
