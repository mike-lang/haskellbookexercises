-- recursion.hs
 
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes' (n-1) f $ b

type Numerator = Integer
type Denomenator = Integer
type Quotient = Integer
dividedBy :: Numerator -> Denomenator -> Quotient
dividedBy = div

dividedBy' :: (Integral a) => a -> a -> (a, a)
dividedBy' num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

data DividedResult =
  Result Integer
  | DividedByZero deriving Show

fixedDividedBy :: Integer -> Integer -> DividedResult
fixedDividedBy n d
  | d == 0 = DividedByZero
  | otherwise = go (abs n) (abs d) 0 ((signum n) * (signum d))
  where go n d count s
            | n < d     = Result (s * count)
            | otherwise = go (n - d) d (count + 1) s


mc91 :: Integer -> Integer
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 $ mc91 (n + 11)


