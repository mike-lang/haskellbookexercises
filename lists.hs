

eftBool :: Bool -> Bool -> [Bool]
eftBool f t
  | f > t = []
  | f < t = f : eftBool (succ f) t
  | otherwise = [t]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a > b = []
  | a < b = a : eftOrd (succ a) b
  | otherwise = [b]

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b = []
  | a < b = a : eftInt (succ a) b
  | otherwise = [b]

eftChar :: Char -> Char -> [Char]
eftChar a b
  | a > b = []
  | a < b = a : eftChar (succ a) b
  | otherwise = [b]

myWords :: String -> [String]
myWords "" = []
myWords x = firstWord : myWords remainingString
  where firstWord = (takeWhile (/=' ') (dropWhile (==' ') x))
        remainingString = dropWhile (==' ') $ dropWhile (/=' ') $ dropWhile (==' ') x

mySplit :: String -> Char -> [String]
mySplit "" _ = []
mySplit x c = firstPart : mySplit remaining c
  where firstPart = takeWhile (/=c) x
        remaining = dropWhile (==c) $ dropWhile (/=c) x

myWords2 :: String -> [String]
myWords2 x = mySplit x ' '


