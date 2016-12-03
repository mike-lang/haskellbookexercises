


stops = "pbtdkg"
vowels = "aeiou"

svs = [ (x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["car", "cup", "dog", "chair", "banana"]
verbs = ["hits", "kisses", "eats"]

nvn = [ (x,y,z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc x = 
  div (sum (map length (words x)))
      (length (words x))

--average word length in sentence x?
-- String -> Integer

seekritTwo x =
  (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))



myOr1 :: [Bool] -> Bool
myOr1 [] = False
myOr1 (x:xs) = 
  if x == True 
  then True
  else myOr1 xs

myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs

myOr3 :: [Bool] -> Bool
myOr3 xs = foldr (\a b -> if a == True then True else b) False xs

myOr4 :: [Bool] -> Bool
myOr4 = foldr (||) False

myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 _ [] = False
myAny1 f (x:xs) = if f x then True else myAny1 f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ [] = False
myAny2 f (x:xs) = (f x) || myAny2 f xs

myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f (x:xs) = foldr (\a b -> if f a then True else b) False xs

myAny4 :: (a -> Bool) -> [a] -> Bool
myAny4 f = foldr ((||).f) False

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 e xs = any (==e) xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 e = foldr ((||).(==e)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = foldl (\a b -> if ((f a b) == GT) then a else b) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = foldl (\a b -> if ((f a b) == LT) then a else b) x xs
