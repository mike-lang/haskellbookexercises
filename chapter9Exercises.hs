
module Chapter9Exercises where

import Data.Char

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

cap :: String -> String
cap "" = ""
cap (c:rem) = toUpper c : rem

allCaps :: String -> String
allCaps "" = ""
allCaps (c:rem) = toUpper c : allCaps rem

firstCap :: String -> Char
firstCap s = (toUpper . head) s

firstCapPF :: String -> Char
firstCapPF = toUpper . head
