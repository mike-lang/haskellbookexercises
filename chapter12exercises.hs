
import Data.Char


notThe :: String -> Maybe String
notThe x
  | map (toUpper) x == "THE" = Nothing
  | otherwise = Just x

notVowelInitial :: String -> Maybe String
notVowelInitial "" = Just ""
notVowelInitial x
  | (toUpper (head x)) `elem` "AEIOU" = Nothing
  | otherwise = Just x


replaceThe :: String -> String
replaceThe "" = ""
replaceThe xs = unwords (replaceThe' (words xs))

replaceThe' :: [String] -> [String]
replaceThe' [] = []
replaceThe' (x:xs) 
  | (notThe x) == Nothing = "a" : replaceThe' xs
  | otherwise = x : replaceThe' xs


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = countTheBeforeVowel' (words xs)

countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' [] = 0
countTheBeforeVowel' (x:[]) = 0
countTheBeforeVowel' (x:xs)
  | ((notThe x) == Nothing) && (notVowelInitial (head xs)) == Nothing = 1 + countTheBeforeVowel' xs
  | otherwise = countTheBeforeVowel' xs

countVowels :: String -> Integer
countVowels x = toInteger $ length $ filter isVowel x

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "AEIOU"

isVowel :: Char -> Bool
isVowel c = (toUpper c) `elem` vowels

isConsonant :: Char -> Bool
isConsonant c = isAlpha c && (not (isVowel c))

mkWord :: String -> Maybe Word'
mkWord s 
  | vowelCount > consonantCount = Nothing
  | otherwise = Just (Word' s)
  where vowelCount = length $ filter isVowel s
        consonantCount = length $ filter isConsonant s


