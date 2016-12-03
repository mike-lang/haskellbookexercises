

import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf ax@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf ax ys


capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:rem) = (toUpper c) : rem

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = [(w, capitalizeWord w) | w <- words s]

-- using asPattern:
-- capitalizeWords xs = map f $ words xs
--   where f as@(s:st) = (as, toUpper s : st)

doCapitalizeParagraph :: String -> [String] -> [String]
doCapitalizeParagraph _ [] = []
doCapitalizeParagraph pword (w:ws)
  | (last pword) == '.' = capitalizeWord w : doCapitalizeParagraph w ws
  | otherwise = w : doCapitalizeParagraph w ws

capitalizeParagraph :: String -> String
capitalizeParagraph p 
  | paragraphWords == [] = p
  | tail paragraphWords == [] = p
  | otherwise = unwords ([capitalizeWord (head paragraphWords)] ++ doCapitalizeParagraph (head paragraphWords) (tail paragraphWords))
  where paragraphWords = words p

-- alternative:
-- capitalizeParagraph :: String -> String
-- capitalizeParagraph xs = concatMap capitalizeWord $ groupBy ((==) `on` (=='.')) xs
