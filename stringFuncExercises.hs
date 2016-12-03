-- stringFuncExercises.hs

module StringFuncExercises where

addBang :: String -> String
addBang x = x ++ "!"

fifth :: String -> Char 
fifth x = x !! 4

dropNine :: String -> String
dropNine x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x 

rvrs :: String -> String
rvrs x = last ++ " " ++ middle ++ " " ++ first
  where first = take 5 x
        middle = drop 6 (take 8 x)
        last = drop 9 x


