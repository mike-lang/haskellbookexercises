module Reverse where

rvrs :: String -> String
rvrs x = last ++ " " ++ middle ++ " " ++ first
  where first = take 5 x
        middle = drop 6 (take 8 x)
        last = drop 9 x


main :: IO ()
main = print $ rvrs "Curry is awesome"

