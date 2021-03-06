-- matchingTuples1.hs

module TupleFunctions where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x,y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

third3 :: (a,b,c) -> c
third3 (_, _, x) = x

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

