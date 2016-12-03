awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == (reverse x)

myAbs :: Integer -> Integer
myAbs x = if x < 0 
            then -x
          else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

x = (+)
f2 xs = x w 1
  where w = length xs

myId = (\x -> x)

myHead = (\(x:xs) -> x)

f (a,b) = a
