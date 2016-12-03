tensDigit x = d
  where xLast = (fst . ((flip divMod) 10)) x
        d     = (snd . ((flip divMod) 10)) xLast

hunsD x = d2
  where xLast = (fst . ((flip divMod) 100)) x
        d2    = (snd . ((flip divMod) 10)) xLast

foldBool :: a -> a -> Bool -> a
foldBool x y z = 
  case z of
    True -> x
    False -> y

foldBool' x y z 
  | z         = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
