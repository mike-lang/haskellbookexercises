functionC x y =
  case (x > y) of
    True -> x
    False -> y

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
