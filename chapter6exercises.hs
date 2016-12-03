

module Chapter6Exercises where




data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'




data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = 
    a == a' && b == b'




data StringOrInt = 
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') = a == a'
  (==) (TisAString a) (TisAString a') = a == a'
  (==) (TisAnInt a) (TisAString a') = False
  (==) (TisAString a) (TisAnInt a') = False




data Pair a = 
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v w) (Pair v' w') = 
    v == v' && w == w'




data Tuple a b = 
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') =
    x == x' && y == y'




data Which a = 
    ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) (ThisOne x) (ThatOne x') = False
  (==) (ThatOne x) (ThisOne x') = False




data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) (Hello x) (Goodbye y) = False
  (==) (Goodbye x) (Hello y) = False
