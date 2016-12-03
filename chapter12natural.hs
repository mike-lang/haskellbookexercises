import Data.List

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = Just (Succ (maybe Zero id (integerToNat (x - 1))))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d _ Nothing = d
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe d (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x:(catMaybes xs)

flipMaybe :: [Maybe a] -> Maybe [a] 
flipMaybe xs = flipMaybe' xs []

flipMaybe' :: [Maybe a] -> [a] -> Maybe [a]
flipMaybe' [] acc = Just (reverse acc)
flipMaybe' (Nothing:xs) acc = Nothing
flipMaybe' (Just x:xs) acc = flipMaybe' xs (x:acc)

lefts' :: [Either a b] -> [a]
lefts' = foldr ((++) . getLeft) []

getLeft :: Either a b -> [a]
getLeft (Left x) = [x]
getLeft (Right _) = []

rights' :: [Either a b] -> [b]
rights' = foldr ((++) . getRight) []

getRight :: Either a b -> [b]
getRight (Left _) = []
getRight (Right x) = [x]

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g = either' (\x -> Nothing) (\x -> Just (g x))

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x: (myIterate f (f x))

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f x = g (f x)
  where g Nothing = []
        g (Just (x, y)) = x : (myUnfoldr f y)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (x,y,z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f a = if (a < n) then Just (a + 1, a, a + 1) else Nothing
