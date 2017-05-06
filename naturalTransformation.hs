{-# LANGUAGE RankNTypes #-}


type Nat f g = forall a . f a -> g a

--This'll work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- This will not work, not allowed
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a+1]

type BadNat f g a = f a -> g a

badMaybeToList :: BadNat Maybe [] a
badMaybeToList Nothing = []
badMaybeToList (Just a) = [a]

-- This is bad because the notion of a natural
-- transformation explicitly avoid adjusting
-- the values rather than the structure.
-- If you want to adjust values, write a fold!
-- Not a natural transformation!
badDegenerateMtl :: Num a => BadNat Maybe [] a
badDegenerateMtl Nothing = []
badDegenerateMtl (Just a) = [a + 1]
