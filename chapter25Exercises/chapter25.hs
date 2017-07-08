{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

import Control.Monad

--newtype Identity a = 
--  Identity { runIdentity :: a }

-- instance Functor Identity where
--   fmap f (Identity a) = Identity (f a)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => 
         Functor (Compose f g)  where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => 
         Applicative (Compose f g) where
--  pure :: a -> Compose f g a
  pure = Compose <$> (pure . pure)

  (Compose f) <*> (Compose a) = Compose $ ((pure (<*>)) <*> f) <*> a

instance (Foldable f, Foldable g) => 
         Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha


class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
        
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second f = bimap id f

data Deux a b = Deux a b
  deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = (Deux (f a) (g b))

data Const a b = Const a
  deriving (Eq, Show)

instance Bifunctor Const where
  bimap f g (Const a) = (Const (f a))

data Drei a b c = Drei a b c
  deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b
  deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei x y) = SuperDrei x (f y)

data SemiDrei a b c = SemiDrei a
  deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei x) = SemiDrei x

data Quadriceps a b c d = Quadzzz a b c d
  deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz x y z z') = (Quadzzz x y (f z) (g z'))

data MyEither a b = MyLeft a | MyRight b
  deriving (Eq, Show)

instance Bifunctor (MyEither) where
  bimap f g (MyLeft x) = MyLeft (f x)
  bimap f g (MyRight y) = MyRight (g y)

newtype MaybeIO a =
  MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a =
  MaybeList { runMaybeList :: [Maybe a] }

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = (Identity (f x))

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = (IdentityT (fab <*> fa))

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

--instance (Monad m) => Monad (IdentityT m) where
--  return = pure
--  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f =
    let aimb = ma >>= (runIdentityT . f) 
    in IdentityT aimb 



