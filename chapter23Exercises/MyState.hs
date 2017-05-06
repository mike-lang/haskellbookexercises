{-# LANGUAGE InstanceSigs #-}

module MyState where

import Control.Monad
import Control.Monad.Trans.State

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, b) = g s
                               in (f a, b)

instance Applicative (Moi s) where                              
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
       -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s -> let fun = fst (f s)
                    (myA, st') = g s
                in (fun myA, st')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, st) = (f s)
                                  ms = runMoi $ g a
                              in ms st


get :: Moi s s
get = Moi $ \s -> (s,s)

put :: s -> Moi s ()
put s = Moi $ \x -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) s =
  let (a, st) = sa s
  in st

eval :: Moi s a -> s -> a
eval (Moi sa) =
  \s -> let (a, st) = sa s
        in a

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
