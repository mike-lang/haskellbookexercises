import Control.Monad ((>=>), join)

-- mcomp :: Monad m =>
--          (b -> m c)
--       -> (a -> m b)
--        -> a -> m c

mcomp'' :: Monad m =>
           (b -> m c)
        -> (a -> m b)
         -> a -> m c


-- mcomp'' f g a = join (f <$> (g a))
-- or expressed alternately
mcomp'' f g a = g a >>= f

-- haskell provides what's called
-- "Kleisli composition"

-- notice the order is flipped to match >>=

-- (>=>) Monad m => (a -> m b) -> (b -> m c) -> a -> m c

-- So promises are a monad
-- A promise chain is a sequence of kleisli composed functions
-- that return a promise


sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

