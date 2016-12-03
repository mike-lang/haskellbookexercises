
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 $ fibs
fibsUnder100 = takeWhile (<100) fibs


recursiveFactorial :: Integer -> Integer
recursiveFactorial 0 = 1
recursiveFactorial n = n * recursiveFactorial (n - 1)

facts = scanl (*) 1 (enumFrom 1)
factorialN x = facts !! x
