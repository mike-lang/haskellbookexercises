-- print2.hs

module Print2 where

main :: IO ()

main = do
  putStrLn "Count to four fo me:"
  putStr "one, two"
  putStr ", three, and"
  putStrLn " four!"
