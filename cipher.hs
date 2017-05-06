-- cipher.hs

module Cipher where

import Data.Char
import Data.Bool


myIsAlpha :: Char -> Bool
myIsAlpha c = ((ord c >= ord 'a') && (ord c <= ord 'z')) || 
              ((ord c >= ord 'A') && (ord c <= ord 'Z'))

shiftChar :: Int -> Char -> Char
shiftChar x c = (chr $ charBase + ((charOffset + x) `mod` 26))
  where charBase = bool (ord 'a') (ord 'A') (isUpper c)
        charOffset = ord c - charBase

type StringPosition = Int

getShiftFromPassphrase :: StringPosition -> String -> Int
getShiftFromPassphrase _ "" = 0
getShiftFromPassphrase i p = (ord (toUpper (p !! i))) - (ord 'A')

caesar :: Int -> String -> String
caesar _ "" = ""
caesar x (c:rem) 
  | not (myIsAlpha c) = c : caesar x rem
  | otherwise = (chr $ charBase + ((charOffset + x) `mod` 26)) : caesar x rem
    where charBase = bool (ord 'a') (ord 'A') (isUpper c)
          charOffset = ord c - charBase

uncaesar :: Int -> String -> String
uncaesar x s = caesar ((-1) * x) s

doVigenere :: Int -> String -> String -> String
doVigenere _ _ "" = ""
doVigenere _ "" s = s
doVigenere n p (c:rem) 
  | not (myIsAlpha c) = c : doVigenere n p rem
  | otherwise = (shiftChar (getShiftFromPassphrase n p) c) : doVigenere ((n+1) `mod` (length p)) p rem

vigenere :: String -> String -> String
vigenere p s = doVigenere 0 p s


runCaesar :: IO ()
runCaesar = do
  putStr "How many characters to shift? "
  shiftInput <- readLn
  putStr "Enter a string to encode: "
  inputString <- getLine
  putStrLn $ "Result: " ++ (caesar shiftInput inputString)

runVigenere :: IO ()
runVigenere = do
  putStr "Enter Passkey: "
  passKey <- getLine
  putStr "Enter a string to encode: "
  inputString <- getLine
  putStrLn $ "Result: " ++ (vigenere passKey inputString)

test :: IO ()
test = do
  myInput <- readLn
  print $ (myInput :: Int) + 5
