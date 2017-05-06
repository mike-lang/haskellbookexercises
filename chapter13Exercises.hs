import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let normalized = map toLower (filter isAlphaNum line1) in
    case (normalized == reverse normalized) of
          True -> putStrLn "It's a palindrome!"
          False -> do putStrLn "Nope!"
                      exitSuccess


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty 
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person

mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  age <- readLn
  case (mkPerson name age) of
    (Right person) -> print person
    (Left myError) -> print myError
