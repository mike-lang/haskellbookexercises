{-# LANGUAGE FlexibleContexts #-}
module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators 

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = do
  readOne <- char '1' 
  eof
  return readOne

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

-- rudimentary char
-- demo only, this won't work as is.
-- char :: Char -> Parser Char
-- char c =
--   Parser $ \ s ->
--     case s of
--       (x:xs) -> if c == x
--                 then [(c, xs)]
--                 else []
--       _ -> []

-- from Text.ParserCombinators.HuttonMeijer
-- polyparse-1.11

-- type Token = Char
-- newtype Parser a =
--   P ([Token] -> [(a, [Token])])

-- Same thing, differently formatted:
type Parser' a = String -> [(a, String)]

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2' 

-- read two characters, '1', and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParse' :: Parser String -> IO ()
testParse' p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

-- oneTwoThree = choice [string "1", string "12", string "123"]

myString (x:[]) = char x
myString (x:xs) = char x >> myString xs

p' :: Parser [Integer]
p' = some $ do
  i <- token (some digit)
  return (read i)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoThree:"
--  print $ parseString oneTwoThree mempty "123"
  testParse (myString "123") 

