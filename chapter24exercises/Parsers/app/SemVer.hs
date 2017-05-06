{-# LANGUAGE FlexibleContexts #-}

module SemVer where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators
import Data.List
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Data.List (intersperse)
import Test.QuickCheck
import Test.QuickCheck.Regex (matching)
import qualified Data.Map.Strict as Map


data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]


data SemVer = 
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare (SemVer maj min pat _ _) (SemVer maj' min' pat' _ _) 
    | maj > maj' = GT
    | maj < maj' = LT
    | min > min' = GT
    | min < min' = LT
    | pat > pat' = GT
    | pat < pat' = LT
    | otherwise = EQ

numOrString = do
    (NOSI <$> integer) 
  <|> (NOSS <$> (some letter))

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.' 
  minor <- integer 
  char '.' 
  patch <- integer 
  optional (char '-')
  release <- many (skipOptional (char '.') >> numOrString)
  metadata <- many numOrString
  return $ SemVer major minor patch release metadata

parseDigit :: Parser Char
parseDigit = choice [char '0', char '1', char '2', char '3', 
                     char '4', char '5', char '6', char '7',
                     char '8', char '9']

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit
  return ((read digits) :: Integer)


base10Integer' :: Parser Integer
base10Integer' = do
  sign <- optional (char '-')
  multiplier <- case sign of
                  (Just '-') -> return (-1)
                  Nothing -> return 1
  digits <- some parseDigit
  return $ ((read digits) :: Integer) * multiplier


type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parseWithParens :: Parser NumberingPlanArea
parseWithParens = do
  area <- between (char '(') (char ')') (count 3 parseDigit)
  char ' '
  return $ ((read area) :: NumberingPlanArea)

parseWithoutParens = do
  area <-(count 3 parseDigit) 
  skipOptional (char '-')
  return $ ((read area) :: NumberingPlanArea)


parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = do
  parseWithParens <|> parseWithoutParens

parseExchange = do
  exch <- (count 3 parseDigit) 
  skipOptional ((char ' ') <|> (char '-'))
  return $ ((read exch) :: Exchange)

parseLineNumber = (count 4 parseDigit) >>= \num -> return $ ((read num) :: LineNumber)

parsePhone :: Parser PhoneNumber
parsePhone = do
  skipOptional (string "1-")
  area <- parseNumberingPlanArea
  exch <- parseExchange
  num <- parseLineNumber
  return $ PhoneNumber area exch num
  

type EntryText = [Char]

data LogEntry =
  LogEntry TimeOfDay EntryText
  deriving (Eq)

instance Show LogEntry where
  show (LogEntry time entryText) = (formatTime defaultTimeLocale "%R" time) ++ " " ++ entryText



data Log =
  Log [DateSection]
  deriving (Eq)

instance Show Log where
  show (Log sections) = 
    mconcat (intersperse "\n\n" (show <$> sections))

data DateSectionHeader =
  DateSectionHeader UTCTime
  deriving (Eq)

instance Show DateSectionHeader where 
  show (DateSectionHeader day) = "# " ++ (formatTime defaultTimeLocale "%F" day)

data DateSection =
  DateSection { header :: DateSectionHeader
              , entries :: [LogEntry] }
  deriving (Eq)

instance Show DateSection where
  show x = show (header x) ++ "\n" ++ (mconcat (intersperse "\n" (show <$> (entries x))))

type Comment = [Char]

newtype CommentString = CommentString [Char]
  deriving (Eq, Show)

commentStringGen = matching "--[^\n]*\n"

timeOfDayGen = do
  hour <- show <$> (choose (0,23) :: Gen Int)
  minute <- show <$> (choose (0,59) :: Gen Int)
  return (TimeOfDay (read hour) (read minute) 0)

timesOfDayGen = do
  randomTimes <- listOf1 timeOfDayGen
  return $ sort randomTimes

dateGen = do
  mjdDays <- (choose (0, 100000) :: Gen Integer)
  return $ UTCTime (ModifiedJulianDay mjdDays) (secondsToDiffTime 0)
 

entryTextGen = resize 20 (matching "[^\n-]*")

-- Maybe there's a formulation where this would be useful?
-- Perhaps the vectorOf combinator together with some
-- transformation inside of the Gen Monad would do the trick?
--
-- logEntryGen = do
--   timeOfDay <- timeOfDayGen
--   entryText <- entryTextGen
--   endOfLine <- oneof [commentStringGen, elements ["\n"]]
--   return (timeOfDay ++ " " ++ entryText ++ endOfLine)

restOfEntryGen = do
  entryText <- entryTextGen
  endOfLine <- oneof [commentStringGen, elements ["\n"]]
  return (entryText ++ endOfLine)

-- Instead I went with first generating a random list of times of day
-- and then using its size to drive a call to vectorOf to generate
-- entry text for each time of day and then zip the lists together
-- to generate the list of log entries for the day
logSectionStringsGen = do
  timesOfDay <- timesOfDayGen
  entries <- vectorOf (length timesOfDay) restOfEntryGen
  return $ zipWith (\timeOfDay entry -> (formatTime defaultTimeLocale "%R" timeOfDay) ++ " " ++ entry) timesOfDay entries


logSectionHeaderStringGen = do
  myDate <- dateGen 
  endOfLine <- oneof [commentStringGen, elements ["\n"]]
  return ("# " ++ (formatTime defaultTimeLocale "%F" myDate) ++ " " ++ endOfLine)

logSectionStringGen = do
  header <- logSectionHeaderStringGen
  bodyStrings <- randoZip logSectionStringsGen (resize 10 (listOf (resize 30 commentStringGen)))
  return $ header ++ (mconcat bodyStrings)

type LogSectionString = [Char]


merge :: [a] -> [a] -> [Int] -> [a]
merge lefts rights inserts = mergeInner lefts rights (sort inserts) [] 0

mergeInner :: [a] -> [a] -> [Int] -> [a] -> Int -> [a]

mergeInner lefts [] [] acc _ = acc ++ lefts
mergeInner lefts (r:rs) (ins:inses) acc i = mergeInner (drop (ins - i) lefts) rs inses (acc ++ (take (ins - i) lefts) ++ [r]) ins 

-- Here's a cute combinator
-- Takes two list generators and produces a
-- generator for lists produced by interleaving the
-- lists produced by the given list generators
-- while preserving the order of the elements
-- in the generated lists
randoZip :: Gen [a] -> Gen [a] -> Gen [a]
randoZip genLeft genRight = do
  left <- genLeft
  right <- genRight
  insertionPoints <- sort <$> (vectorOf (length right) (choose (0,(length left)) :: Gen Int))
  return $ merge left right insertionPoints


parseCommentToText :: Parser [Char]
parseCommentToText = do
  string "--" 
  skipOptional (space)
  commentText <- many (notChar '\n')
  newline
  return commentText

parseYYYYMMDD :: Parser UTCTime
parseYYYYMMDD = do
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  return (read (year ++ "-" ++ month ++ "-" ++ day ++ " 00:00:00") :: UTCTime)

parseDateSectionHeader :: Parser DateSectionHeader
parseDateSectionHeader = do
  string "# " 
  date <- parseYYYYMMDD
  many (char ' ')
  choice [parseCommentToText, ((many (char ' ')) >> newline >> return "")]
  return (DateSectionHeader date)
  
parseLogEntry :: Parser LogEntry
parseLogEntry = do
  hour <- count 2 digit
  char ':'
  minute <- count 2 digit
  char ' '
  entryTextStart <- many (noneOf "-\n")
  comment <- optional (try (char '-' >> char '-' >> (many (noneOf "\n"))))
  rest <- optional (many (noneOf "\n"))
  entryText <- return (maybe entryTextStart (\end -> entryTextStart ++ end) rest)
  newline
  return (LogEntry ((read (hour ++ ":" ++ minute ++ ":00")) :: TimeOfDay) entryText)

parseLogSectionBody :: Parser [LogEntry]
parseLogSectionBody = do
  many ((many parseCommentToText) >> (parseLogEntry))

parseDateSection :: Parser DateSection
parseDateSection = do
  header <- parseDateSectionHeader
  entries <- parseLogSectionBody
  return (DateSection header entries)

parseLog :: Parser Log
parseLog = do
  sections <- (many (do
    many (choice [parseCommentToText, newline >> return ""])
    section <- parseDateSection
    newline
    return section))
  return (Log sections)

getSectionDate :: DateSectionHeader

-- collectTimesSpent :: Log -> (Map [Char], [Int])
-- collectTimesSpent (Log sections) =
--   where allEntriesOrdered = (foldMap (\s -> (sortBy (\(DateSection (DateSectionHeader d1) _) (DateSection (DateSectionHeader d2)) -> (compare (d1 d2))) sections)




-- calculateAverages :: Log -> ([Char], Float)



-- doCheck :: IO ()
-- doCheck = do

sampleData = 
  -- wheee a comment
  # 2025-02-05
  08:00 Breakfast
  09:00 Sanitizing moisture collector
  11:00 Exercising in high-grav gym
  12:00 Lunch
  13:00 Programming
  17:00 Commuting home in rover
  17:30 R&R
  19:00 Dinner
  21:00 Shower
  21:15 Read
  22:00 Sleep
  # 2025-02-07 -- dates not nececessarily sequential
  08:00 Breakfast -- should I try skippin bfast?
  09:00 Bumped head, passed out
  13:36 Wake up, headache
  13:37 Go to medbay
  13:40 Patch self up
  13:45 Commute home for rest
  14:15 Read
  21:00 Dinner
  21:15 Read
  22:00 Sleep
  945
  
  
  
  
  
  
  
  

