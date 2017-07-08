{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Debug.Trace
import Text.HTML.TagSoup
import System.IO
import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators
import Data.List
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Data.List (intersperse)
import Test.QuickCheck (Gen, oneof, resize, elements, vectorOf, listOf1, choose, listOf )
import Test.QuickCheck.Regex (matching)
import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Char (isSpace, toLower, toUpper)
import Data.Word
import Data.Bits
import Numeric (showHex)
import Text.Printf
import Data.Graph.Inductive
import Control.Monad

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

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

getSections :: Log -> [DateSection]
getSections (Log s) = s

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
  <?> "Tried to parse a comment"

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
  choice [parseCommentToText, ((many (char ' ')) >> newline >> return "" <?> "Tried to parse the rest of a date section header")]
  return (DateSectionHeader date)
  <?> "Tried to parse a section header"

parseLogEntry :: Parser LogEntry
parseLogEntry = do
  hour <- count 2 digit
  char ':'
  minute <- count 2 digit
  char ' '
  entryTextStart <- many (noneOf "-\n")
  comment <- optional (try (char '-' >> char '-' >> (many (noneOf "\n"))))
  rest <- optional (many (noneOf "\n"))
  entryText <- (return (maybe entryTextStart (\end -> entryTextStart ++ end) rest)) <?> "Tried to parse the text of a LogEntry"
  newline
  return (LogEntry ((read (hour ++ ":" ++ minute ++ ":00")) :: TimeOfDay) entryText)
  <?> "Tried to parse a LogEntry"

parseLogSectionBody :: Parser [LogEntry]
parseLogSectionBody = do
  many ((many parseCommentToText) >> (parseLogEntry))
  <?> "Tied to parse a LogSectionBody"

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
    many newline
    return section
    <?> "Tried to parse a log section"))
  return (Log sections)

getSectionDate :: DateSectionHeader -> UTCTime
getSectionDate (DateSectionHeader x) = x

-- collectTimesSpent :: Log -> (Map [Char], [Int])
-- collectTimesSpent (Log sections) =
--   where allEntriesOrdered = (foldMap (\s -> (sortBy (\(DateSection (DateSectionHeader d1) _) (DateSection (DateSectionHeader d2)) -> (compare (d1 d2))) sections)

getAllEntriesOrdered :: Log -> [(UTCTime, LogEntry)]
getAllEntriesOrdered (Log sections) = (foldMap (\s@(DateSection (DateSectionHeader d) _) -> (((,) d) <$> (entries s))) sortedSections)
  where sortedSections = (sortBy (\(DateSection (DateSectionHeader d1) _) (DateSection (DateSectionHeader d2) _) -> (compare d1 d2)) sections)



data ActivityInterval = 
  ActivityInterval EntryText NominalDiffTime
  deriving (Show)

calculateIntervals :: [(UTCTime, LogEntry)] -> [ActivityInterval]
calculateIntervals (_:[]) = [] -- Ignore the very last entry because we don't know when it ended
calculateIntervals ((startDate, (LogEntry startTime activityDescription)):rest@((endDate, (LogEntry endTime _)):_)) = (ActivityInterval activityDescription timeSpent) : (calculateIntervals rest)
  where start = (UTCTime (utctDay startDate) (timeOfDayToTime startTime))
        end = (UTCTime (utctDay endDate) (timeOfDayToTime endTime))
        timeSpent = diffUTCTime end start

groupIntervals :: [ActivityInterval] -> [[ActivityInterval]]
groupIntervals is = groupedIntervals
  where sortedIntervals = (sortBy (\(ActivityInterval d1 _) (ActivityInterval d2 _) -> (compare d1 d2)) is)
        groupedIntervals = (groupBy (\(ActivityInterval d1 _) (ActivityInterval d2 _) -> (trim d1) == (trim d2)) sortedIntervals)

sumIntervalGroups :: [[ActivityInterval]] -> [ActivityInterval]
sumIntervalGroups [] = []
sumIntervalGroups (intervals:rest) = firstSum : (sumIntervalGroups rest)
  where (ActivityInterval firstActivityDescription _) = (head intervals) 
        firstSum = ActivityInterval firstActivityDescription (sum ((\(ActivityInterval _ t) -> t) <$> intervals))

getSumReportLine (ActivityInterval description timeSpent) = 
  description ++ " - " ++ (show timeSpent) ++ "\n"

getAvgReportLine numDays (ActivityInterval description timeSpent) =
  description ++ " - " ++ (show avgSecondsPerDay) ++ " s/day\n"
  where avgSecondsPerDay = (realToFrac ((toRational timeSpent) / (toRational numDays)))

showTotalTimeSpentInActivities :: Log -> IO ()
showTotalTimeSpentInActivities myLog = 
  let allEntries = getAllEntriesOrdered myLog
      allIntervals = calculateIntervals allEntries
      allGroupedIntervals = groupIntervals allIntervals
      allSums = sumIntervalGroups allGroupedIntervals
  in putStr $ foldMap getSumReportLine allSums

showAvgTimeSpentInActivities :: Log -> IO ()
showAvgTimeSpentInActivities myLog =
  let allEntries = getAllEntriesOrdered myLog
      allIntervals = calculateIntervals allEntries
      allGroupedIntervals = groupIntervals allIntervals
      allSums = sumIntervalGroups allGroupedIntervals
      (Log logSections) = myLog
      numDaysInLog = length logSections
  in putStr $ foldMap (getAvgReportLine numDaysInLog) allSums


sampleData = "\
\-- wheee a comment\n\
\# 2025-02-05\n\
\08:00 Breakfast\n\
\09:00 Sanitizing moisture collector\n\
\11:00 Exercising in high-grav gym\n\
\12:00 Lunch\n\
\13:00 Programming\n\
\17:00 Commuting home in rover\n\
\17:30 R&R\n\
\19:00 Dinner\n\
\21:00 Shower\n\
\21:15 Read\n\
\22:00 Sleep\n\
\# 2025-02-07 -- dates not nececessarily sequential\n\
\08:00 Breakfast -- should I try skippin bfast?\n\
\09:00 Bumped head, passed out\n\
\13:36 Wake up, headache\n\
\13:37 Go to medbay\n\
\13:40 Patch self up\n\
\13:45 Commute home for rest\n\
\14:15 Read\n\
\21:00 Dinner\n\
\21:15 Read\n\
\22:00 Sleep\n"

optimisticParseLog :: [Char] -> Log
optimisticParseLog s = (head (toList (parseString parseLog mempty s)))


data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress addressWord) = 
    (show octet1) ++ "." ++ (show octet2) ++ "." ++ (show octet3) ++ "." ++ (show octet4)
    where octet1 = shiftR addressWord 24
          octet2 = shiftR ((.&.) addressWord 0xFF0000) 16
          octet3 = shiftR ((.&.) addressWord 0xFF00) 8
          octet4 = ((.&.) addressWord 0xFF)


parseOctet :: Parser Integer
parseOctet = do
  octetDigits <- some digit
  if (((read octetDigits) :: Integer) > 255) then (unexpected "Invalid Octet") else return $ ((read octetDigits) :: Integer)

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  first <- parseOctet
  char '.'
  second <- parseOctet
  char '.'
  third <- parseOctet
  char '.'
  fourth <- parseOctet
  return $ IPAddress (fromInteger ((first * 16777216) + (second * 65536) + (third * 256) + fourth))

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord)

instance Show IPAddress6 where
  show (IPAddress6 firstWord secondWord) =
    ((printf fmt segment1) ++ ":" ++ (printf fmt segment2) ++ ":" ++ (printf fmt segment3) ++ ":" ++ (printf fmt segment4) ++ ":" ++ (printf fmt segment5) ++ ":" ++ (printf fmt segment6) ++ ":" ++ (printf fmt segment7) ++ ":" ++ (printf fmt segment8)) 
    where segment1 = shiftR firstWord 48
          segment2 = (shiftR ((.&.) 0x0000ffff00000000 firstWord) 32)
          segment3 = (shiftR ((.&.) 0x00000000ffff0000 firstWord) 16)
          segment4 = ((.&.) 0x000000000000ffff firstWord)
          segment5 = shiftR secondWord 48
          segment6 = (shiftR ((.&.) 0x0000ffff00000000 secondWord) 32)
          segment7 = (shiftR ((.&.) 0x00000000ffff0000 secondWord) 16)
          segment8 = ((.&.) 0x000000000000ffff secondWord)
          fmt = "%04x"

parseAddressSegment :: Parser Integer
parseAddressSegment = do
  segmentCharacters <- many hexDigit
  if (length segmentCharacters) > 4 then (unexpected "Too many hex digits for address segment") else (return ((read ("0x" ++ segmentCharacters)) :: Integer))

parseSeparator :: Parser ()
parseSeparator = do
  char ':'
  notFollowedBy (char ':')

parseSegmentWithSeparator :: Parser Integer
parseSegmentWithSeparator = do
  segment <- parseAddressSegment
  optional (char ':')
  return segment

fromSegmentsToAddress :: [Integer] -> Maybe [Integer] -> IPAddress6
fromSegmentsToAddress bss Nothing = fromSegmentsToAddress bss (Just [])
fromSegmentsToAddress beforeSeparatorSegments (Just afterSeparatorSegments) =
  IPAddress6 firstWord secondWord
  where zeroesToAdd = take (8 - (length beforeSeparatorSegments) - (length afterSeparatorSegments)) (repeat 0)
        allSegments = beforeSeparatorSegments ++ zeroesToAdd ++ afterSeparatorSegments
        firstWordSegmentOne:firstWordSegmentTwo:firstWordSegmentThree:firstWordSegmentFour:secondWordSegmentOne:secondWordSegmentTwo:secondWordSegmentThree:secondWordSegmentFour:_ = allSegments
        firstWord = fromInteger $ ((16 ^ 12) * firstWordSegmentOne) + ((16 ^ 8) * firstWordSegmentTwo) + ((16 ^ 4) * firstWordSegmentThree) + (firstWordSegmentFour)
        secondWord = fromInteger $ ((16 ^ 12) * secondWordSegmentOne) + ((16 ^ 8) * secondWordSegmentTwo) + ((16 ^ 4) * secondWordSegmentThree) + (secondWordSegmentFour)

--myDebug :: [Integer] -> [Integer] -> IPAddress6
myDebug bss Nothing = myDebug bss (Just [])
myDebug beforeSeparatorSegments (Just afterSeparatorSegments) =
  (IPAddress6 firstWord secondWord, zeroesToAdd, allSegments, firstWordSegmentOne, firstWordSegmentTwo, firstWordSegmentThree, firstWordSegmentFour, secondWordSegmentOne, secondWordSegmentTwo, secondWordSegmentThree, secondWordSegmentFour, firstWord, secondWord)
  where zeroesToAdd = take (8 - (length beforeSeparatorSegments) - (length afterSeparatorSegments)) (repeat 0)
        allSegments = beforeSeparatorSegments ++ zeroesToAdd ++ afterSeparatorSegments
        firstWordSegmentOne:firstWordSegmentTwo:firstWordSegmentThree:firstWordSegmentFour:secondWordSegmentOne:secondWordSegmentTwo:secondWordSegmentThree:secondWordSegmentFour:_ = allSegments
        firstWord = fromInteger $ ((16 ^ 12) * firstWordSegmentOne) + ((16 ^ 8) * firstWordSegmentTwo) + ((16 ^ 4) * firstWordSegmentThree) + (firstWordSegmentFour)
        secondWord = fromInteger $ ((16 ^ 12) * secondWordSegmentOne) + ((16 ^ 8) * secondWordSegmentTwo) + ((16 ^ 4) * secondWordSegmentThree) + (secondWordSegmentFour)


parseStuff = do
   beforeDoubleSeparator <- sepBy parseAddressSegment (try parseSeparator)
   afterDoubleSeparator <- (optional parseRest)
   return (beforeDoubleSeparator, afterDoubleSeparator)

parseRest = do
  string "::"
  sepBy parseAddressSegment (try parseSeparator)

parseIP6Address = do
   beforeDoubleSeparator <- sepBy parseAddressSegment (try parseSeparator)
   afterDoubleSeparator <- (optional parseRest)
   return $ fromSegmentsToAddress beforeDoubleSeparator afterDoubleSeparator

fromIPv4ToIPv6 :: IPAddress -> IPAddress6
fromIPv4ToIPv6 (IPAddress addressWord) = (IPAddress6 0 ((.|.) 0xffff00000000 (fromInteger (toInteger addressWord))))

isIPv4MappedAddress :: IPAddress6 -> Bool
isIPv4MappedAddress (IPAddress6 firstWord secondWord) = 
  (firstWord == 0) && ((shiftR secondWord 32) == 0xffff)

fromIPv6ToIPv4 :: IPAddress6 -> (Maybe IPAddress) -- Only a small piece of the IPv6 address space has equivalent IPv4 addresses
fromIPv6ToIPv4 ip@(IPAddress6 _ secondWord) = if (isIPv4MappedAddress ip) then (Just (IPAddress (fromInteger (toInteger ((.&.) 0xffffffff secondWord))))) else Nothing


ichar :: Char -> Parser Char
ichar c = char (toLower c) <|> char (toUpper c)

istring :: String -> Parser String
istring s = (mapM ichar s) <?> ("Case Insensitive \"" ++ s ++ "\"")

parseWhitespace = (try (many (oneOf " \n"))) <?> "parseWhitespace"

parseDOT :: Parser DOTGraph
parseDOT = do
  strictKeyword <- optional (istring "strict")
  optional parseWhitespace
  graphType <- (try (istring "graph")) <|> (istring "digraph")
  optional parseWhitespace
  graphId <- (optional parseGraphID) <?> "parseDOT parseGraphID"
  optional parseWhitespace
  statements <- (between (char '{') (char '}') parseDOTStatements) <?> "parseDOT statements"
  return $ case (strictKeyword, graphType) of
    (Nothing, "graph") -> DOTNonStrictGraph graphId statements
    (Just "strict", "graph") -> DOTStrictGraph graphId statements
    (Nothing, "digraph") -> DOTNonStrictDigraph graphId statements
    (Just "strict", "digraph") -> DOTStrictDigraph graphId statements

-- Maybe extend this to recognize different id types later?
-- currently left at dummy solution so we can move on to the
-- meat of graph parsing: nodes & edges
parseGraphID :: Parser String
parseGraphID = (some (noneOf "{}:=,; ][")) <?> "parseGraphID"


parseDOTStatements :: Parser [DOTStatement]
parseDOTStatements = do
  statements <- (sepEndBy (try parseStatement) (char ';')) <?> "parseDOTStatements sepEndBy"
  (parseWhitespace <?> "parseDOTStatements parseWhitespace")
  return statements
  


parseCompassPt = choice [string "ne", string "nw", string "n", string "se", string "sw", string "s", string "w", string "e", string "c", string "_"]

data Port =
  Port (Maybe String) (Maybe String)
  deriving (Eq, Show)

parsePort = ((try (char ':' >> (try $ parseCompassPt <* notFollowedBy (char ':')) >>= \cp -> (parsePortEnd) >> return (Port Nothing (Just cp)))) <?> "Port Without Identifier" ) <|> ((char ':' >> parseGraphID >>= \gId -> optional (char ':' >> parseCompassPt) >>= \mCp -> (parsePortEnd) >> return (Port (Just gId) mCp)) <?> "Port With Identifier" )

data NodeId =
  NodeId String (Maybe Port)
  deriving (Eq, Show)

parseNodeId = (do
  idString <- parseGraphID 
  maybePort <- (optional (try parsePort)) 
  return (NodeId idString maybePort)) <?> "parseNodeId"

parsePortEnd = ((char ' ') >> return ()) <|> eof 

parseSingleAttribute = do
  attrName <- parseGraphID
  char '='
  attrValue <- parseGraphID
  return (attrName, attrValue)

parseAttrList :: Parser [[(String, String)]]
parseAttrList = do
  many (between (char '[') (char ']') (sepBy (parseSingleAttribute)(oneOf ",;")))


parseGraphAttributeStatement = do
  attrName <- parseGraphID
  char '='
  attrValue <- parseGraphID
  return (attrName, attrValue)

data DotNode = 
  DotNode NodeId [(String, String)]
  deriving (Eq, Show)

parseNodeStatement :: Parser DOTStatement
parseNodeStatement = (do
  (trace "parsing node statement" (return ()))
  nodeId <- parseNodeId
  (trace "parsingAttrList" (return ()))
  attrList <- parseAttrList
  (trace "attrList parsed" (return ()))
  return $  DOTNodeStatement nodeId attrList) <?> "parseNodeStatement"

parseAttrStatementType :: Parser AttrStatementType
parseAttrStatementType = (do
  typeString <- (try (string "node")) <|> (try (string "edge")) <|> (try (string "graph"))
  return $ case (typeString) of
    "node" -> NodeType
    "edge" -> EdgeType
    "graph" -> GraphType) <?> "parseAttrStatementType"

parseAttrStatement :: Parser DOTStatement 
parseAttrStatement = do
  (trace "parsing attr statement" (return ()))
  statementType <- parseAttrStatementType
  char ' '
  attrList <- parseAttrList
  return $ DOTAttrStatement statementType attrList 

data AttrStatementType = NodeType | EdgeType | GraphType
  deriving (Eq, Show)

data DOTStatement =
    DOTNodeStatement NodeId [[(String, String)]]
  | DOTEdgeStatement EdgeList [[(String, String)]]
  | DOTAttrStatement AttrStatementType [[(String, String)]]
  | DOTSubgraphStatement DOTSubgraph
  | DOTSingleAttrStatement (String, String)
  deriving (Eq, Show)

data DOTSubgraph =
  DOTSubgraph (Maybe String) [DOTStatement]
  deriving (Eq, Show)

data DOTGraph =
    DOTStrictGraph (Maybe String) [DOTStatement]
  | DOTNonStrictGraph (Maybe String) [DOTStatement]
  | DOTStrictDigraph (Maybe String) [DOTStatement]
  | DOTNonStrictDigraph (Maybe String) [DOTStatement]
  deriving (Eq, Show)

parseSingleAttrStatement :: Parser DOTStatement
parseSingleAttrStatement = do
  attribute <- parseSingleAttribute
  (return $ DOTSingleAttrStatement attribute) <?> "SingleAttrStatement"

parseSubgraphStatement :: Parser DOTStatement
parseSubgraphStatement = do
  (trace "parsing subgraph statement" (return ()))
  theSubgraph <- parseSubgraph
  (return $ DOTSubgraphStatement theSubgraph) <?> "SubgraphStatement"

parseSubgraph :: Parser DOTSubgraph
parseSubgraph = do
  subgraphId <- optional (string "subgraph" >> optional (char ' ' >> parseGraphID))
  subgraphStatements <- between (char '{') (char '}') (sepBy (parseStatement)(char ';'))
  return $ DOTSubgraph (join subgraphId) subgraphStatements

parseStatement = do
  (trace "calling parseWhitespace" parseWhitespace)
  statement <- trace "parsing statement" ((try parseSubgraphStatement) <|> (try parseAttrStatement) <|> (try parseEdgeStatement) <|> (try parseNodeStatement)) <?> "parseStatement statement"
  trace ("parsed statement" ++ (show statement)) (return ())
  parseWhitespace
  return statement


data EdgeList = 
  EdgeList (Either NodeId DOTSubgraph) EdgeListTail
  deriving (Eq, Show)

parseEdgeList :: Parser EdgeList
parseEdgeList = do
  firstEnd <- parseEdgeEnd
  char ' '
  listTail <- parseEdgeListTail
  return $ EdgeList firstEnd listTail


parseEdgeListTail :: Parser EdgeListTail
parseEdgeListTail = do
  edgeOp <- parseEdgeOp
  char ' '
  edgeEnd <- parseEdgeEnd
  rest <- optional (try parseEdgeListTail)
  return $ case (rest) of 
             Nothing -> EdgeListTail edgeOp edgeEnd Nil
             Just myTail -> EdgeListTail edgeOp edgeEnd myTail


parseEdgeEnd :: Parser (Either NodeId DOTSubgraph)
parseEdgeEnd = (parseNodeId >>= \nId -> return (Left nId)) <|> (parseSubgraph >>= \sg -> return (Right sg))

parseEdgeOp :: Parser EdgeOp
parseEdgeOp = do
  opText <- (string "->") <|> (string "--")
  return $ case (opText) of
             "->" -> ArrowEdge
             "--" -> LineEdge

data EdgeListTail =
    EdgeListTail EdgeOp (Either NodeId DOTSubgraph) EdgeListTail
  | Nil
  deriving (Eq, Show)

data EdgeOp = ArrowEdge | LineEdge
  deriving (Eq)

instance Show EdgeOp where
  show ArrowEdge = "->"
  show LineEdge = "--"

parseEdgeStatement :: Parser DOTStatement
parseEdgeStatement = do
  (trace "parsing edge statement" (return()))
  edgeList <- parseEdgeList
  parseWhitespace
  attrList <- parseAttrList
  (return $ DOTEdgeStatement edgeList attrList) <?> "parseEdgeStatement"


attrStatement = "node [foo=bar][biz=baz;wuz=was]"
nodeStatement = "foo[what=the;this=is][a=node]"

sampleSubgraph = "subgraph mySubgraph{" ++ nodeStatement ++ ";" ++ nodeStatement ++ "}"

sampleEdgeStatement = "C_0 -- H_0 [type=s];"

-- AT EOD 6/25/17 finished parsing edge statements
-- resume work here, adding support for attr_stmt next

simpleDOTExample = "\
\graph ethane {\n\
\     C_0 -- H_0 [type=s];\n\
\     C_0 -- H_1 [type=s];\n\
\     C_0 -- H_2 [type=s];\n\
\     C_0 -- C_1 [type=s];\n\
\     C_1 -- H_3 [type=s];\n\
\     C_1 -- H_4 [type=s];\n\
\     C_1 -- H_5 [type=s];\n\
\}"

main = do
  contents <- readFile "/home/michael/Downloads/catalogueB.html"
  parsedDoc <- return $ parseTags contents
  pElements <- return $ collectOnlyPTrees parsedDoc
  result <- return $ concat (map mapCatalogBRow pElements)
  output <- return $ ("<html><body>" ++ (renderTags result) ++ "</body></html>")
  putStr output 

main' = do
  contents <- readFile "/home/michael/Downloads/8_12deg.html"
  parsedDoc <- return $ parseTags contents
  pElements <- return $ collectOnlyPTrees parsedDoc
  numberedTrees <- return $ numberParagraphs pElements 683 
  result <- return $ concat (map mapSequentialRow numberedTrees)
  output <- return $ renderTags result
  putStr output

isPTag :: Tag String -> Bool
isPTag (TagOpen "p" _) = True
isPTag _ = False

isNotClosePTag :: Tag String -> Bool
isNotClosePTag (TagClose "p") = False
isNotClosePTag _ = True

collectOnlyPTrees :: [Tag String] -> [[Tag String]]
collectOnlyPTrees [] = []
collectOnlyPTrees (t:ts) = if (isPTag t) then ([t] ++ innerTags ++ [r]) : (collectOnlyPTrees rest) else (collectOnlyPTrees ts)
  where innerTags = takeWhile isNotClosePTag ts
        r:rest = dropWhile isNotClosePTag ts

numberParagraphs [] _ = []
numberParagraphs (pTree:rest) acc =
  case (hasCatalogueNumber pTree) of
    True -> (Just acc, pTree) : numberParagraphs rest (acc+1)
    False -> (Nothing, pTree) : numberParagraphs rest acc

takePTree tags = (upToClose ++ [c], rest)
  where upToClose = takeWhile isNotClosePTag tags
        c = head $ dropWhile isNotClosePTag tags
        rest = tail $ dropWhile isNotClosePTag tags

--isTagText t = case (t) of
--  (TagText _) -> True
--  (_) -> False

--mapSequentialRow :: [Tag String] -> [Tag String]
mapSequentialRow (Nothing, p) = p
mapSequentialRow (Just i, p) = case (firstText) of
  (Nothing) -> p
  (Just (TagText "\160")) -> p
  _ -> mapCatalogueRow i p
  where firstText = find isTagText p

mapCatalogBRow p = case (isTextWithCatalogueBNumber <$> lastText) of
  (Nothing) -> p
  (Just False) -> p
  (Just True) -> replaceCatalogueBNumberTag (parseString catalogBNumberParser mempty txt) p
                   where (Just (TagText txt)) = lastText
  where lastText = find isTagText (reverse p)

hasCatalogueNumber ts = case (find isTextWithCatalogueNumber ts) of 
  (Nothing) -> False
  (_) -> True

isTextWithCatalogueNumber :: Tag String -> Bool
isTextWithCatalogueNumber t = (isTagText t) && (parseSucceeds catalogNumberParser txt)
  where (TagText txt) = t

isTextWithCatalogueBNumber t = (isTagText t) && (parseSucceeds catalogBNumberParser txt)
  where (TagText txt) = t

parseSucceeds p str = case (parseString p mempty str) of
  (Success _) -> True
  _ -> False
  
mapCatalogueRow i p = case (i<=74, catNumberTag) of
  (_, Nothing) -> p
  (True, _) -> p
  (False, Just (TagText txt)) -> replaceCatalogueNumberTag (parseString catalogNumberParser mempty txt) p
  where catNumberTag = find isTextWithCatalogueNumber p




replaceCatalogueNumberTag (Success parseResult) tags = beforeTag ++ [(TagText ((show ((fst parseResult) + 20)) ++ (snd parseResult)))] ++ afterTag
  where beforeTag = takeWhile (not . isTextWithCatalogueNumber) tags
        (_:afterTag) = dropWhile (not . isTextWithCatalogueNumber) tags

replaceCatalogueBNumberTag (Success parseResult) tags = beforeTag ++ [(convertParseResultToNewTag originalTag parseResult)] ++ afterTag
  where beforeTag = takeWhile (not . isTextWithCatalogueBNumber) tags
        (originalTag:afterTag) = dropWhile (not . isTextWithCatalogueBNumber) tags

convertParseResultToNewTag :: Tag String -> ([Char], Integer, Maybe [Char], [Char]) -> Tag String
convertParseResultToNewTag originalTag (beforeNumber, mainNumber, Nothing, afterNumber) | mainNumber <= 74 = originalTag
                                                                                        | otherwise = (TagText (beforeNumber ++ (show (mainNumber + 20)) ++ afterNumber))
convertParseResultToNewTag originalTag (beforeNumber, mainNumber, (Just secondaryNumber), afterNumber) | mainNumber < 55 = originalTag 
                                                                                                       | otherwise = (TagText (beforeNumber ++ (show (mainNumber + 20)) ++ "." ++ secondaryNumber ++ afterNumber))


-- updateCatalogueNumber (mainNum, rest) = if (mainNum <= 

-- catalogNumberParser = do
--   mainNumber <- some digit
--   secondaryNumber <- try (optional (char '.' >> some digit >>= \sNum -> return sNum)) <|> (char '.' >> return Nothing)
--   rest <- many anyChar
--   return (mainNumber, secondaryNumber, rest)

catalogNumberParser = do
  mainNumber <- some digit
  rest <- many anyChar
  return (read mainNumber::Integer, rest)

catalogBNumberParser = do
  (beforeNumber, (mainNumber, secondaryNumber, rest)) <- manyTill' anyChar (try catalogBEntryEndParser) <?> "beforeNumber"
  return (beforeNumber, mainNumber, secondaryNumber, rest)

catalogBEntryEndParser = do
  mainNumber <- some digit <?> "mainNumber"
  secondaryNumber <- optional (char '.' >> some digit) <?> "secondaryNumber"
  rest <- many anyChar <?> "rest"
  return (read mainNumber::Integer, secondaryNumber, rest)


-- manyTill' p end does the same thing as manyTill, but instead of
-- just returning any array of results from parser p, it returns
-- a tuple where the first component is the array of results and
-- the second component is the result of parsing end
manyTill' :: Parser a -> Parser end -> Parser ([a], end)
manyTill' p end = go where go = (end >>= \e -> (return ([], e))) <|> ((myColon) <$> p <*> go)

myColon x (l,e) = (x:l, e)






