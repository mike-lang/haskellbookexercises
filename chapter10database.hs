

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime 
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

itemToTime :: DatabaseItem -> [UTCTime]
itemToTime (DbDate t) = [t]
itemToTime _ = []

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dis = foldr (\a b -> (itemToTime a) ++ b) [] dis


itemToNumber :: DatabaseItem -> [Integer]
itemToNumber (DbNumber n) = [n]
itemToNumber _ = []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dis = foldr (\a b -> (itemToNumber a) ++ b) [] dis

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dis = maximum (filterDbDate theDatabase)

sumDb :: [DatabaseItem] -> Integer
sumDb dis = sum (filterDbNumber theDatabase)

avgDb :: [DatabaseItem] -> Double
avgDb dis = (fromIntegral (sum numbers)) / (fromIntegral (length numbers))
  where numbers = filterDbNumber theDatabase
