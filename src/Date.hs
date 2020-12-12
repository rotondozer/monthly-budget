module Date (convert, getStartAndEndDates) where

import Data.List.Split (linesBy)
import Data.Maybe (fromMaybe)
import qualified Debug.Trace as Debug
import Text.Read (readMaybe)

months :: [String]
months =
  [ "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  ]

type Date = String -- "4/13/19"

-- "4/13/19" -> "April 13, 2019"
convert :: Date -> String
convert = debugAndReturn . gettem . toMDYList
  where
    gettem :: [String] -> String
    gettem (m : d : y : _) = getMonth m ++ " " ++ d ++ ", 20" ++ y

getStartAndEndDates :: [Date] -> (String, String)
getStartAndEndDates dates = (convert $ head dates, convert $ last dates) -- assume they're already sorted, for now
  where
    sort :: [Date] -> [Date]
    sort ds = ds -- TODO

--    PRIVATE

toMDYList :: String -> [String]
toMDYList = linesBy (== '/')

-- Take a Month number and get the string
getMonth :: String -> String
getMonth numStr = months !! (readInt numStr - 1)
  where
    readInt :: String -> Int
    readInt str = fromMaybe 0 (readMaybe str)

debugAndReturn x = Debug.trace ("DEBUGGER: " ++ show x) x
