module Date (toLongDate, getStartAndEndDates) where

import Data.List.Split (linesBy)
import Data.Maybe (fromMaybe)
import qualified Debug.Trace as Debug
import Text.Read (readMaybe)

months :: [String]
months =
  [ "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december"
  ]

type CSVDate = String -- "4/13/19"

type RawDate = Int -- 20190413

-- | "4/13/19" -> "April 13, 2019"
toLongDate :: CSVDate -> String
toLongDate = gettem . toYDMList
  where
    gettem :: [String] -> String
    gettem (y : d : m : _) = getMonth m ++ " " ++ d ++ ", 20" ++ y

getStartAndEndDates :: [CSVDate] -> (String, String)
getStartAndEndDates dates = (toLongDate $ head dates, toLongDate $ last dates) -- assume they're already sorted, for now

------ SORTING -------
-- Convert dates into 8 digit representations that can be easily sorted
-- ---> i.e. "4/13/2020" -> 20200413 (make sure single digits get padded with 0s)
-- ---> should this be the raw Date representation? (as far as this module cares)
-- 1. Turn date into [String] -> ["4", "13", "2020"]
-- 2. Read the Int from each string, padding single digits for Ds or Ms with a leading 0
-- ---> which should come first, the padding or the conversion to int?
-- ---> [04, 13, 2020]
-- 3. Sort each date to the form YMD --> [2020, 04, 13]
-- 4. Convert to Int --> 20200413
-- 5. With each date converted, sort this array using the basic `sort` function
-- ---> [20190413, 20200213, 20200403, 20200413, 20200413, 20200413, 20201225]
-- 6. Use these as the basic Date structure, to then convert to whatever form...?

--    PRIVATE

-- | 20200413 -> "04/13/2020"
-- fromDateToCSVDate :: RawDate -> CSVDate
-- fromDateToCSVDate = fromDateToYDMList

-- | 20200413 -> ["2020", "04", "13"]
fromDateToYDMList :: RawDate -> [String]
fromDateToYDMList int = [show int]

-- | ["4/13/2020", "4/12/2020"] -> [20200412, 20200413]
sortie :: [CSVDate] -> [RawDate]
sortie = map toDateFromCSVDate

-- | "4/13/2020" -> 20200413
toDateFromCSVDate :: CSVDate -> RawDate
toDateFromCSVDate = toDateFromYDMList . toYDMList

-- | ["4", "13" , "2020"] -> 20200413
toDateFromYDMList :: [String] -> RawDate
toDateFromYDMList (y : m : d : _) = read (y ++ padZero m ++ padZero d)

-- TODO: look into less type conversion, if possible.

-- | Parse an Int from a String, and lead it with a 0 if
-- | it is a single digit. Then return that number as a String.
-- | --> (I'm doing this for now because I'm not sure how haskell will treat the Int 01,
-- | --> but I know I can concat it as a string then plop it into a number from there)
-- | If an Int cannot be parsed, it will resolve to "00".
padZero :: String -> String
padZero str
  | readInt str > 10 = str
  | otherwise = "0" ++ str

-- | "4/13/2020" -> ["4", "13", "2020"]
toYDMList :: CSVDate -> [String]
toYDMList = reverse . linesBy (== '/')

-- Take a Month number and get the string
getMonth :: String -> String
getMonth numStr = months !! (readInt numStr - 1)

readInt :: String -> Int
readInt str = fromMaybe 0 (readMaybe str)

debugAndReturn x = Debug.trace ("DEBUGGER: " ++ show x) x
