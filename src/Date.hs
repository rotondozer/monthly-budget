module Date (toLongDate, getMonths, getYears, getStartAndEndDates) where

import Data.List (sort)
import Data.List.Split (linesBy)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Text.Read (readMaybe)

type Month = String

months :: [Month]
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

type DateTriple = (Int, Int, Int) -- (YYYY, MM, DD)

type YMDList = [String] -- ["20", "4", "13"]

-- | "4/13/19" -> "April 13, 2019"
toLongDate :: CSVDate -> String
toLongDate = gettem . toYMDList
  where
    gettem :: [String] -> String
    gettem (y : m : d : _) = getLongMonth (readInt m) ++ " " ++ d ++ ", " ++ pad20 y

getStartAndEndDates :: [CSVDate] -> (CSVDate, CSVDate)
getStartAndEndDates dates =
  let sortedDates = sortAsRawDates dates
   in (fromDateToCSVDate $ head sortedDates, fromDateToCSVDate $ last sortedDates)

-- This is making the assumption we won't have a CSV report that can include the same month
-- from more than one year. Since this is 'monthly-budget', we're assuming we're dealing with
-- a few months at most.
getMonths :: [CSVDate] -> [String]
getMonths ds =
  let dtrips = map (fromDateToTriple . toDateFromCSVDate) ds
   in uniq (map (getLongMonth . getM) dtrips)

-- | ["4/13/20", "4/12/20", "4/13/21"] -> ["2020", "2021"]
getYears :: [CSVDate] -> [String]
getYears ds =
  let dtrips = map (fromDateToTriple . toDateFromCSVDate) ds
   in uniq (map (pad20 . show . getY) dtrips)

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

getM :: DateTriple -> Int
getM (_, m, __) = m

getY :: DateTriple -> Int
getY (y, _, __) = y

getD :: DateTriple -> Int
getD (_, __, d) = d

fromDateToTriple :: RawDate -> DateTriple
fromDateToTriple date =
  let (y : m : d : _) = map readInt (fromDateToYMDList date)
   in (y, m, d)

-- | 20200413 -> "04/13/2020"
fromDateToCSVDate :: RawDate -> CSVDate
fromDateToCSVDate = fromYMDListToCSVDate . fromDateToYMDList

-- | ["20", "4", "13"] -> "4/13/2020"
fromYMDListToCSVDate :: YMDList -> CSVDate
fromYMDListToCSVDate (y : m : d : _) = m ++ "/" ++ d ++ "/" ++ y

-- | 20200413 -> ["2020", "04", "13"]
fromDateToYMDList :: RawDate -> YMDList
fromDateToYMDList int =
  let (y, md) = splitAt 4 (show int)
      (m, d) = splitAt 2 md
   in [y, m, d]

-- | ["4/13/2020", "4/12/2020"] -> [20200412, 20200413]
sortAsRawDates :: [CSVDate] -> [RawDate]
sortAsRawDates = sort . map toDateFromCSVDate

-- | "4/13/2020" -> 20200413
toDateFromCSVDate :: CSVDate -> RawDate
toDateFromCSVDate = toDateFromYMDList . toYMDList

-- | ["20", "4", "13"] -> 20200413
toDateFromYMDList :: YMDList -> RawDate
toDateFromYMDList (y : m : d : _) = read (pad20 y ++ padZero m ++ padZero d)

-- TODO: look into less type conversion, if possible.

-- | Parse an Int from a String, and lead it with a 0 if
-- | it is a single digit. Then return that number as a String.
-- | --> (I'm doing this for now because I'm not sure how haskell will treat the Int 01,
-- | --> but I know I can concat it as a string then plop it into a number from there)
-- | If an Int cannot be parsed, it will resolve to "00".
padZero :: String -> String
padZero str
  | readInt str > 9 = str
  | otherwise = "0" ++ str

-- specific for years. Should rename?
pad20 :: String -> String
pad20 str
  | length str == 4 = str
  | length str == 2 = "20" ++ str
  | otherwise = "0000"

-- | "4/13/20" -> ["20", "4", "13"]
toYMDList :: CSVDate -> YMDList
toYMDList d = fromMDYtoYMD (linesBy (== '/') d)
  where
    fromMDYtoYMD :: [String] -> [String]
    fromMDYtoYMD (m : d : y : _) = [y, m, d]

-- Take a Month number and get the string
-- TODO: safely access the List
getLongMonth :: Int -> String
getLongMonth int = months !! (int - 1)

readInt :: String -> Int
readInt str = fromMaybe 0 (readMaybe str)

uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList -- prob not the most efficient
