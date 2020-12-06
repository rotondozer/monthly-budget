module Date (convert) where

import           Data.List.Split (linesBy)
import           Data.Maybe      (fromMaybe)
import qualified Debug.Trace     as Debug
import           Text.Read       (readMaybe)

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

-- TODO: "4/13/19" -> "April 13, 2019"
convert :: String -> String
convert = debugAndReturn . gettem . splittemUp
  where
    splittemUp :: String -> [String]
    splittemUp s = linesBy (== '/') s
    gettem :: [String] -> String
    gettem (m : d : y : _) = getMonth m ++ " " ++ d ++ ", 20" ++ y

-- Take a Month number and get the string
getMonth :: String -> String
getMonth numStr = months !! (readInt numStr - 1)
  where
    readInt str = fromMaybe 0 (readMaybe str)

debugAndReturn x = Debug.trace ("DEBUGGER: " ++ show x) x
