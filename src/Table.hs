module Table
    ( Row
    , Table
    , toTable
    , getCell
    )
where

import           Data.List
import           Data.List.Split                ( splitOneOf )

type Header = String
type Row = [String]
type Table = [Row]

-- CSV -> Table
-- [ ["Transaction Type", "Date",   "Account Type", "Description", "Amount", "Reference No.", "Credits", "Debits"]
-- , ["DEBIT",            "6/2/20", "Checking",     "VENMO",       "-100",   "",              "",        "-$100"]
-- , ["CREDIT",           "6/3/20", "Checking",     "PAYCHECK",    "500",    "",              "$500",    ""]
-- ]
toTable :: String -> Table
toTable csv = [ map stringCleanup row | row <- toRowsAndColumns csv ]

-- idk what to call this, dirtyTable? The strings that comprise the Table still need to be cleaned up!
toRowsAndColumns :: String -> Table
toRowsAndColumns csv = map (splitOneOf ",") (lines csv)

getCell :: Table -> Header -> Row -> Maybe String
getCell (headers : rows) header row =
    (header `elemIndex` headers) >>= \i -> return (row !! i)

-- Remove excess spaces and unnest strings
stringCleanup :: String -> String
stringCleanup = unwords . words . unnestString

-- "\"Transaction Type\"" -> "Transaction Type"
unnestString :: String -> String
unnestString str = [ c | c <- str, c /= '"', c /= '/' ] -- surely there's a more Haskelly way to do a filter or...
