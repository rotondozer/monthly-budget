module Table
    ( Table
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
toTable csv = map (splitOneOf ",") (lines csv)

getCell :: Table -> Header -> Row -> Maybe String
getCell (headers : rows) header row =
    (header `elemIndex` headers) >>= \i -> return (row !! i)

