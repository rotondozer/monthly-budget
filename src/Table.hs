module Table
    ( toTable
    , Table
    )
where

import           Data.List.Split                ( splitOneOf )
import           Data.List

type Header = String
type Row = [String]
type Table = [Row]

-- CSV -> Table
-- [ ["Transaction Type", "Date",   "Account Type", "Description", "Amount", "Reference No.", "Credits", "Debits"]
-- , ["DEBIT",            "6/2/20", "Checking",     "VENMO",       "-100",   "",              "",        "-$100"]
-- , ["CREDIT",           "6/3/20", "Checking",     "PAYCHECK",    "500",    "",              "$500",    ""]
-- ]
toTable :: String -> Table
toTable csv =
    let tableHeaders = splitOneOf ",\\" header
        tableRows    = map (splitOneOf ",") rows
    in  tableHeaders : tableRows
    where (header : rows) = (lines csv)

columnIndexFor :: Header -> Table -> Maybe Int
columnIndexFor header (tableHeaders : _) = elemIndex header tableHeaders
