module Table
    ( Row
    , Table
    , toCSV
    , toTable
    , getCell
    )
where

import           Data.List
import           Data.List.Split                ( splitOneOf )
import qualified Parser

type Header = String
type Row = [String]
type Table = [Row]

-- CSV -> Table
-- [ ["Transaction Type", "Date",   "Account Type", "Description", "Amount", "Reference No.", "Credits", "Debits"]
-- , ["DEBIT",            "6/2/20", "Checking",     "VENMO",       "-100",   "",              "",        "-$100"]
-- , ["CREDIT",           "6/3/20", "Checking",     "PAYCHECK",    "500",    "",              "$500",    ""]
-- ]
toTable :: String -> Table
toTable contents = case Parser.parseCSV contents of
    Left  e     -> [["Error parsing input:" ++ (show e)]]
    Right table -> table

toCSV :: Table -> String
toCSV = genCsvFile

getCell :: Table -> Header -> Row -> Maybe String
getCell (headers : rows) header row =
    (header `elemIndex` headers) >>= \i -> return (row !! i)

-- Other copypasta from Data.CSV -- 
-- https://hackage.haskell.org/package/MissingH-1.4.3.0/docs/src/Data.CSV.html#genCsvFile

genCsvFile :: [[String]] -> String
genCsvFile inp = unlines . map csvline $ inp
  where
    csvline :: [String] -> String
    csvline l = concat . intersperse "," . map csvcells $ l
    csvcells :: String -> String
    csvcells "" = ""
    csvcells c  = '"' : convcell c ++ "\""
    convcell :: String -> String
    convcell c = unwords (words (concatMap convchar c))
    convchar :: Char -> String
    convchar '"' = "\"\""
    convchar x   = [x]
