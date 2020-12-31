module Table
  ( Row,
    Table,
    toCSV,
    fromCSV,
    getCell,
    getColumn,
    create,
    addRow,
    rowSpacer,
    addSectionSeparator,
  )
where

import Data.List
import Data.Maybe as Maybe
import Debug.Trace as Debug
import qualified Parser
import Text.Parsec (ParseError)
import qualified Util

type ColHeader = String

type Row = [String]

type Table = [Row]

-- CSV -> Table
-- [ ["Transaction Type", "Date",   "Account Type", "Description", "Amount", "Reference No.", "Credits", "Debits"]
-- , ["DEBIT",            "6/2/20", "Checking",     "VENMO",       "-100",   "",              "",        "-$100"]
-- , ["CREDIT",           "6/3/20", "Checking",     "PAYCHECK",    "500",    "",              "$500",    ""]
-- ]

toCSV :: Table -> String
toCSV = genCsvFile

fromCSV :: String -> Either ParseError Table
fromCSV = Parser.parseCSV

getCell :: Table -> ColHeader -> Row -> Maybe String
getCell (headers : _) header row =
  (header `elemIndex` headers) >>= \i -> return (row !! i)

getColumn :: Table -> ColHeader -> [String]
getColumn t colH = tail' (Maybe.mapMaybe getCell' t)
  where
    getCell' = getCell t colH
    tail' :: [a] -> [a]
    tail' ls
      | null ls = Debug.trace "getColumn" []
      | otherwise = tail ls

create :: [ColHeader] -> Table
create colHs = [colHs]

addRow :: Row -> Table -> Table
addRow r t
  | null t = Debug.trace "no head for you" t
  | length (head t) /= length r = Util.logWarn t
  | otherwise = t ++ [r]

-- TODO: make this configurable
addSectionSeparator :: Table -> Table
addSectionSeparator = addRow ["", "----------"]

-- TODO: make this configurable for the Table's num of columns
rowSpacer :: Table -> Table
rowSpacer = addRow ["", ""]

-- Other copypasta from Data.CSV --
-- https://hackage.haskell.org/package/MissingH-1.4.3.0/docs/src/Data.CSV.html#genCsvFile

genCsvFile :: [[String]] -> String
genCsvFile = unlines . map csvline
  where
    csvline :: [String] -> String
    csvline l = concat . intersperse "," . map csvcells $ l
    csvcells :: String -> String
    csvcells "" = ""
    csvcells c = '"' : convcell c ++ "\""
    convcell :: String -> String
    convcell c = unwords (words (concatMap convchar c))
    convchar :: Char -> String
    convchar '"' = "\"\""
    convchar x = [x]
