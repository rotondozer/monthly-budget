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
import           Text.ParserCombinators.Parsec

type Header = String
type Row = [String]
type Table = [Row]

-- CSV -> Table
-- [ ["Transaction Type", "Date",   "Account Type", "Description", "Amount", "Reference No.", "Credits", "Debits"]
-- , ["DEBIT",            "6/2/20", "Checking",     "VENMO",       "-100",   "",              "",        "-$100"]
-- , ["CREDIT",           "6/3/20", "Checking",     "PAYCHECK",    "500",    "",              "$500",    ""]
-- ]
toTable :: String -> Table
toTable contents = case parse csvFile "(stdin)" contents of
    Left  e -> [["Error parsing input:" ++ (show e)]]
    Right r -> r

toCSV :: Table -> String
toCSV = genCsvFile

-- This implementation only works when there are no commas within the table cells. When there are, 
-- those substrings get split up, resulting in too many cells for that row. Copied the example used in the Parsec
-- library to fix the bugs I was encountering, although I would've liked to write my own solution.
toRowsAndColumns :: String -> Table
toRowsAndColumns csv = map (splitOneOf ",") (lines csv)

getCell :: Table -> Header -> Row -> Maybe String
getCell (headers : rows) header row =
    (header `elemIndex` headers) >>= \i -> return (row !! i)

-- Begin copypasta --
-- http://book.realworldhaskell.org/read/using-parsec.html

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell :: GenParser Char st String
quotedCell = do
    char '"'
    content <- many quotedChar
    char '"' <?> "quote at end of cell"
    return content

quotedChar :: GenParser Char st Char
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol :: GenParser Char st String
eol =
    try (string "\n\r")
        <|> try (string "\r\n")
        <|> string "\n"
        <|> string "\r"
        <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

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
    convcell c = concatMap convchar c
    convchar '"' = "\"\""
    convchar x   = [x]
