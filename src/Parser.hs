module Parser
    ( parseCSV
    )
where

import           Text.ParserCombinators.Parsec
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromMaybe )
import           Text.Read                      ( readMaybe )

--          DATE            --

parseMonth :: String -> Maybe String
parseMonth = intToMonthName . head . sepMDYY

-- "6/2/20" -> [6, 2, 20]
sepMDYY :: String -> [Maybe Int]
sepMDYY = map readMaybe . (splitOn "/")

-- 6 -> "June"
intToMonthName :: Int -> Maybe String
intToMonthName num | num == 1  = Just "January"
                   | num == 2  = Just "February"
                   | num == 3  = Just "March"
                   | num == 4  = Just "April"
                   | num == 5  = Just "May"
                   | num == 6  = Just "June"
                   | num == 7  = Just "July"
                   | num == 8  = Just "August"
                   | num == 9  = Just "September"
                   | num == 10 = Just "October"
                   | num == 11 = Just "November"
                   | num == 12 = Just "December"
                   | otherwise = Nothing -- lol @ me

--              CSV             --
-- Begin copypasta --
-- http://book.realworldhaskell.org/read/using-parsec.html

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(stdin)"

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
