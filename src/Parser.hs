module Parser (parseCSV) where

import           Text.ParserCombinators.Parsec

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
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r" <?> "end of line"
