module Main where

import qualified Date
-- import qualified Debug.Trace as Debug
import Lib (monthlyBudget)
import qualified Table
import Text.Read (readMaybe)

main :: IO ()
main = do
  table <- csvToTable
  let ds = Table.getColumn table "Date"
  let wPath = "./generated_reports/" ++ yearPrefix (Date.getYears ds) ++ "." ++ path (Date.getMonths ds)
  cashDiff <- getCashDiff
  writeFile wPath $ monthlyBudget cashDiff table
  putStrLn $ "Your monthly budget can be found at: " ++ wPath

-- TODO: check for a CSV in some designated input folder before requesting it.
csvToTable :: IO Table.Table
csvToTable =
  putStrLn "Path to CSV (relative to this program):"
    >> getLine
      >>= readFile
      -- debugging shortcut: uncomment below and comment above
      -- readFile "../../Downloads/EXPORT.csv"
      >>= toTable
  where
    toTable :: String -> IO Table.Table
    toTable c = case Table.fromCSV c of
      Left e -> putStrLn "Failed reading CSV!" >> print e >> csvToTable
      Right t -> return t

getCashDiff :: IO Float
getCashDiff = do
  putStrLn "How much cash on-hand at end of period?"
  remainingCash <- getLine >>= readCashInputOr getCashDiff
  startingCash <- getStartingCash
  return (remainingCash - startingCash)
  where
    getStartingCash :: IO Float
    getStartingCash = do
      putStrLn "How much did you have on-hand at the start of this period?"
      getLine >>= readCashInputOr getStartingCash
    readCashInputOr :: IO Float -> String -> IO Float
    readCashInputOr onFail cash = case readMaybe cash of
      Nothing -> putStrLn "Needs to be a number. Try Again." >> onFail
      Just amt -> return amt

path :: [String] -> String
path [] = "budget.csv"
path (m : ms) = m ++ "-" ++ path ms

-- | For now this, returns a string to append to the month naming of the file
-- | Right now, I'm thinking like ../reports/2020/april-budget.csv, but the year dir
-- | needs to exist for this work.
-- TODO: commands to make the directory.
yearPrefix :: [String] -> String
yearPrefix ys = if onlyOneYear then firstYear else firstYear ++ "-" ++ last ys
  where
    onlyOneYear = length ys == 1
    firstYear = head ys