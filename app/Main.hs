module Main where

import Debug.Trace (trace)
import Lib (monthlyBudget)
import qualified Parser
import qualified Table
import Text.Read (readMaybe)

main :: IO ()
main = do
  table <- csvToTable
  cashDiff <- getCashDiff
  let wPath = "../../Downloads/monthly-budget.csv"
  writeFile wPath $ monthlyBudget cashDiff table
  putStrLn $ "Your monthly budget can be found at: " ++ wPath

csvToTable :: IO Table.Table
csvToTable =
  putStrLn "Path to CSV (relative to this program):"
    >> getLine
    >>= readFile
    >>= toTable
  where
    toTable :: String -> IO Table.Table
    toTable c = case Parser.parseCSV c of
      Left e -> trace (show e) putStrLn "Failed reading CSV!" >> csvToTable
      Right table -> return table

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
