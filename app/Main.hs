module Main where

import qualified Date
import Lib (monthlyBudget)
import System.Directory (listDirectory)
import qualified Table
import Text.Read (readMaybe)

-- not sure if I'm gonna keep this, but it helps for debugging while writing this out.
data InputMethod = Requested | FromDir | Debugging | None

startingInputMethod :: InputMethod
startingInputMethod = FromDir -- Change this to play around

type MonthlyBudget = String

main :: IO [()]
main = do
  tables <- csvToTables startingInputMethod
  mapM doStuff tables
  where
    doStuff :: Table.Table -> IO ()
    doStuff t = do
      cashDiff <- getCashDiff (getStartAndEndDates t)
      writeToReports cashDiff t

writeToReports :: Float -> Table.Table -> IO ()
writeToReports cashDiff table = do
  let wPath = buildReportPath table
  writeFile wPath $ monthlyBudget cashDiff table
  putStrLn $ "Your monthly budget(s) can be found in " ++ wPath

getStartAndEndDates :: Table.Table -> (String, String)
getStartAndEndDates t = Date.getStartAndEndDates (t `Table.getColumn` "Date")

buildReportPath :: Table.Table -> FilePath
buildReportPath table =
  let ds = Table.getColumn table "Date"
   in "./generated_reports/" ++ yearPrefix (Date.getYears ds) ++ "." ++ path (Date.getMonths ds)

-- TODO: check for a CSV in some designated input folder before requesting it.
csvToTables :: InputMethod -> IO [Table.Table]
csvToTables inputMethod =
  case inputMethod of
    None -> putStrLn "Aborting. See ya." >> return [[]]
    Debugging -> readFile "../../Downloads/EXPORT.csv" >>= toTable >>= toIOArr
    Requested ->
      putStrLn "Path to CSV (relative to this program):"
        >> getLine
          >>= readFile
          >>= toTable
          >>= toIOArr
    FromDir -> do
      dirs <- listDirectory "./csvs"
      putStrLn $ "Reading " ++ show (length dirs) ++ " CSV files: " ++ show dirs
      mapM fpToTable dirs
      where
        fpToTable :: FilePath -> IO Table.Table
        fpToTable fp = readFile ("./csvs/" ++ fp) >>= toTable

toIOArr :: a -> IO [a]
toIOArr x = return [x]

toTable :: String -> IO Table.Table
toTable c = case Table.fromCSV c of
  Left e -> putStrLn "Failed reading CSV!" >> print e >> return [[]]
  Right t -> return t

getCashDiff :: (String, String) -> IO Float
getCashDiff (startDate, endDate) = do
  putStrLn $ "Enter cash diff between " ++ startDate ++ " and " ++ endDate
  getLine >>= readCashInputOr (getCashDiff (startDate, endDate))
  where
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