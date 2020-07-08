module Main where

import           Lib                            ( monthlyBudget )
import           Text.Read                      ( readMaybe )

main :: IO ()
main = do
    putStrLn "Path to CSV (relative to this program):"
    contents <- getLine >>= readFile
    cashDiff <- getCashDiff
    let wPath = "../../Downloads/monthly-budget.csv"
    writeFile wPath $ monthlyBudget cashDiff contents
    putStrLn $ "Your monthly budget can be found at: " ++ wPath
    print cashDiff -- Just for dev purposes

getCashDiff :: IO Float
getCashDiff = do
    putStrLn "How much cash on-hand now?"
    remainingCash <- getLine >>= readCashInputOr getCashDiff
    startingCash  <- getStartingCash
    return (remainingCash - startingCash)
  where
    getStartingCash :: IO Float
    getStartingCash = do
        putStrLn "No info found for cash on-hand to start the month..." -- TODO 
        putStrLn "How much did you have on-hand at the start of this month?"
        getLine >>= readCashInputOr getStartingCash
    readCashInputOr :: IO Float -> String -> IO Float
    readCashInputOr onFail cash = case readMaybe cash of
        Nothing  -> putStrLn "Needs to be a number. Try Again." >> onFail
        Just amt -> return amt
