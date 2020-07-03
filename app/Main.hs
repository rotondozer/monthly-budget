module Main where

import           Lib                            ( monthlyBudget )
import           Text.Read                      ( readMaybe )

main :: IO ()
main = do
    putStrLn "Path to CSV (relative to this program):"
    path     <- getLine
    contents <- readFile path
    cashDiff <- getCashDiff
    writeFile "../../Downloads/monthly-budget.csv"
              (monthlyBudget cashDiff contents)
    print cashDiff

getCashDiff :: IO Float
getCashDiff = do
    putStrLn "How much cash on-hand now?"
    remainingCash <- getLine >>= parseCashInput (return, getCashDiff)
    startingCash  <- getStartingCash
    return (remainingCash - startingCash)
  where
    getStartingCash :: IO Float
    getStartingCash = do
        putStrLn "No info found for cash on-hand to start the month..." -- TODO 
        putStrLn "How much did you have on-hand at the start of this month?"
        sCash <- getLine
        parseCashInput (return, getStartingCash) sCash

parseCashInput :: (Float -> IO Float, IO Float) -> String -> IO Float
parseCashInput (onSuccess, onFail) cash = case readMaybe cash of
    Nothing  -> putStrLn "Needs to be a number. Try Again." >> onFail
    Just amt -> onSuccess amt
