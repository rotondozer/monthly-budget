module Main where

import           Lib                            ( monthlyBudget )
import           Text.Read                      ( readMaybe )

main :: IO ()
main = do
    putStrLn "Path to CSV (relative to this program):"
    path     <- getLine
    contents <- readFile path
    -- putStrLn "How much cash on hand at the START of the month?"
    -- startingCash <- getLine
    -- putStrLn "How much cash on hand at the END of the month?"
    -- endingCash <- getLine
    cash     <- getCashInput
    let budget = monthlyBudget contents
    writeFile "../../Downloads/monthly-budget.csv" budget
    print budget

getCashInput :: IO Float
getCashInput = do
    putStrLn "How much cash on hand?"
    startingCash <- getLine
    case readMaybe startingCash of
        Nothing  -> putStrLn "Needs to be a number. Try Again." >> getCashInput
        Just amt -> return amt
