module Main where

import           Lib                            ( monthlyBudget )

main :: IO ()
main = do
    contents <- readFile "../../Downloads/EXPORT.CSV"
    let budget = monthlyBudget contents
    -- writeFile "../../Downloads/monthly-budget.txt" (budget)
    print budget
