module Lib
    ( monthlyBudget
    )
where

import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Table

type Expenses = Map.Map String String

monthlyBudget :: String -> Expenses
monthlyBudget csv = expenses (Table.toTable csv)

expenses :: Table.Table -> Expenses
expenses table@(_ : rows) = Map.fromListWith addAmounts
    $ map (\row -> (descriptionFrom row, amountFrom row)) rows
  where
    amountFrom      = getAmount table
    descriptionFrom = getDescription table

addAmounts :: String -> String -> String
addAmounts a1 a2 = show $ (read a1 :: Float) + (read a2 :: Float)

getDescription :: Table.Table -> Table.Row -> String
getDescription table row =
    fromMaybe "No Description" (Table.getCell table "\"Description\"" row)

getAmount :: Table.Table -> Table.Row -> String
getAmount table row = fromMaybe "0" (Table.getCell table "\"Amount\"" row)
