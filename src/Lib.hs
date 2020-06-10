module Lib
    ( monthlyBudget
    )
where

import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Table

type Expenses = Map.Map String Float

monthlyBudget :: String -> Expenses
monthlyBudget csv = expenses (Table.toTable csv)

expenses :: Table.Table -> Expenses
expenses table@(_ : rows) = Map.fromList
    $ map (\r -> (description r, amount r)) rows
  where
    amount      = getAmount table
    description = getDescription table


getDescription :: Table.Table -> Table.Row -> String
getDescription table row =
    fromMaybe "No Description" (Table.getCell table "\"Description\"" row)

getAmount :: Table.Table -> Table.Row -> Float
getAmount table row =
    read (fromMaybe "0" (Table.getCell table "\"Amount\"" row)) :: Float
