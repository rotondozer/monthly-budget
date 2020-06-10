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

expenses :: Table.Table -> Map.Map String String
expenses table@(headers : rows) = Map.fromList $ map
    (\row ->
        ( fromMaybe "No Description" (getDescription row)
        , fromMaybe "0"              (getAmount row)
        )
    )
    rows
  where
    getDescription = Table.getCell table "\"Description\""
    getAmount      = Table.getCell table "\"Amount\""
