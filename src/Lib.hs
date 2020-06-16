module Lib
    ( monthlyBudget
    )
where

import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Table
import qualified CashFlow

monthlyBudget :: String -> [CashFlow.CashFlowTotals]
monthlyBudget =
    CashFlow.toList . addAmounts . tableToDebitsAndCredits . Table.toTable
    where addAmounts = CashFlow.map' (Map.fromListWith CashFlow.addAmounts)

tableToDebitsAndCredits :: Table.Table -> CashFlow.DebitsAndCredits
tableToDebitsAndCredits table@(_ : rows) = foldl
    (\debsAndCreds row ->
        CashFlow.addToDebsAndCreds (description row, amount row) debsAndCreds
    )
    ([], [])
    rows
  where
    amount      = getAmount table
    description = getDescription table

getDescription :: Table.Table -> Table.Row -> CashFlow.Description
getDescription table row =
    fromMaybe "No Description" (Table.getCell table "Description" row)

getAmount :: Table.Table -> Table.Row -> CashFlow.Amount
getAmount table row = fromMaybe "0" (Table.getCell table "Amount" row)
