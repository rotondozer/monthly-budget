module Lib
    ( monthlyBudget
    )
where

import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Table
import qualified CashFlow

monthlyBudget :: String -> [CashFlow.CashFlowTotals]
monthlyBudget csv = totaledCashFlow (Table.toTable csv)

totaledCashFlow :: Table.Table -> [CashFlow.CashFlowTotals]
totaledCashFlow table@(_ : rows) =
    let (d, c)  = tableToBudget table
        debits  = Map.fromListWith CashFlow.addAmounts d
        credits = Map.fromListWith CashFlow.addAmounts c
    in  [debits, credits]

tableToBudget :: Table.Table -> CashFlow.DebitsAndCredits
tableToBudget table@(_ : rows) = foldl
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
