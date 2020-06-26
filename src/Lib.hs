module Lib
    ( monthlyBudget
    )
where

import           Data.Maybe                     ( fromMaybe )
import qualified Table
import qualified CashFlow

monthlyBudget :: String -> [[String]]
monthlyBudget =
    CashFlow.fromMapsToMatrix
        . CashFlow.totalsFromDebsAndCreds
        . sumDebsAndCreds
        . toDebsAndCreds
  where
    toDebsAndCreds  = tableToDebitsAndCredits . Table.toTable
    sumDebsAndCreds = map CashFlow.toMapWithAmountSum

tableToDebitsAndCredits :: Table.Table -> [[CashFlow.CashFlow]]
tableToDebitsAndCredits table@(_ : rows) = foldl
    (\debsAndCreds row ->
        CashFlow.addToDebsAndCreds (description row, amount row) debsAndCreds
    )
    [[], []]
    rows
  where
    amount      = getAmount table
    description = getDescription table

getDescription :: Table.Table -> Table.Row -> CashFlow.Description
getDescription table row =
    fromMaybe "No Description" (Table.getCell table "Description" row)

getAmount :: Table.Table -> Table.Row -> CashFlow.Amount
getAmount table row = fromMaybe "0" (Table.getCell table "Amount" row)
