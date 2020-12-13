module Lib
  ( monthlyBudget,
  )
where

import qualified CashFlow
import qualified Data.Maybe as Maybe
import qualified Date
import qualified Table

monthlyBudget :: Float -> Table.Table -> String
monthlyBudget cashDiff =
  Table.toCSV . CashFlow.toMatrix . sumDebsAndCreds . toDebsAndCreds
  where
    toDebsAndCreds = tableToDebitsAndCredits cashDiff
    sumDebsAndCreds = map CashFlow.toUniqueListWithAmountSum

tableToDebitsAndCredits :: Float -> Table.Table -> [[CashFlow.CashFlow]]
tableToDebitsAndCredits cashDiff table@(_ : rows) =
  foldl
    ( \debsAndCreds row ->
        if isTransfer row
          then debsAndCreds
          else
            CashFlow.addToDebsAndCreds
              (description row, amount row)
              debsAndCreds
    )
    (CashFlow.addToDebsAndCreds ("CA$H DIFF", cashDiff) [[], []]) -- TODO: place this with totals for clarity, not really a debit/credit
    rows
  where
    amount = getAmount table
    description = getDescription table
    isTransfer r = getTransactionType table r == "TRANSFER"

getDescription :: Table.Table -> Table.Row -> CashFlow.Description
getDescription table row = case Table.getCell table "Description" row of
  Nothing -> "No Description"
  Just "" -> getTransactionType table row
  Just d -> d

getAmount :: Table.Table -> Table.Row -> CashFlow.Amount
getAmount table row =
  let amt = Table.getCell table "Amount" row
   in Maybe.maybe 0 CashFlow.readAmount amt

getTransactionType :: Table.Table -> Table.Row -> String
getTransactionType table row = Maybe.fromMaybe "No Transaction Type" (Table.getCell table "Transaction Type" row)

getDates :: Table.Table -> (String, String)
getDates t = Date.getStartAndEndDates $ Table.getColumn t "Date"
