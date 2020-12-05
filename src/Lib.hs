module Lib
  ( monthlyBudget,
  )
where

import qualified CashFlow
import           Data.Maybe (fromMaybe)
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
        if (isTransfer row)
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
getDescription table row = case (Table.getCell table "Description" row) of
  Nothing -> "No Description"
  Just "" -> getTransactionType table row
  Just d  -> d

-- TODO: understand how to map over maybes correctly, so I'm not unboxing and reboxing
getAmount :: Table.Table -> Table.Row -> CashFlow.Amount
getAmount table row = case (Table.getCell table "Amount" row) of
  Nothing -> 0
  Just a  -> CashFlow.readAmount a

getTransactionType :: Table.Table -> Table.Row -> String
-- getTransactionType table row = fromMaybe "No Transaction Type" (Table.getCell table "Transaction Type" row)
getTransactionType = getDate

getDate :: Table.Table -> Table.Row -> String
getDate table row = fromMaybe "" (Table.getCell table "Date" row)

-- getStartAndEndDates :: Table.Table -> Table.Row -> (String, String) -- (StartDate, EndDate) // Does haskell core have a date type with parsing functions?
-- getStartAndEndDates table row = case Table.getCell table "Date" row of
--   Nothing -> ("No date", "No Date")
--   Just d -> (Date.convert d, Date.convert d)

-- Get list of dates as raw string values from the csv
-- Assume they are ordered chronologically, get the first and last entry
-- Is there a Date library to do the parsing? If not, how to do the thing?
