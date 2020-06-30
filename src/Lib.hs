module Lib
    ( monthlyBudget
    )
where

import           Data.Maybe                     ( fromMaybe )
import qualified Table
import qualified CashFlow

monthlyBudget :: String -> String
monthlyBudget =
    Table.toCSV . CashFlow.toMatrix . sumDebsAndCreds . toDebsAndCreds
  where
    toDebsAndCreds  = tableToDebitsAndCredits . Table.toTable
    sumDebsAndCreds = map CashFlow.toUniqueListWithAmountSum

tableToDebitsAndCredits :: Table.Table -> [[CashFlow.CashFlow]]
tableToDebitsAndCredits table@(_ : rows) = foldl
    (\debsAndCreds row -> if (isTransfer row)
        then debsAndCreds
        else CashFlow.addToDebsAndCreds (description row, amount row)
                                        debsAndCreds
    )
    [[], []]
    rows
  where
    amount      = getAmount table
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
getTransactionType table row =
    case (Table.getCell table "Transaction Type" row) of
        Nothing -> "No Transaction Type"
        Just tt -> tt
