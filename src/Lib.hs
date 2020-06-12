module Lib
    ( monthlyBudget
    )
where

import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Table

type Description = String
type Amount = String -- tobe Float

type CashFlow = ([(Description, Amount)], [(Description, Amount)])
type CashFlowTotals = Map.Map String String

monthlyBudget :: String -> CashFlowTotals
monthlyBudget csv = fst $ totaledCashFlow (Table.toTable csv)

-- Ideally, amount would be converted to a Float once and stored that way in the map until
-- it can be converted back to a String. But that creates complications where `fromListWith`
-- needs to handle when it encounters duplicate keys, and at that point, I/Haskell aren't sure which
-- types are being compared, so it's easier for now to keep them as strings and read the value on demand.
totaledCashFlow :: Table.Table -> (CashFlowTotals, CashFlowTotals)
totaledCashFlow table@(_ : rows) =
    let (debits, credits) = toCashFlow table
    in  ( Map.fromListWith addAmounts debits
        , Map.fromListWith addAmounts credits
        )
  where
    amountFrom      = getAmount table
    descriptionFrom = getDescription table

toCashFlow :: Table.Table -> CashFlow
toCashFlow table@(_ : rows) = foldl
    (\(debits, credits) row ->
        let amount = amountFrom row
            cf     = (descriptionFrom row, amount)
        in  if (readAmount amount) < 0
                then (debits ++ [cf], credits)
                else (debits, credits ++ [cf])
    )
    ([], [])
    rows
  where
    amountFrom      = getAmount table
    descriptionFrom = getDescription table

toDebitsAndCredits :: CashFlow -> Table.Row -> CashFlow
toDebitsAndCredits (debits, credits) row = (debits, credits)

readAmount :: Amount -> Float
readAmount a = read a :: Float

addAmounts :: Amount -> Amount -> Amount
addAmounts a1 a2 = show $ (readAmount a1) + (readAmount a2)

getDescription :: Table.Table -> Table.Row -> Description
getDescription table row =
    fromMaybe "No Description" (Table.getCell table "\"Description\"" row)

getAmount :: Table.Table -> Table.Row -> Amount
getAmount table row = fromMaybe "0" (Table.getCell table "\"Amount\"" row)
