module CashFlow
  ( Amount,
    Description,
    CashFlow,
    addToDebsAndCreds,
    toMatrix,
    readAmount,
    toUniqueListWithAmountSum,
  )
where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Table
import Text.Printf
import Text.Read (readMaybe)

type Amount = Float

type Description = String

type CashFlow = (Description, Amount)

readAmount :: String -> Float
readAmount a = fromMaybe 0 (readMaybe a)

addToDebsAndCreds :: CashFlow -> [[CashFlow]] -> [[CashFlow]]
addToDebsAndCreds cf@(_, amt) (debits : credits : _) =
  if amt < 0 then [debits ++ [cf], credits] else [debits, credits ++ [cf]]

total :: [CashFlow] -> Amount
total = foldl (\tot (_, amt) -> tot + amt) 0

toList :: CashFlow -> [String]
toList (description, amount) = [description, round2Dec amount]

toUniqueListWithAmountSum :: [CashFlow] -> [CashFlow]
toUniqueListWithAmountSum = foldl addAmounts []
  where
    addAmounts :: [CashFlow] -> CashFlow -> [CashFlow]
    addAmounts uniqList cf@(desc, amount) = case (lookup desc uniqList) of
      Nothing -> uniqList ++ [cf]
      Just amt -> (deleteBy isSameDesc cf uniqList) ++ [(desc, amt + amount)]
    isSameDesc :: (CashFlow -> CashFlow -> Bool)
    isSameDesc (desc1, _) (desc2, _) = desc1 == desc2

toMatrix :: [[CashFlow]] -> Table.Table
toMatrix (debits : credits : _) =
  ( Table.addRow ["NET", round2Dec $ total credits + total debits]
      . Table.rowSpacer
      . Table.addRow ["Total Credits", round2Dec (total credits)]
      . Table.addSectionSeparator
      . (++ toTableSection credits)
      . Table.addRow ["------ CREDITS ------", ""]
      . Table.rowSpacer
      . Table.addRow ["Total Debits", round2Dec (total debits)]
      . Table.addSectionSeparator
      . (++ toTableSection debits)
      . Table.addRow ["------ DEBITS ------", ""]
  )
    ( Table.create
        ["Grouped Transactions", "Amount"]
    )
  where
    toTableSection :: [CashFlow] -> Table.Table
    toTableSection = map toList . sortCashFlows

-- Sort the greater absolute value to be higher, so highest expense and highest credit appear first
sortCashFlows :: [CashFlow] -> [CashFlow]
sortCashFlows = sortBy (\(_, a) (_, b) -> compare (abs b) (abs a))

round2Dec :: (PrintfArg a, Floating a) => a -> String
round2Dec = printf "%.2f"
