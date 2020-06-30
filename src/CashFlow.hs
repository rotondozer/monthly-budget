module CashFlow
    ( Amount
    , Description
    , CashFlow
    , addToDebsAndCreds
    , toMatrix
    , readAmount
    , toUniqueListWithAmountSum
    )
where

import           Data.List
import           Data.Maybe                     ( fromMaybe )
import           Text.Read                      ( readMaybe )

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
toList (debits, credits) = [debits, show credits]

toUniqueListWithAmountSum :: [CashFlow] -> [CashFlow]
toUniqueListWithAmountSum = foldl addAmounts []
  where
    addAmounts :: [CashFlow] -> CashFlow -> [CashFlow]
    addAmounts uniqList cf@(desc, amt) = case (lookup desc uniqList) of
        Nothing -> uniqList ++ [cf]
        Just amount ->
            (deleteBy isSameDesc cf uniqList) ++ [(desc, amt + amount)]
    isSameDesc :: (CashFlow -> CashFlow -> Bool)
    isSameDesc (desc1, _) (desc2, _) = desc1 == desc2

toMatrix :: [[CashFlow]] -> [[String]]
toMatrix (debits : credits : _) =
    let totalCred = total credits
        totalDeb  = total debits
    in  (["--- DEBITS ---"] : (toSortedList debits))
            ++ (["", "-----"] : ["--- CREDITS ---"] : (toSortedList credits))
            ++ [ ["", "-----"]
               , ["Total Credits", show totalCred]
               , ["Total Debits", show totalDeb]
               , ["NET", show $ totalCred + totalDeb]
               ]
    where toSortedList = (map toList) . sortCashFlows

-- Sort the greater absolute value to be higher, so highest expense and highest credit appear first
sortCashFlows :: [CashFlow] -> [CashFlow]
sortCashFlows = sortBy (\(_, a) (_, b) -> compare (abs b) (abs a))

