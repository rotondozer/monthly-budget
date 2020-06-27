module CashFlow
    ( Amount
    , Description
    , CashFlow
    , CashFlowMap
    , addAmounts
    , addToDebsAndCreds
    , fromMapsToMatrix
    , mapTotal
    , readAmount
    , toMapWithAmountSum
    )
where

-- Ideally, Amount would be converted to a Float once and stored that way in the map until
-- it can be converted back to a String. But that creates complications where `fromListWith`
-- needs to handle when it encounters duplicate keys, and at that point, I/Haskell aren't sure which
-- types are being compared, so it's easier for now to keep them as strings and read the value on demand.

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.List                      ( sortBy )

type Amount = String -- tobe Float
type Description = String
type CashFlow = (Description, Amount)

readAmount :: Amount -> Float
readAmount a = read a :: Float

addAmounts :: Amount -> Amount -> Amount
addAmounts a1 a2 = show $ (readAmount a1) + (readAmount a2)

addToDebsAndCreds :: CashFlow -> [[CashFlow]] -> [[CashFlow]]
addToDebsAndCreds cf@(_, amt) (debits : credits : _) = if (readAmount amt) < 0
    then [debits ++ [cf], credits]
    else [debits, credits ++ [cf]]

toList :: CashFlow -> [String]
toList (debits, credits) = [debits, credits]

-- CashFlowMap
-- Converting to a map makes the entries unique, and sum duplicate entries

type CashFlowMap = Map.Map Description Amount

toMapWithAmountSum :: [CashFlow] -> CashFlowMap
toMapWithAmountSum = Map.fromListWith addAmounts

mapTotal :: CashFlowMap -> Amount
mapTotal = Map.foldl addAmounts "0"

fromMapsToMatrix :: [CashFlowMap] -> [[String]]
fromMapsToMatrix [] = []
fromMapsToMatrix (debits : credits : _) =
    let totalCred = mapTotal credits
        totalDeb  = mapTotal debits
    in  (toSortedList debits)
            ++ (toSortedList credits)
            ++ [ ["Total Credits", totalCred]
               , ["Total Debits", totalDeb]
               , ["NET", addAmounts totalCred totalDeb]
               ]
    where toSortedList = (map toList) . sortCashFlows . Map.toList

sortCashFlows :: [CashFlow] -> [CashFlow]
sortCashFlows =
    sortBy (\(_, a) (_, b) -> compare (readAmount a) (readAmount b))

