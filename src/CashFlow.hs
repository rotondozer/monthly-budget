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
    , totalsFromDebsAndCreds
    )
where

-- Ideally, Amount would be converted to a Float once and stored that way in the map until
-- it can be converted back to a String. But that creates complications where `fromListWith`
-- needs to handle when it encounters duplicate keys, and at that point, I/Haskell aren't sure which
-- types are being compared, so it's easier for now to keep them as strings and read the value on demand.

import qualified Data.Map                      as Map

type Amount = String -- tobe Float
type Description = String
type CashFlow = (Description, Amount)

type CashFlowMap = Map.Map Description Amount

readAmount :: Amount -> Float
readAmount a = read a :: Float

addAmounts :: Amount -> Amount -> Amount
addAmounts a1 a2 = show $ (readAmount a1) + (readAmount a2)

addToDebsAndCreds :: CashFlow -> [[CashFlow]] -> [[CashFlow]]
addToDebsAndCreds cf@(_, amt) (debits : credits : _) = if (readAmount amt) < 0
    then [debits ++ [cf], credits]
    else [debits, credits ++ [cf]]

-- CashFlowMap
-- Converting to a map makes the entries unique, and sum duplicate entries

toMapWithAmountSum :: [CashFlow] -> CashFlowMap
toMapWithAmountSum = Map.fromListWith addAmounts

mapTotal :: CashFlowMap -> Amount
mapTotal = Map.foldl addAmounts "0"

totalsFromDebsAndCreds :: [CashFlowMap] -> [CashFlowMap]
totalsFromDebsAndCreds (debits : credits : _) =
    let totalDebit  = CashFlow.mapTotal debits
        totalCredit = CashFlow.mapTotal credits
        totals      = Map.insert
            "Total Credits"
            totalCredit
            (Map.insert "Total Debits" totalDebit Map.empty)
    in  [debits, credits, totals]

toList :: CashFlow -> [String]
toList (debits, credits) = [debits, credits]

fromMapsToMatrix :: [CashFlowMap] -> [[String]]
fromMapsToMatrix [] = []
fromMapsToMatrix (mapA : maps) =
    (map toList (Map.toList mapA)) ++ (fromMapsToMatrix maps)
