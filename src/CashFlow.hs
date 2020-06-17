module CashFlow
    ( Amount
    , Description
    , CashFlow
    , CashFlowMap
    , addAmounts
    , addToDebsAndCreds
    , readAmount
    , toMapWithAmountSum
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

-- Converting to a map makes the entries unique, and we sum duplicate entries
toMapWithAmountSum :: [CashFlow] -> CashFlowMap
toMapWithAmountSum = Map.fromListWith addAmounts

fromMap :: CashFlowMap -> [CashFlow]
fromMap = Map.toList
