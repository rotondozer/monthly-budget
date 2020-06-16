module CashFlow
    ( Amount
    , Description
    , DebitsAndCredits
    , CashFlow
    , CashFlowTotals
    , addAmounts
    , addToDebsAndCreds
    , map'
    , readAmount
    , toList
    )
where

import qualified Data.Map                      as Map

-- Ideally, amount would be converted to a Float once and stored that way in the map until
-- it can be converted back to a String. But that creates complications where `fromListWith`
-- needs to handle when it encounters duplicate keys, and at that point, I/Haskell aren't sure which
-- types are being compared, so it's easier for now to keep them as strings and read the value on demand.
type Amount = String -- tobe Float
type Description = String
type CashFlow = (Description, Amount)

type DebitsAndCredits = ([CashFlow], [CashFlow])

type CashFlowTotals = Map.Map Description Amount

readAmount :: Amount -> Float
readAmount a = read a :: Float

addAmounts :: Amount -> Amount -> Amount
addAmounts a1 a2 = show $ (readAmount a1) + (readAmount a2)

addToDebsAndCreds :: CashFlow -> DebitsAndCredits -> DebitsAndCredits
addToDebsAndCreds cf@(_, amt) (debits, credits) = if (readAmount amt) < 0
    then (debits ++ [cf], credits)
    else (debits, credits ++ [cf])

map' :: (a -> b) -> (a, a) -> (b, b)
map' f (x, y) = (f x, f y)

toList :: (a, a) -> [a]
toList (x, y) = [x, y]
