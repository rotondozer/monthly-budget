module Lib
    ( monthlyBudget
    )
where

import qualified Table

monthlyBudget :: String -> Table.Table
monthlyBudget = Table.toTable
