module Util where

import Debug.Trace as Debug

logWarn :: a -> a
logWarn = Debug.trace "WARNING FOR: "