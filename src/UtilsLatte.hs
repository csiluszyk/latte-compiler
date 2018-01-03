module UtilsLatte where

import Data.Maybe

import AbsLatte

showPos :: Maybe (Int, Int) -> String
showPos pos = show $ fromJust pos
