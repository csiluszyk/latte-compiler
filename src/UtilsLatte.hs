module UtilsLatte where

import AbsLatte

type Pos = Maybe (Int, Int)
type TypePos = Type Pos

type Loc = String

emptyLoc :: Loc
emptyLoc = ""

-- Returns next location.
_getLoc :: Char -> Int -> Loc
_getLoc c n = '%' : c : show (n + 1)
