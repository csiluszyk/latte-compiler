module UtilsLatte where

import AbsLatte
import LlvmLatte

type Pos = Maybe (Int, Int)
type TypePos = Type Pos

-- Returns next location.
_getLoc :: Char -> Int -> Loc
_getLoc c n = '%' : c : show (n + 1)


splitBlocks :: [LlvmInst] -> [[LlvmInst]]
splitBlocks insts = _splitBlocks insts [] []

_splitBlocks :: [LlvmInst] -> [LlvmInst] -> [[LlvmInst]] -> [[LlvmInst]]
_splitBlocks [] currBlock acc = reverse (reverse currBlock : acc)
_splitBlocks (Lab l : insts) currBlock acc
  | null currBlock = _splitBlocks insts [Lab l] acc
  | otherwise = _splitBlocks insts [Lab l] (reverse currBlock : acc)
_splitBlocks (inst : insts) currBlock acc =
  _splitBlocks insts (inst : currBlock) acc
