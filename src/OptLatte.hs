module OptLatte where

import qualified Data.Map as M

import LlvmLatte

type LabM = M.Map Label Label

optimize :: LlvmProg -> LlvmProg
optimize (LlvmProg s e defines) = LlvmProg s e optimizedDefs
  where
    optimizedDefs = map optimizeDef defines
    optimizeDef (LlvmDef t l vs insts) = LlvmDef t l vs (optimizeInsts insts)

optimizeInsts :: [LlvmInst] -> [LlvmInst]
optimizeInsts = removeEmpty

removeEmpty :: [LlvmInst] -> [LlvmInst]
-- todo: add only if first has predecessors
removeEmpty insts = Lab "%entry" : Goto firstLab : nonEmpty
  where
    blocks = splitBlocks insts
    (nonEmptyBlocks, labM) = removeEmptyBlocks blocks
    nonEmpty = removeEmptyInsts (concat nonEmptyBlocks) labM
    firstLab = getLab $ head nonEmpty

getLab :: LlvmInst -> Label
getLab (Lab lab) = lab

-- todo: converge?

removeEmptyBlocks :: [[LlvmInst]] -> ([[LlvmInst]], LabM)
removeEmptyBlocks [] = ([], M.empty)
removeEmptyBlocks ([Lab lEmp] : blocks) =
  (newBlocks, M.insert lEmp emptyLab labM)
  where (newBlocks, labM) = removeEmptyBlocks blocks
removeEmptyBlocks ([Lab lSrc, Goto lDst] : blocks) =
  (newBlocks, M.insert lSrc lDst labM)
  where (newBlocks, labM) = removeEmptyBlocks blocks
removeEmptyBlocks (b : blocks) = (b : newBlocks, labM)
  where (newBlocks, labM) = removeEmptyBlocks blocks

removeEmptyInsts :: [LlvmInst] -> LabM -> [LlvmInst]
removeEmptyInsts [] _ = []
removeEmptyInsts (Goto lab : insts) labM
  | newLab == emptyLab = removeEmptyInsts insts labM
  | otherwise = Goto newLab : removeEmptyInsts insts labM
  where newLab = _getFinalLab lab labM
removeEmptyInsts (Br v lab1 lab2 : insts) labM
  | all (== emptyLab) [newLab1, newLab2] = removeEmptyInsts insts labM
  | newLab1 == emptyLab = Goto newLab2 : removeEmptyInsts insts labM
  | newLab2 == emptyLab = Goto newLab1 : removeEmptyInsts insts labM
  | otherwise = Br v newLab1 newLab2 : removeEmptyInsts insts labM
  where newLab1 = _getFinalLab lab1 labM
        newLab2 = _getFinalLab lab2 labM
removeEmptyInsts (inst : insts) labM = inst : removeEmptyInsts insts labM

_getFinalLab :: Label -> LabM -> Label
_getFinalLab lab labM
  | nextLab == lab = lab
  | otherwise = _getFinalLab nextLab labM
  where nextLab = M.findWithDefault lab lab labM

splitBlocks :: [LlvmInst] -> [[LlvmInst]]
splitBlocks insts = _splitBlocks insts [] []

_splitBlocks :: [LlvmInst] -> [LlvmInst] -> [[LlvmInst]] -> [[LlvmInst]]
_splitBlocks [] currBlock acc = reverse (reverse currBlock : acc)
_splitBlocks (Lab l : insts) currBlock acc
  | null currBlock = _splitBlocks insts [Lab l] acc
  | otherwise = _splitBlocks insts [Lab l] (reverse currBlock : acc)
_splitBlocks (inst : insts) currBlock acc =
  _splitBlocks insts (inst : currBlock) acc
