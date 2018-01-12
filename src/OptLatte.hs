module OptLatte where

import qualified Data.Map as M

import LlvmLatte


optimize :: LlvmProg -> LlvmProg
optimize (LlvmProg s e defines) = LlvmProg s e optimizedDefs
  where
    optimizedDefs = map optimizeDef defines
    optimizeDef (LlvmDef t l vs insts) = LlvmDef t l vs (optimizeInsts insts)

optimizeInsts :: [LlvmInst] -> [LlvmInst]
optimizeInsts x = x
