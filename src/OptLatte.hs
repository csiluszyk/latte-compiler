module OptLatte where

import Control.Monad.State
import qualified Data.Map as M

import LlvmLatte
import UtilsLatte


optimize :: LlvmProg -> LlvmProg
optimize (LlvmProg s e defines) = LlvmProg s e optimizedDefs
  where
    optimizedDefs = map optimizeDef defines
    optimizeDef (LlvmDef t l vs insts) = LlvmDef t l vs (optimizeInsts insts)

type Consts = M.Map Loc Value
type OptM a = State Consts a

optimizeInsts :: [LlvmInst] -> [LlvmInst]
optimizeInsts insts = propagatedConsts
  where (propagatedConsts, _) = runState (propagateConstsInsts insts) M.empty

propagateConstsInsts :: [LlvmInst] -> OptM [LlvmInst]
propagateConstsInsts = mapM propagateConstsInst

propagateConstsInst :: LlvmInst -> OptM LlvmInst
propagateConstsInst (RetInst val) = do
  newVal <- computeVal val
  return $ RetInst newVal
propagateConstsInst (Br val lab1 lab2) = do
  newVal <- computeVal val
  return $ Br newVal lab1 lab2
propagateConstsInst (Call loc t n vals) = do
  newVals <- mapM computeVal vals
  return $ Call loc t n newVals
propagateConstsInst (Mul loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  consts <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 * i2)) consts
        _ -> consts
  put updatedConsts
  return $ Mul loc newVal1 newVal2
propagateConstsInst (SDiv loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  consts <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 `div` i2)) consts
        _ -> consts
  put updatedConsts
  return $ SDiv loc newVal1 newVal2
propagateConstsInst (SRem loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  consts <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 `rem` i2)) consts
        _ -> consts
  put updatedConsts
  return $ SRem loc newVal1 newVal2
propagateConstsInst (Add loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  consts <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 + i2)) consts
        (BoolLit b1, BoolLit b2) -> M.insert loc (BoolLit (b1 `xor` b2)) consts
        _ -> consts
  put updatedConsts
  return $ Add loc newVal1 newVal2
propagateConstsInst (Sub loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  consts <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 - i2)) consts
        (BoolLit b1, BoolLit b2) ->
          M.insert loc (BoolLit (not (b1 && b2))) consts
        _ -> consts
  put updatedConsts
  return $ Sub loc newVal1 newVal2
propagateConstsInst (Icmp loc op val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  consts <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) ->
          M.insert loc (BoolLit (getCmpIntFun op i1 i2)) consts
        (BoolLit b1, BoolLit b2) ->
          M.insert loc (BoolLit (getCmpBoolFun op b1 b2)) consts
        _ -> consts
  put updatedConsts
  return $ Icmp loc op newVal1 newVal2
propagateConstsInst (Phi loc t valLabs) = do
  let (vals, labs) = unzip valLabs
  newVals <- mapM computeVal vals
  return $ Phi loc t (zip newVals labs)
propagateConstsInst inst = return inst

xor :: Bool -> Bool -> Bool
xor a b = not (a && b) && (a || b)

getCmpIntFun :: LlvmRelOp -> Int -> Int -> Bool
getCmpIntFun Lth = (<)
getCmpIntFun Le = (<=)
getCmpIntFun Gth = (>)
getCmpIntFun Ge = (>=)
getCmpIntFun Equ = (==)
getCmpIntFun Ne = (/=)

getCmpBoolFun :: LlvmRelOp -> Bool -> Bool -> Bool
getCmpBoolFun Equ = (==)
getCmpBoolFun Ne = (/=)

computeVal :: Value -> OptM Value
computeVal r@(Reg loc t) = do
  consts <- get
  case M.lookup loc consts of
    Just val -> return val
    Nothing -> return r
computeVal val = return val
