module OptLatte where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe

import LlvmLatte
import UtilsLatte


type Consts = M.Map Loc Value
type StrLits = M.Map Loc String
type OptM a = State (Consts, StrLits) a
type StrMap = M.Map String Loc

optimize :: LlvmProg -> LlvmProg
optimize (LlvmProg _ e defines) = LlvmProg strConsts e filledStrDefs
  where
    (optimizedDefs, strLitsL) = unzip $ map optimizeDef defines
    optimizeDef (LlvmDef t l vs insts) =
      (LlvmDef t l vs optimizedInsts, strLits)
      where
        (optimizedInsts, strLits) = optimizeInsts insts
    strLits = nub $ "\"\"" : concat strLitsL
    strLitLocs = map (\(s, i) -> (s, "@.str." ++ show i)) $ zip strLits [0 ..]
    strConsts = map (\(s, l) -> StrConst l s) strLitLocs
    strLitMap = M.fromList strLitLocs
    filledStrDefs = map fillStrDef optimizedDefs
    fillStrDef (LlvmDef t l vs insts) = LlvmDef t l vs filledInsts
      where filledInsts = fillInsts insts strLitMap

fillInsts :: [LlvmInst] -> StrMap -> [LlvmInst]
fillInsts [] _ = []
fillInsts (StrLit loc s _ : insts) strMap =
  StrLit loc s (fromJust $ M.lookup s strMap) : fillInsts insts strMap
fillInsts (i : insts) strMap = i : fillInsts insts strMap

optimizeInsts :: [LlvmInst] -> ([LlvmInst], [String])
optimizeInsts insts = (propagatedConsts, M.elems strLits)
  where
    (propagatedConsts, (_, strLits)) =
      runState (propagateConstsInsts insts) (M.empty, M.empty)

propagateConstsInsts :: [LlvmInst] -> OptM [LlvmInst]
propagateConstsInsts = mapM propagateConstsInst

propagateConstsInst :: LlvmInst -> OptM LlvmInst
propagateConstsInst (RetInst val) = do
  newVal <- computeVal val
  return $ RetInst newVal
propagateConstsInst (Br val lab1 lab2) = do
  newVal <- computeVal val
  return $ Br newVal lab1 lab2
propagateConstsInst (Call loc t n@("_concat") vals@[Reg l1 t1, Reg l2 t2]) = do
  (consts, strLits) <- get
  case (M.lookup l1 strLits, M.lookup l2 strLits) of
    (Just str1, Just str2) -> do
      -- Truncate `"` on the ends.
      let str = init str1 ++ tail str2
      put (consts, M.insert loc str strLits)
      return $ StrLit loc str emptyLoc
    (_, _) -> return $ Call loc t n vals
propagateConstsInst (Call loc t n vals) = do
  newVals <- mapM computeVal vals
  return $ Call loc t n newVals
propagateConstsInst (StrLit loc s sLoc) = do
  (consts, strLits) <- get
  put (consts, M.insert loc s strLits)
  return $ StrLit loc s sLoc
propagateConstsInst (Mul loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 * i2)) consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ Mul loc newVal1 newVal2
propagateConstsInst (SDiv loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 `div` i2)) consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ SDiv loc newVal1 newVal2
propagateConstsInst (SRem loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 `rem` i2)) consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ SRem loc newVal1 newVal2
propagateConstsInst (Add loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 + i2)) consts
        (BoolLit b1, BoolLit b2) -> M.insert loc (BoolLit (b1 `xor` b2)) consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ Add loc newVal1 newVal2
propagateConstsInst (Sub loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 - i2)) consts
        (BoolLit b1, BoolLit b2) ->
          M.insert loc (BoolLit (not (b1 && b2))) consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ Sub loc newVal1 newVal2
propagateConstsInst (Icmp loc op val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) ->
          M.insert loc (BoolLit (getCmpIntFun op i1 i2)) consts
        (BoolLit b1, BoolLit b2) ->
          M.insert loc (BoolLit (getCmpBoolFun op b1 b2)) consts
        _ -> consts
  put (updatedConsts, strLits)
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
  (consts, _) <- get
  case M.lookup loc consts of
    Just val -> return val
    Nothing -> return r
computeVal val = return val
