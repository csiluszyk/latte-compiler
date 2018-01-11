module SsaLatte where

import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import qualified Data.Map as M

import LlvmLatte


type SymTab = M.Map (Loc, Label) Loc
type Cfg = M.Map Label [Label]

type SsaLocalM a = State (SymTab, Label) a
type SsaGlobalM a =
  WriterT [(Label, LlvmInst)] (StateT (SymTab, Label) (Reader Cfg)) a
type LabM = M.Map Label Label


-- Converts LlvmProg to SSA form
-- [Braun, Buchwald, Hack, Leißa, Mallon, Zwinkau 2013]
toSsa :: LlvmProg -> LlvmProg
toSsa (LlvmProg s e defines) = LlvmProg s e newDefines
  where
    newDefines = map runSsaDef defines
    runSsaDef (LlvmDef t l vs insts) = LlvmDef t l vs ssa
      where
        ssa = removeEmpty (insertPhis (sort phis) globalSsa) globalSymTab cfg
        ((globalSsa, phis), (globalSymTab, _)) = runReader (runStateT
          (runWriterT (toSsaGlobalInsts localSsa)) (localSymTab, "")) cfg
        (localSsa, (localSymTab, _)) =
          runState (toSsaLocalInsts insts) (M.empty, "")
        cfg = generateCfg insts


insertPhis :: [(Label, LlvmInst)] -> [LlvmInst] -> [LlvmInst]
insertPhis [] insts = insts
insertPhis phis@((phiLab, phi) : rest) (l@(Lab lab) : insts)
  | phiLab == lab = insertPhis rest (l : phi : insts)
  | otherwise = l : insertPhis phis insts
insertPhis phis (i : insts) = i : insertPhis phis insts
insertPhis p i = error $ show p ++ " " ++ show i --todo


removeEmpty :: [LlvmInst] -> SymTab -> Cfg -> [LlvmInst]
-- todo: add only if first has predecessors
removeEmpty insts symTab cfg = Lab "%entry" : Goto firstLab : nonEmpty
  where
    blocks = splitBlocks insts
    (nonEmptyBlocks, labM) = removeEmptyBlocks blocks
    nonEmpty = removeEmptyInsts (concat nonEmptyBlocks) labM symTab cfg ""
    firstLab = getLab $ head nonEmpty

getLab :: LlvmInst -> Label
getLab (Lab lab) = lab

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

removeEmptyInsts :: [LlvmInst] -> LabM -> SymTab -> Cfg -> Label -> [LlvmInst]
removeEmptyInsts [] _ _ _ _ = []
removeEmptyInsts (Goto lab : insts) labM symTab cfg l
  | newLab == emptyLab = removeEmptyInsts insts labM symTab cfg l
  | otherwise = Goto newLab : removeEmptyInsts insts labM symTab cfg l
  where newLab = _getFinalLab lab labM
removeEmptyInsts (Br v lab1 lab2 : insts) labM symTab cfg l
  | all (== emptyLab) [nLab1, nLab2] = removeEmptyInsts insts labM symTab cfg l
  | nLab1 == emptyLab = Goto nLab2 : removeEmptyInsts insts labM symTab cfg l
  | nLab2 == emptyLab = Goto nLab1 : removeEmptyInsts insts labM symTab cfg l
  | otherwise = Br v nLab1 nLab2 : removeEmptyInsts insts labM symTab cfg l
  where nLab1 = _getFinalLab lab1 labM
        nLab2 = _getFinalLab lab2 labM
removeEmptyInsts (Phi loc t loc1 _ loc2 _ : insts) labM symTab cfg l =
  -- todo: find predecessor in which loc is in scope
  Phi loc t loc1 lab1 loc2 lab2 : removeEmptyInsts insts labM symTab cfg l
  where
    preds = M.findWithDefault [] l cfg
    pred1 = head preds
    pred2 = last preds
    (lab1, lab2) =
      case (M.lookup (loc1, pred1) symTab, M.lookup (loc1, pred2) symTab,
            M.lookup (loc2, pred1) symTab, M.lookup (loc2, pred2) symTab) of
        (Just _, Nothing, Nothing, Just _) -> (pred1, pred2)
        (Nothing, Just _, Just _, Nothing) -> (pred2, pred1)
        (Just _, Nothing, Just _, Just _) -> (pred1, pred2)
        (Just _, Just _, Just _, Nothing) -> (pred2, pred1)
        (_, _, _, _) -> error "internal error: couldn't find new phi labels"
removeEmptyInsts (Lab lab: insts) labM symTab cfg _ =
  Lab lab : removeEmptyInsts insts labM symTab cfg lab
removeEmptyInsts (inst : insts) labM symTab cfg l =
  inst : removeEmptyInsts insts labM symTab cfg l

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


generateCfg :: [LlvmInst] -> Cfg
generateCfg insts = snd $ foldl foldInst ("", M.empty) insts
  where
    foldInst (currLab, cfg) (Br _ lab1 lab2) = (currLab, newerCfg)
      where newCfg = updateCfg cfg lab1 currLab
            newerCfg = updateCfg newCfg lab2 currLab
    foldInst (currLab, cfg) (Goto lab) = (currLab, updateCfg cfg lab currLab)
    foldInst (currLab, cfg) (Lab lab) = (lab, cfg)
    foldInst (currLab, cfg) _ = (currLab, cfg)
    updateCfg cfg toLab fromLab = M.insert toLab bList cfg
      where bList = nub $ fromLab : M.findWithDefault [] toLab cfg


toSsaLocalInsts :: [LlvmInst] -> SsaLocalM [LlvmInst]
toSsaLocalInsts = concatMapM toSsaLocalInst

toSsaLocalInst :: LlvmInst -> SsaLocalM [LlvmInst]
toSsaLocalInst (AssInst _ lLoc rLoc) = do
  (symTab, lab) <- get
  let newSymTab
        | lLoc == rLoc = symTab
        | otherwise = M.insert (lLoc, lab) rLoc symTab
  put (newSymTab, lab)
  return []
toSsaLocalInst inst@(Lab lab) = do
  (symTab, _) <- get
  put (symTab, lab)
  return [inst]
toSsaLocalInst (RetInst val) = do
  upVal <- updateValLocal val
  return [RetInst upVal]
toSsaLocalInst (Br val l1 l2) = do
  upVal <- updateValLocal val
  return [Br upVal l1 l2]
toSsaLocalInst (Call l t s vals) = do
  upVals <- mapM updateValLocal vals
  return [Call l t s upVals]
toSsaLocalInst (Mul l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return [Mul l upVal1 upVal2]
toSsaLocalInst (SDiv l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return [SDiv l upVal1 upVal2]
toSsaLocalInst (SRem l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return [SRem l upVal1 upVal2]
toSsaLocalInst (Add l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return [Add l upVal1 upVal2]
toSsaLocalInst (Sub l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return [Sub l upVal1 upVal2]
toSsaLocalInst (Icmp l o valT valF) = do
  upValT <- updateValLocal valT
  upValF <- updateValLocal valF
  return [Icmp l o upValT upValF]
toSsaLocalInst inst = return [inst]

updateValLocal :: Value -> SsaLocalM Value
updateValLocal (Reg loc t)
  | head (tail loc) == 't' = do  -- tmp arg to replace
      upLoc <- updateLocLocal loc
      return (Reg upLoc t)
  | otherwise = return (Reg loc t)
updateValLocal val = return val

updateLocLocal :: Loc -> SsaLocalM Loc
updateLocLocal loc = do
  (symTab, lab) <- get
  let newLoc = getFinalLoc loc lab symTab
  return newLoc


toSsaGlobalInsts :: [LlvmInst] -> SsaGlobalM [LlvmInst]
toSsaGlobalInsts = mapM toSsaGlobalInst

toSsaGlobalInst :: LlvmInst -> SsaGlobalM LlvmInst
toSsaGlobalInst inst@(Lab lab) = do
  (symTab, _) <- get
  put (symTab, lab)
  return inst

toSsaGlobalInst (RetInst val) = do
  upVal <- updateValGlobal val
  return $ RetInst upVal
toSsaGlobalInst (Br val l1 l2) = do
  upVal <- updateValGlobal val
  return $ Br upVal l1 l2
toSsaGlobalInst (Call l t s vals) = do
  upVals <- mapM updateValGlobal vals
  return $ Call l t s upVals
toSsaGlobalInst (Mul l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return $ Mul l upVal1 upVal2
toSsaGlobalInst (SDiv l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return $ SDiv l upVal1 upVal2
toSsaGlobalInst (SRem l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return $ SRem l upVal1 upVal2
toSsaGlobalInst (Add l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return $ Add l upVal1 upVal2
toSsaGlobalInst (Sub l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return $ Sub l upVal1 upVal2
toSsaGlobalInst (Icmp l o valT valF) = do
  upValT <- updateValGlobal valT
  upValF <- updateValGlobal valF
  return $ Icmp l o upValT upValF

toSsaGlobalInst inst = return inst

updateValGlobal :: Value -> SsaGlobalM Value
updateValGlobal (Reg loc t)
  | head (tail loc) == 't' = do  -- tmp arg to replace
      cfg <- ask
      (symTab, lab) <- get
      let preds = M.findWithDefault [] lab cfg
      (_, newLoc) <- getGlobalLoc lab loc t preds
      return $ Reg newLoc t
  | otherwise = return $ Reg loc t
updateValGlobal val = return val

getGlobalLoc :: Label -> Loc -> LlvmType -> [Label] -> SsaGlobalM (Label, Loc)
getGlobalLoc currLab loc t preds = do
  (symTab, lab) <- get
  predsLocs <- mapM (\p -> _getGlobalLoc p loc t) preds
  case predsLocs of
    [] -> error "internal error: empty preds or loc not found"
    [(_, newLoc)] -> do
      put (M.insert (loc, currLab) newLoc symTab, lab)
      return (currLab, newLoc)
    [(labA, locA), (labB, locB)]
      | all (== emptyLoc) [locA, locB] -> error "internal error: all empty"
      | locA == emptyLoc -> return (currLab, locB)
      | locB == emptyLoc -> return (currLab, locA)
      | locA == locB -> return (currLab, locA)
      | otherwise -> do
        let newLoc = locA ++ "_" ++ tail locB
        put (M.insert (loc, currLab) newLoc symTab, lab)
        tell [(currLab, Phi newLoc t locA labA locB labB)]
        return (currLab, newLoc)
    _ -> error "internal error: to many preds found"

_getGlobalLoc :: Label -> Loc -> LlvmType -> SsaGlobalM (Label, Loc)
_getGlobalLoc currLab loc t = do
  (symTab, lab) <- get
  case M.lookup (loc, currLab) symTab of
    Just newLoc -> return (currLab, getFinalLoc newLoc currLab symTab)
    Nothing -> do
      put (M.insert (loc, currLab) emptyLoc symTab, lab)
      cfg <- ask
      let preds = M.findWithDefault [] currLab cfg
      getGlobalLoc currLab loc t preds

getFinalLoc :: Loc -> Label -> SymTab -> Loc
getFinalLoc loc lab symTab =
  case M.lookup (loc, lab) symTab of
    Just newLoc
      | newLoc == loc -> loc
      | otherwise -> getFinalLoc newLoc lab symTab
    Nothing -> loc
