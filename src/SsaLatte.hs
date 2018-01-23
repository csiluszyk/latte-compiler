module SsaLatte where

import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import qualified Data.Map as M

import LlvmLatte
import UtilsLatte


type SymTab = M.Map (Loc, Label) Loc
type Cfg = M.Map Label [Label]

type SsaLocalM a = State (SymTab, Label) a
type SsaGlobalM a =
  WriterT [(Label, LlvmInst)] (StateT (SymTab, Label, Int) (Reader Cfg)) a
type LabM = M.Map Label Label


-- Converts LlvmProg to SSA form
-- [Braun, Buchwald, Hack, Leißa, Mallon, Zwinkau 2013]
toSsa :: LlvmProg -> LlvmProg
toSsa (LlvmProg s e defines) = LlvmProg s e newDefines
  where
    newDefines = map runSsaDef defines
    runSsaDef (LlvmDef t l vs insts) = LlvmDef t l vs ssa
      where
        ssa = insertPhis phis globalSsa
        ((globalSsa, phis), (globalSymTab, _, _)) = runReader (runStateT
          (runWriterT (toSsaGlobalInsts localSsa)) (localSymTab, "", 0)) cfg
        (localSsa, (localSymTab, _)) =
          runState (toSsaLocalInsts insts) (M.empty, "")
        cfg = generateCfg insts


insertPhis :: [(Label, LlvmInst)] -> [LlvmInst] -> [LlvmInst]
insertPhis phis = _insertPhis phisM
  where phisM = M.fromListWith (++) $ map (\(l, p) -> (l, [p])) phis
_insertPhis phisM [] = []
_insertPhis phisM (l@(Lab lab) : insts) =
  l : phis ++ _insertPhis phisM insts
  where phis = M.findWithDefault [] lab phisM
_insertPhis phisM (i : insts) = i : _insertPhis phisM insts


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
toSsaLocalInsts = mapM toSsaLocalInst

toSsaLocalInst :: LlvmInst -> SsaLocalM LlvmInst
toSsaLocalInst inst@(Lab lab) = do
  (symTab, _) <- get
  put (symTab, lab)
  return inst
toSsaLocalInst (AssInst t lLoc rLoc) = do
  (symTab, lab) <- get
  let newSymTab
        | lLoc == rLoc = symTab
        | otherwise = M.insert (lLoc, lab) rLoc symTab
  put (newSymTab, lab)
  upVal <- updateValLocal (Reg rLoc t)
  let upLoc = case upVal of
                Reg upLoc _ -> upLoc  -- only this case is posssible
  return $ AssInst t lLoc upLoc
toSsaLocalInst (RetInst val) = do
  upVal <- updateValLocal val
  return $ RetInst upVal
toSsaLocalInst (Br val l1 l2) = do
  upVal <- updateValLocal val
  return $ Br upVal l1 l2
toSsaLocalInst (Call l t s vals) = do
  upVals <- mapM updateValLocal vals
  return $ Call l t s upVals
toSsaLocalInst (Mul l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return $ Mul l upVal1 upVal2
toSsaLocalInst (SDiv l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return $ SDiv l upVal1 upVal2
toSsaLocalInst (SRem l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return $ SRem l upVal1 upVal2
toSsaLocalInst (Add l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return $ Add l upVal1 upVal2
toSsaLocalInst (Sub l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  return $ Sub l upVal1 upVal2
toSsaLocalInst (Icmp l o valT valF) = do
  upValT <- updateValLocal valT
  upValF <- updateValLocal valF
  return $ Icmp l o upValT upValF
toSsaLocalInst inst = return inst

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
toSsaGlobalInsts = concatMapM toSsaGlobalInst

toSsaGlobalInst :: LlvmInst -> SsaGlobalM [LlvmInst]
toSsaGlobalInst inst@(Lab lab) = do
  (symTab, _, n) <- get
  put (symTab, lab, n)
  return [inst]
toSsaGlobalInst (AssInst t _ rLoc) = do
  updateValGlobal (Reg rLoc t)
  return []
toSsaGlobalInst (RetInst val) = do
  upVal <- updateValGlobal val
  return [RetInst upVal]
toSsaGlobalInst (Br val l1 l2) = do
  upVal <- updateValGlobal val
  return [Br upVal l1 l2]
toSsaGlobalInst (Call l t s vals) = do
  upVals <- mapM updateValGlobal vals
  return [Call l t s upVals]
toSsaGlobalInst (Mul l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return [Mul l upVal1 upVal2]
toSsaGlobalInst (SDiv l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return [SDiv l upVal1 upVal2]
toSsaGlobalInst (SRem l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return [SRem l upVal1 upVal2]
toSsaGlobalInst (Add l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return [Add l upVal1 upVal2]
toSsaGlobalInst (Sub l val1 val2) = do
  upVal1 <- updateValGlobal val1
  upVal2 <- updateValGlobal val2
  return [Sub l upVal1 upVal2]
toSsaGlobalInst (Icmp l o valT valF) = do
  upValT <- updateValGlobal valT
  upValF <- updateValGlobal valF
  return [Icmp l o upValT upValF]

toSsaGlobalInst inst = return [inst]

updateValGlobal :: Value -> SsaGlobalM Value
updateValGlobal (Reg loc t)
  | head (tail loc) == 't' = do  -- tmp arg to replace
      cfg <- ask
      (symTab, lab, n) <- get
      let preds = M.findWithDefault [] lab cfg
      (_, newLoc) <- getGlobalLoc lab loc t preds
      return $ Reg newLoc t
  | otherwise = return $ Reg loc t
updateValGlobal val = return val

getGlobalLoc :: Label -> Loc -> LlvmType -> [Label] -> SsaGlobalM (Label, Loc)
getGlobalLoc currLab loc t [] =
  error $ "internal error: empty preds for " ++ currLab ++ " " ++ loc
getGlobalLoc currLab loc t [pred] = do
  -- Optimize the common case of one predecessor: no phi needed.
  (_, locA) <- _getGlobalLoc pred loc t
  (symTab, lab, n) <- get
  put (M.insert (loc, currLab) locA symTab, lab, n)
  return (currLab, locA)
getGlobalLoc currLab loc t preds = do
  (symTab, lab, n) <- get
  let newLoc = getPhiLoc n
  put (M.insert (loc, currLab) newLoc symTab, lab, n + 1)
  [(labA, locA), (labB, locB)] <- mapM (\p -> _getGlobalLoc p loc t) preds
  (newSymTab, lab, newN) <- get
  if all (== newLoc) [locA, locB] then
    error "internal error: all newLoc"
  else if locA == newLoc then do
    put (M.insert (loc, currLab) locB newSymTab, lab, newN)
    return (currLab, locB)
  else if (locB == newLoc ) || locA == locB then do
    put (M.insert (loc, currLab) locA newSymTab, lab, newN)
    return (currLab, locA)
  else do
    put (M.insert (loc, currLab) newLoc newSymTab, lab, newN)
    tell [(currLab, Phi newLoc t [(Reg locA t, labA), (Reg locB t, labB)])]
    return (currLab, newLoc)

_getGlobalLoc :: Label -> Loc -> LlvmType -> SsaGlobalM (Label, Loc)
_getGlobalLoc currLab loc t = do
  (symTab, lab, n) <- get
  case M.lookup (loc, currLab) symTab of
    Just newLoc -> return (currLab, getFinalLoc newLoc currLab symTab)
    Nothing -> do
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

getPhiLoc :: Int -> Loc
getPhiLoc = _getLoc 'p'
