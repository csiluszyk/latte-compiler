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

-- Converts LlvmProg to SSA form
-- [Braun, Buchwald, Hack, Leißa, Mallon, Zwinkau 2013]
toSsa :: LlvmProg -> LlvmProg
toSsa (LlvmProg s e defines) = LlvmProg s e newDefines
  where
    newDefines = map runSsaDef defines
    runSsaDef (LlvmDef t l vs insts) = LlvmDef t l vs ssa
      where
        ssa = insertPhis (sort phis) globalSsa
        (globalSsa, phis) = fst $ runReader (runStateT (runWriterT (
          toSsaGlobalInsts localSsa)) (symTab, "")) cfg
        (localSsa, (symTab, _)) =
          runState (toSsaLocalInsts insts) (M.empty, "")
        cfg = generateCfg insts

insertPhis :: [(Label, LlvmInst)] -> [LlvmInst] -> [LlvmInst]
insertPhis [] insts = insts
insertPhis phis@((phiLab, phi) : rest) (l@(Lab lab) : insts)
  | phiLab == lab = insertPhis rest (l : phi : insts)
  | otherwise = l : insertPhis phis insts
insertPhis phis (i : insts) = i : insertPhis phis insts

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
  (symTab, lab) <- get
  put (M.insert (l, lab) l symTab, lab)
  return [Call l t s upVals]
toSsaLocalInst strLit@(StrLit l s sL) = do
  (symTab, lab) <- get
  put (M.insert (l, lab) l symTab, lab)
  return [strLit]
toSsaLocalInst (Mul l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  (symTab, lab) <- get
  put (M.insert (l, lab) l symTab, lab)
  return [Mul l upVal1 upVal2]
toSsaLocalInst (SDiv l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  (symTab, lab) <- get
  put (M.insert (l, lab) l symTab, lab)
  return [SDiv l upVal1 upVal2]
toSsaLocalInst (SRem l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  (symTab, lab) <- get
  put (M.insert (l, lab) l symTab, lab)
  return [SRem l upVal1 upVal2]
toSsaLocalInst (Add l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  (symTab, lab) <- get
  put (M.insert (l, lab) l symTab, lab)
  return [Add l upVal1 upVal2]
toSsaLocalInst (Sub l val1 val2) = do
  upVal1 <- updateValLocal val1
  upVal2 <- updateValLocal val2
  (symTab, lab) <- get
  put (M.insert (l, lab) l symTab, lab)
  return [Sub l upVal1 upVal2]
toSsaLocalInst (Icmp l o valT valF) = do
  upValT <- updateValLocal valT
  upValF <- updateValLocal valF
  (symTab, lab) <- get
  put (M.insert (l, lab) l symTab, lab)
  return [Icmp l o upValT upValF]
toSsaLocalInst inst = return [inst]

updateValLocal :: Value -> SsaLocalM Value
updateValLocal (Reg loc t)
  | head (tail loc) /= 'a' = do  -- not local arg
      upLoc <- updateLocLocal loc
      return (Reg upLoc t)
  | otherwise = return (Reg loc t)
updateValLocal val = return val

updateLocLocal :: Loc -> SsaLocalM Loc
updateLocLocal loc = do
  (symTab, lab) <- get
  case M.lookup (loc, lab) symTab of
    Just newLoc
      | newLoc == loc -> return loc
      | otherwise -> updateLocLocal newLoc
    Nothing -> return loc


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
  | head (tail loc) /= 'a' = do  -- not local arg
      (symTab, lab) <- get
      (_, newLoc) <- getGlobalLoc lab loc t
      return $ Reg newLoc t
  | otherwise = return $ Reg loc t
updateValGlobal val = return val

getGlobalLoc :: Label -> Loc -> LlvmType -> SsaGlobalM (Label, Loc)
getGlobalLoc currLab loc t = do
  cfg <- ask
  (symTab, lab) <- get
  case M.lookup (loc, currLab) symTab of
    Just newLoc -> return (currLab, newLoc)
    Nothing -> do
      put (M.insert (loc, currLab) emptyLoc symTab, lab)
      preds <- mapM getFromPreds (M.findWithDefault [] currLab cfg)
      case preds of
        [] -> error "internal error: empty preds"
        [(_, newLoc)] -> do
          when (newLoc /= emptyLoc) $
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
      where getFromPreds pLab = getGlobalLoc pLab loc t
