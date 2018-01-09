module SsaLatte where

import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import LlvmLatte

type SymTab = M.Map (Loc, Label) Loc
type Cfg = M.Map Label [Label]

type SsaM a = StateT (SymTab, Label) (Reader Cfg) a

-- Converts LlvmProg to SSA form
-- [Braun, Buchwald, Hack, Leißa, Mallon, Zwinkau 2013]
toSsa :: LlvmProg -> LlvmProg
toSsa (LlvmProg s e defines) = LlvmProg s e newDefines
  where
    newDefines = map runSsaDef defines
    runSsaDef (LlvmDef t l vs insts) = LlvmDef t l vs globalSsa
      where
        globalSsa = fst $
          runReader (runStateT (toSsaGlobalInsts localSsa) (symTab, "")) cfg
        (localSsa, (symTab, _)) =
          runReader (runStateT (toSsaLocalInsts insts) (M.empty, "")) cfg
        cfg = generateCfg insts

generateCfg :: [LlvmInst] -> Cfg
generateCfg insts = snd $ foldl foldInst ("", M.empty) insts
  where
    foldInst (currLab, cfg) (Br _ lab1 lab2) = (currLab, newCfg)
      where newCfg = updateCfg cfg lab1 currLab
            newerCfg = updateCfg newCfg lab2 currLab
    foldInst (currLab, cfg) (Goto lab) = (currLab, updateCfg cfg lab currLab)
    foldInst (currLab, cfg) (Lab lab) = (lab, cfg)
    foldInst (currLab, cfg) _ = (currLab, cfg)
    updateCfg cfg toLab fromLab = M.insert toLab bList cfg
      where bList = nub $ fromLab : M.findWithDefault [] toLab cfg

toSsaLocalInsts :: [LlvmInst] -> SsaM [LlvmInst]
toSsaLocalInsts = concatMapM toSsaLocalInst

toSsaLocalInst :: LlvmInst -> SsaM [LlvmInst]
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

updateValLocal :: Value -> SsaM Value
updateValLocal (Reg loc t) = do
  upLoc <- updateLocLocal loc
  return (Reg upLoc t)
updateValLocal val = return val

updateLocLocal :: Loc -> SsaM Loc
updateLocLocal loc = do
  (symTab, lab) <- get
  case M.lookup (loc, lab) symTab of
    Just newLoc
      | newLoc == loc -> return loc
      | otherwise -> updateLocLocal newLoc
    Nothing -> return loc


toSsaGlobalInsts :: [LlvmInst] -> SsaM [LlvmInst]
toSsaGlobalInsts = concatMapM toSsaGlobalInst

toSsaGlobalInst :: LlvmInst -> SsaM [LlvmInst]
toSsaGlobalInst inst@(Lab lab) = do
  (symTab, _) <- get
  put (symTab, lab)
  return [inst]

toSsaGlobalInst (RetInst val) = do
  (phi, upVal) <- updateValGlobal val
  return $ phi ++ [RetInst upVal]
toSsaGlobalInst (Br val l1 l2) = do
  (phi, upVal) <- updateValGlobal val
  return $ phi ++ [Br upVal l1 l2]
toSsaGlobalInst (Call l t s vals) = do
  (phis, upVals) <- mapAndUnzipM updateValGlobal vals
  return $ concat phis ++ [Call l t s upVals]
toSsaGlobalInst (Mul l val1 val2) = do
  (phi1, upVal1) <- updateValGlobal val1
  (phi2, upVal2) <- updateValGlobal val2
  return $ phi1 ++ phi2 ++ [Mul l upVal1 upVal2]
toSsaGlobalInst (SDiv l val1 val2) = do
  (phi1, upVal1) <- updateValGlobal val1
  (phi2, upVal2) <- updateValGlobal val2
  return $ phi1 ++ phi2 ++ [SDiv l upVal1 upVal2]
toSsaGlobalInst (SRem l val1 val2) = do
  (phi1, upVal1) <- updateValGlobal val1
  (phi2, upVal2) <- updateValGlobal val2
  return $ phi1 ++ phi2 ++ [SRem l upVal1 upVal2]
toSsaGlobalInst (Add l val1 val2) = do
  (phi1, upVal1) <- updateValGlobal val1
  (phi2, upVal2) <- updateValGlobal val2
  return $ phi1 ++ phi2 ++ [Add l upVal1 upVal2]
toSsaGlobalInst (Sub l val1 val2) = do
  (phi1, upVal1) <- updateValGlobal val1
  (phi2, upVal2) <- updateValGlobal val2
  return $ phi1 ++ phi2 ++ [Sub l upVal1 upVal2]
toSsaGlobalInst (Icmp l o valT valF) = do
  (phiT, upValT) <- updateValGlobal valT
  (phiF, upValF) <- updateValGlobal valF
  return $ phiT ++ phiF ++ [Icmp l o upValT upValF]

toSsaGlobalInst inst = return [inst]

updateValGlobal :: Value -> SsaM ([LlvmInst], Value)
updateValGlobal (Reg loc t) = do
  (phi, upLoc) <- updateLocGlobal loc
  return (phi, Reg upLoc t)
updateValGlobal val = return ([], val)

updateLocGlobal :: Loc -> SsaM ([LlvmInst], Loc)
updateLocGlobal loc = do
  (symTab, lab) <- get
  return ([], loc)
