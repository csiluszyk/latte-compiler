module OptLatte where

import Control.Monad.Extra
import Control.Monad.State
import Data.Int
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import LlvmLatte
import UtilsLatte


type Consts = M.Map Loc Value
type StrLits = M.Map Loc String
type OptConstM a = State (Consts, StrLits) a
type StrMap = M.Map String Loc

type Replacements = M.Map Value Value
-- block label: (predecessors, successors)
type CfgEdges = M.Map Label ([Label], [Label])
-- label: basic block
type CfgVertices = M.Map Label [LlvmInst]
type OptDeadM a = State (CfgEdges, CfgVertices, Replacements) a

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
optimizeInsts insts = (optInsts, M.elems strLits)
  where
    -- todo generate strLits at the end
    (propagatedConsts, (_, strLits)) =
      runState (propagateConstsInsts insts) (M.empty, M.empty)
    blocks = splitBlocks propagatedConsts
    labels = foldr (\(Lab l : _) labs -> l : labs) [] blocks
    vertices = generateVertices blocks
    edges = generateEdges propagatedConsts
    (optInsts, _) =
      runState (eliminateDeadCode labels) (edges, vertices, M.empty)


generateVertices :: [[LlvmInst]] -> CfgVertices
generateVertices = foldl insertBlock M.empty
  where insertBlock vertices b@(Lab l : _) = M.insert l b vertices

generateEdges :: [LlvmInst] -> CfgEdges
generateEdges insts = snd $ foldl foldEdges ("", M.empty) insts
  where
    foldEdges (currLab, edges) (Br _ lab1 lab2) = (currLab, newerEdges)
      where newEdges = updateEdges edges lab1 currLab
            newerEdges = updateEdges newEdges lab2 currLab
    foldEdges (currLab, edges) (Goto lab) =
      (currLab, updateEdges edges lab currLab)
    foldEdges (currLab, edges) (Lab lab) = (lab, M.insert lab predSuccs edges)
      where predSuccs = M.findWithDefault ([], []) lab edges
    foldEdges (currLab, edges) _ = (currLab, edges)

    updateEdges edges toLab fromLab = updatedEdges
      where
        (fromPreds, fromSuccs) = M.findWithDefault ([], []) fromLab edges
        (toPreds, toSuccs) = M.findWithDefault ([], []) toLab edges
        updatedFromSuccs = nub $ toLab : fromSuccs
        updatedToPreds = nub $ fromLab : toPreds
        tmpEdges = M.insert fromLab (fromPreds, updatedFromSuccs) edges
        updatedEdges = M.insert toLab (updatedToPreds, toSuccs) tmpEdges

eliminateDeadCode :: [Label] -> OptDeadM [LlvmInst]
eliminateDeadCode labels = do
  (_, vertices, _) <- get
  liveStores <- foldM getLiveStores S.empty labels
  mapM_ (eliminateDeadStores liveStores) labels
  mapM_ eliminateUnreachables labels
  mapM_ eliminateDeadJumps labels
  mapM_ replaceVals labels
  (optEdges, optVertices, _) <- get
  let currLabs = filter (`M.member` optVertices) labels  -- at least one elem
  if optVertices == vertices then do
    let firstLab = head currLabs
        (fromPreds, _) = let (Just x) = M.lookup firstLab optEdges in x
        entryBlock = case fromPreds of
          [] -> []
          _ -> [Lab "%entry", Goto firstLab]
        insts =
          concatMap (\l -> let (Just x) = M.lookup l optVertices in x) currLabs
    return $ entryBlock ++ insts
  else
    eliminateDeadCode currLabs

getLiveStores :: S.Set Loc -> Label -> OptDeadM (S.Set Loc)
getLiveStores lives lab = do
  (_, vertices, _) <- get
  case M.lookup lab vertices of
    Just insts -> do
      let newLives = foldl getLiveStoresInst lives insts
      return newLives
    Nothing -> return lives

getLiveStoresInst :: S.Set Loc -> LlvmInst -> S.Set Loc
getLiveStoresInst lives (RetInst val) = addLoc val lives
getLiveStoresInst lives (Br val _ _) = addLoc val lives
getLiveStoresInst lives (Call _ _ _ vals) = foldl (flip addLoc) lives vals
getLiveStoresInst lives (Mul _ val1 val2) = addLoc val2 (addLoc val1 lives)
getLiveStoresInst lives (SDiv _ val1 val2) = addLoc val2 (addLoc val1 lives)
getLiveStoresInst lives (SRem _ val1 val2) = addLoc val2 (addLoc val1 lives)
getLiveStoresInst lives (Add _ val1 val2) = addLoc val2 (addLoc val1 lives)
getLiveStoresInst lives (Sub _ val1 val2) = addLoc val2 (addLoc val1 lives)
getLiveStoresInst lives (Icmp _ _ valT valF) = addLoc valF (addLoc valT lives)
getLiveStoresInst lives (Phi _ _ valLabs) =
  foldl (\l (v, _) -> addLoc v l) lives valLabs
getLiveStoresInst lives inst = lives

addLoc :: Value -> S.Set Loc -> S.Set Loc
addLoc (Reg loc _) lives = S.insert loc lives
addLoc _ lives = lives

eliminateDeadStores :: S.Set Loc -> Label -> OptDeadM ()
eliminateDeadStores lives lab = do
  (edges, vertices, replacements) <- get
  case M.lookup lab vertices of
    Just insts -> do
      let cleanedInsts = concatMap (eliminateDeadStoresInst lives) insts
      put (edges, M.insert lab cleanedInsts vertices, replacements)
      return ()
    Nothing -> return ()

eliminateDeadStoresInst :: S.Set Loc -> LlvmInst -> [LlvmInst]
eliminateDeadStoresInst lives inst@(StrLit loc _ _)
  | S.member loc lives = [inst]
  | otherwise = []
eliminateDeadStoresInst lives inst@(Mul loc _ _)
  | S.member loc lives = [inst]
  | otherwise = []
eliminateDeadStoresInst lives inst@(SDiv loc _ _)
  | S.member loc lives = [inst]
  | otherwise = []
eliminateDeadStoresInst lives inst@(SRem loc _ _)
  | S.member loc lives = [inst]
  | otherwise = []
eliminateDeadStoresInst lives inst@(Add loc _ _)
  | S.member loc lives = [inst]
  | otherwise = []
eliminateDeadStoresInst lives inst@(Sub loc _ _)
  | S.member loc lives = [inst]
  | otherwise = []
eliminateDeadStoresInst lives inst@(Icmp loc _ _ _)
  | S.member loc lives = [inst]
  | otherwise = []
eliminateDeadStoresInst lives inst@(Phi loc _ _)
  | S.member loc lives = [inst]
  | otherwise = []
eliminateDeadStoresInst _ inst = [inst]

eliminateDeadJumps :: Label -> OptDeadM ()
eliminateDeadJumps lab = do
  (_, vertices, _) <- get
  case M.lookup lab vertices of
    Just insts -> do
      eliminateDeadJump lab insts
      return ()
    Nothing -> return ()

eliminateDeadJump :: Label -> [LlvmInst] -> OptDeadM ()

eliminateDeadJump lab [Lab lSrc, Goto lDst] =
  eliminateDeadJumpGoto lab lDst
eliminateDeadJump lab [Phi {}, Goto lDst] =
  eliminateDeadJumpGoto lab lDst

eliminateDeadJump lab [Lab lSrc, Br val lDstT lDstF] =
  eliminateDeadJumpBr lab val lDstT lDstF
eliminateDeadJump lab [Phi {}, Br val lDstT lDstF] =
  eliminateDeadJumpBr lab val lDstT lDstF

eliminateDeadJump lab [inst@(Goto lDst)]
  | lab /= lDst = do
    (edges, vertices, _) <- get
    let (preds, _) = let (Just x) = M.lookup lab edges in x
        predBlocks = map (\p -> let (Just x) = M.lookup p vertices in x) preds
    when (length preds == 1 && isGoto (head predBlocks)) $ do
      mergeEdges (head preds) lab lDst
      (newEdges, newVertices, replacements) <- get
      let block = let (Just x) = M.lookup lab newVertices in x
          newPredBlock = init (head predBlocks) ++ tail block
          updatedVertices = M.insert (head preds) newPredBlock newVertices
          deletedEdges = M.delete lab newEdges
          deletedVertices = M.delete lab updatedVertices
      put (deletedEdges, deletedVertices, replacements)
  | otherwise = return ()

eliminateDeadJump lab [inst@(Br val lDstT lDstF)]
  | lab `notElem` [lDstT, lDstF] = do
    (edges, vertices, _) <- get
    let (preds, _) = let (Just x) = M.lookup lab edges in x
        predBlocks = map (\p -> let (Just x) = M.lookup p vertices in x) preds
    when (length preds == 1 && isGoto (head predBlocks)) $ do
      mergeEdges (head preds) lab lDstT
      mergeEdges (head preds) lab lDstF
      (newEdges, newVertices, replacements) <- get
      let block = let (Just x) = M.lookup lab newVertices in x
          newPredBlock = init (head predBlocks) ++ tail block
          updatedVertices = M.insert (head preds) newPredBlock newVertices
          deletedEdges = M.delete lab newEdges
          deletedVertices = M.delete lab updatedVertices
      put (deletedEdges, deletedVertices, replacements)
  | otherwise = return ()

eliminateDeadJump lab [inst@(RetInst val)] = eliminateDeadJumpRet lab inst
eliminateDeadJump lab [inst@VRetInst] = eliminateDeadJumpRet lab inst

eliminateDeadJump lab (inst : insts) = do
  eliminateDeadJump lab insts
  return ()

eliminateDeadJumpRet :: Label -> LlvmInst -> OptDeadM ()
eliminateDeadJumpRet lab ret = do
  (edges, vertices, replacements) <- get
  let (preds, _) = let (Just x) = M.lookup lab edges in x
      predBlocks = map (\p -> let (Just x) = M.lookup p vertices in x) preds
  when (length preds == 1 && isGoto (head predBlocks)) $ do
    let fromLab = head preds
        (fromPreds, fromSuccs) = let (Just x) = M.lookup fromLab edges in x
        updatedFromSuccs = delete lab fromSuccs
        updatedEdges = M.insert fromLab (fromPreds, updatedFromSuccs) edges
        block = let (Just x) = M.lookup lab vertices in x
        newPredBlock = init (head predBlocks) ++ tail block
        updatedVertices = M.insert (head preds) newPredBlock vertices
        deletedEdges = M.delete lab updatedEdges
        deletedVertices = M.delete lab updatedVertices
    put (deletedEdges, deletedVertices, replacements)

eliminateDeadJumpGoto :: Label -> Label -> OptDeadM ()
eliminateDeadJumpGoto lab lDst
  | lab /= lDst = do
    (edges, vertices, _) <- get
    let (preds, _) = let (Just x) = M.lookup lab edges in x
        predBlocks = map (\p -> let (Just x) = M.lookup p vertices in x) preds
        block = let (Just x) = M.lookup lab vertices in x
        toBlock = let (Just x) = M.lookup lDst vertices in x
        (phis, _) = partition isPhi block
        (toPhis, _) = partition isPhi toBlock
        canMovePhis = null phis || not (null toPhis)
    when (not (null predBlocks) && all isGoto predBlocks && canMovePhis) $ do
      rerouteEdge preds lab lDst
      (newEdges, newVertices, replacements) <- get
      let replacedPredBlocks = map (\b -> init b ++ [Goto lDst]) predBlocks
          labPredBlocks = zip preds replacedPredBlocks
          foldInsertLabPredBlock vs (l, b) = M.insert l b vs
          updatedVertices =
            foldl foldInsertLabPredBlock newVertices labPredBlocks
          deletedEdges = M.delete lab newEdges
          deletedVertices = M.delete lab updatedVertices
      put (deletedEdges, deletedVertices, replacements)
  | otherwise = return ()

eliminateDeadJumpBr :: Label -> Value -> Label -> Label -> OptDeadM ()
eliminateDeadJumpBr lab val lDstT lDstF
  | lab `notElem` [lDstT, lDstF] = do
    (edges, vertices, _) <- get
    let (preds, _) = let (Just x) = M.lookup lab edges in x
        predBlocks = map (\p -> let (Just x) = M.lookup p vertices in x) preds
        block = let (Just x) = M.lookup lab vertices in x
        toBlockT = let (Just x) = M.lookup lDstT vertices in x
        toBlockF = let (Just x) = M.lookup lDstF vertices in x
        (phis, _) = partition isPhi block
        (toPhisT, _) = partition isPhi toBlockT
        (toPhisF, _) = partition isPhi toBlockF
        canMovePhis = null phis || (not (null toPhisT) && not (null toPhisF))
    when (not (null predBlocks) && all isGoto predBlocks && canMovePhis) $ do
      rerouteEdge preds lab lDstT
      rerouteEdge preds lab lDstF
      (newEdges, newVertices, replacements) <- get
      let replacedPredBlocks =
            map (\b -> init b ++ [Br val lDstT lDstF]) predBlocks
          labPredBlocks = zip preds replacedPredBlocks
          foldInsertLabPredBlock vs (l, b) = M.insert l b vs
          updatedVertices =
            foldl foldInsertLabPredBlock newVertices labPredBlocks
          deletedEdges = M.delete lab newEdges
          deletedVertices = M.delete lab updatedVertices
      put (deletedEdges, deletedVertices, replacements)
  | otherwise = return ()

mergeEdges :: Label -> Label -> Label -> OptDeadM ()
mergeEdges fromLab viaLab toLab = do
  (edges, vertices, replacements) <- get
  let (fromPreds, fromSuccs) = let (Just x) = M.lookup fromLab edges in x
      (toPreds, toSuccs) = let (Just x) = M.lookup toLab edges in x
      updatedFromSuccs = nub $ toLab : delete viaLab toPreds
      updatedToPreds = nub $ fromLab : delete viaLab toPreds
      tmpEdges = M.insert fromLab (fromPreds, updatedFromSuccs) edges
      updatedEdges = M.insert toLab (updatedToPreds, toSuccs) tmpEdges
  put (updatedEdges, vertices, replacements)

isGoto :: [LlvmInst] -> Bool
isGoto insts = case last insts of
  Goto _ -> True
  Br {} -> False

rerouteEdge :: [Label] -> Label -> Label -> OptDeadM ()
rerouteEdge fromLabs viaLab toLab = do
  (edges, vertices, replacements) <- get
  let (toPreds, toSuccs) = let (Just x) = M.lookup toLab edges in x
      updatedToPreds = nub $ fromLabs ++ delete viaLab toPreds
      tmpEdges = foldl foldUpdateFromEdge edges fromLabs
        where
          foldUpdateFromEdge e l =
            M.insert l (lPreds, nub $ toLab : delete viaLab lSuccs) e
              where (lPreds, lSuccs) = let (Just x) = M.lookup l e in x
      updatedEdges = M.insert toLab (updatedToPreds, toSuccs) tmpEdges

      viaBlock = let (Just x) = M.lookup viaLab vertices in x
      toBlock = let (Just x) = M.lookup toLab vertices in x
      (viaPhis, _) = partition isPhi viaBlock
      (toPhis, toNoPhis) = partition isPhi toBlock
      newPhis = case toPhis of
        [] -> viaPhis
        _ -> expandPhis viaLab fromLabs viaPhisMap toPhis
          where
            viaPhisMap = M.fromList (map (\(Phi l t vls) -> (l, vls)) viaPhis)
      newToBlock = head toBlock : newPhis ++ tail toNoPhis
      updatedVertices = M.insert toLab newToBlock vertices
  put (updatedEdges, updatedVertices, replacements)

isPhi :: LlvmInst -> Bool
isPhi Phi {} = True
isPhi inst = False

expandPhis ::
  Label -> [Label] -> M.Map Loc [(Value, Label)] -> [LlvmInst] -> [LlvmInst]
expandPhis _ _ _ [] = []
expandPhis viaLab fromLabs viaPhis (Phi loc t valLabs : currPhis) =
  Phi loc t updatedValLabs : expandPhis viaLab fromLabs viaPhis currPhis
  where
    getValLab ((val, lab) : vlrest)
      | lab == viaLab = val
      | otherwise = getValLab vlrest
    newValLabs = case getValLab valLabs of
      r@(Reg vlLoc _) ->
        M.findWithDefault (zip (repeat r) fromLabs) vlLoc viaPhis
      v -> zip (repeat v) fromLabs -- referencing constant, repeat
    updatedValLabs = newValLabs ++ deleteValLab viaLab valLabs

eliminateUnreachables :: Label -> OptDeadM ()
eliminateUnreachables lab = do
  (_, vertices, _) <- get
  case M.lookup lab vertices of
    Just insts -> do
      updatedInsts <- eliminateUnreachable lab insts
      (edges, newVertices, replacements) <- get
      put (edges, M.insert lab updatedInsts newVertices, replacements)
    Nothing -> return ()

eliminateUnreachable :: Label -> [LlvmInst] -> OptDeadM [LlvmInst]
eliminateUnreachable _ [] = return []

eliminateUnreachable lab [Br (BoolLit True) lDstT lDstF] = do
  deleteEdge lab lDstF
  return [Goto lDstT]
eliminateUnreachable lab [Br (BoolLit False) lDstT lDstF] = do
  deleteEdge lab lDstT
  return [Goto lDstF]

eliminateUnreachable lab (RetInst val : insts) = do
  (edges, _, _) <- get
  mapM_ (deleteEdge lab) (let (Just x) = M.lookup lab edges in snd x)
  return [RetInst val]
eliminateUnreachable lab (VRetInst : insts) = do
  (edges, _, _) <- get
  mapM_ (deleteEdge lab) (let (Just x) = M.lookup lab edges in snd x)
  return [VRetInst]

eliminateUnreachable lab (inst : insts) = do
   rest <- eliminateUnreachable lab insts
   return $ inst : rest

deleteEdge :: Label -> Label -> OptDeadM ()
deleteEdge fromLab toLab = do
  (edges, vertices, replacements) <- get
  let (fromPreds, fromSuccs) = let (Just x) = M.lookup fromLab edges in x
      (toPreds, toSuccs) = let (Just x) = M.lookup toLab edges in x
      updatedFromSuccs = delete toLab fromSuccs
      updatedToPreds = delete fromLab toPreds
      tmpEdges = M.insert fromLab (fromPreds, updatedFromSuccs) edges
      updatedEdges = M.insert toLab (updatedToPreds, toSuccs) tmpEdges
  put (updatedEdges, vertices, replacements)
  if null updatedToPreds then do
    mapM_ (deleteEdge toLab) toSuccs
    (freshEdges, freshVertices, replacements) <- get
    let deletedEdges = M.delete toLab freshEdges
        deletedVertices = M.delete toLab freshVertices
    put (deletedEdges, deletedVertices, replacements)
  else
    shrinkPhis fromLab toLab

shrinkPhis :: Label -> Label -> OptDeadM ()
shrinkPhis fromLab toLab = do
  (_, vertices, _) <- get
  let insts = let (Just x) = M.lookup toLab vertices in x
  updatedInsts <- concatMapM (shrinkPhi fromLab) insts
  (edges, newVertices, replacements) <- get
  put (edges, M.insert toLab updatedInsts newVertices, replacements)

shrinkPhi :: Label -> LlvmInst -> OptDeadM [LlvmInst]
shrinkPhi lab (Phi loc t valLabs) = do
  let updatedValLabs = deleteValLab lab valLabs
  case updatedValLabs of
    [(pVal, pLab)] -> do
      (edges, vertices, replacements) <- get
      put (edges, vertices, M.insert (Reg loc t) pVal replacements)
      return []
    vals -> return [Phi loc t vals]
shrinkPhi lab inst = return [inst]

deleteValLab :: Label -> [(Value, Label)] -> [(Value, Label)]
deleteValLab lab [] = []
deleteValLab lab (lv@(pVal, pLab) : labVals)
  | lab == pLab = labVals
  | otherwise = lv : deleteValLab lab labVals

replaceVals :: Label -> OptDeadM ()
replaceVals lab = do
  (edges, vertices, replacements) <- get
  case M.lookup lab vertices of
    Just insts -> do
      let replacedInsts = replaceValsInsts insts replacements
      put (edges, M.insert lab replacedInsts vertices, replacements)
      return ()
    Nothing -> return ()

replaceValsInsts :: [LlvmInst] -> Replacements -> [LlvmInst]
replaceValsInsts [] _ = []
replaceValsInsts (RetInst val : insts) replacements =
  RetInst newVal : replaceValsInsts insts replacements
  where newVal = M.findWithDefault val val replacements
replaceValsInsts (Br val labT labF : insts) replacements =
  Br newVal labT labF : replaceValsInsts insts replacements
  where newVal = M.findWithDefault val val replacements
replaceValsInsts (Call loc t n vals : insts) replacements =
  Call loc t n newVals : replaceValsInsts insts replacements
  where newVals = map (\val -> M.findWithDefault val val replacements) vals
replaceValsInsts (Mul loc val1 val2: insts) replacements =
  Mul loc newVal1 newVal2 : replaceValsInsts insts replacements
  where newVal1 = M.findWithDefault val1 val1 replacements
        newVal2 = M.findWithDefault val2 val2 replacements
replaceValsInsts (SDiv loc val1 val2: insts) replacements =
  SDiv loc newVal1 newVal2 : replaceValsInsts insts replacements
  where newVal1 = M.findWithDefault val1 val1 replacements
        newVal2 = M.findWithDefault val2 val2 replacements
replaceValsInsts (SRem loc val1 val2: insts) replacements =
  SRem loc newVal1 newVal2 : replaceValsInsts insts replacements
  where newVal1 = M.findWithDefault val1 val1 replacements
        newVal2 = M.findWithDefault val2 val2 replacements
replaceValsInsts (Add loc val1 val2: insts) replacements =
  Add loc newVal1 newVal2 : replaceValsInsts insts replacements
  where newVal1 = M.findWithDefault val1 val1 replacements
        newVal2 = M.findWithDefault val2 val2 replacements
replaceValsInsts (Sub loc val1 val2: insts) replacements =
  Sub loc newVal1 newVal2 : replaceValsInsts insts replacements
  where newVal1 = M.findWithDefault val1 val1 replacements
        newVal2 = M.findWithDefault val2 val2 replacements
replaceValsInsts (Icmp loc op val1 val2: insts) replacements =
  Icmp loc op newVal1 newVal2 : replaceValsInsts insts replacements
  where newVal1 = M.findWithDefault val1 val1 replacements
        newVal2 = M.findWithDefault val2 val2 replacements
replaceValsInsts (Phi loc t valLabs : insts) replacements =
  Phi loc t newValLabs : replaceValsInsts insts replacements
  where
  newValLabs =
    map (\(val, lab) -> (M.findWithDefault val val replacements, lab)) valLabs
replaceValsInsts (inst : insts) replacements =
  inst : replaceValsInsts insts replacements


-- Performs const folding, const propagation and peephole optimizations.
propagateConstsInsts :: [LlvmInst] -> OptConstM [LlvmInst]
propagateConstsInsts = mapM propagateConstsInst

propagateConstsInst :: LlvmInst -> OptConstM LlvmInst
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
        (IntLit 1, reg) -> M.insert loc reg consts
        (reg, IntLit 1) -> M.insert loc reg consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ Mul loc newVal1 newVal2
propagateConstsInst (SDiv loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 `quot` i2)) consts
        (reg, IntLit 1) -> M.insert loc reg consts
        (Reg loc1 t1, Reg loc2 t2)
          | loc1 == loc2 -> M.insert loc (IntLit 1) consts
          | otherwise -> consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ SDiv loc newVal1 newVal2
propagateConstsInst (SRem loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 `rem` i2)) consts
        (reg, IntLit 1) -> M.insert loc (IntLit 0) consts
        (Reg loc1 t1, Reg loc2 t2)
          | loc1 == loc2 -> M.insert loc (IntLit 0) consts
          | otherwise -> consts
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
        (IntLit 0, reg) -> M.insert loc reg consts
        (reg, IntLit 0) -> M.insert loc reg consts
        (BoolLit False, reg) -> M.insert loc reg consts
        (reg, BoolLit False) -> M.insert loc reg consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ Add loc newVal1 newVal2
propagateConstsInst (Sub loc val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) -> M.insert loc (IntLit (i1 - i2)) consts
        (reg, IntLit 0) -> M.insert loc reg consts
        (Reg loc1 t1, Reg loc2 t2)
          | loc1 == loc2 -> M.insert loc (IntLit 0) consts
          | otherwise -> consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ Sub loc newVal1 newVal2
propagateConstsInst (Icmp loc op val1 val2) = do
  newVal1 <- computeVal val1
  newVal2 <- computeVal val2
  (consts, strLits) <- get
  let updatedConsts = case (newVal1, newVal2) of
        (IntLit i1, IntLit i2) ->
          M.insert loc (BoolLit (cmpInt op i1 i2)) consts
        (BoolLit b1, BoolLit b2) ->
          M.insert loc (BoolLit (cmpBool op b1 b2)) consts
        (Reg loc1 t1, Reg loc2 t2)
          | loc1 == loc2 -> M.insert loc (BoolLit (isReflective op)) consts
          | otherwise -> consts
        _ -> consts
  put (updatedConsts, strLits)
  return $ Icmp loc op newVal1 newVal2
propagateConstsInst (Phi loc t valLabs) = do
  let (vals, labs) = unzip valLabs
  newVals <- mapM computeVal vals
  return $ Phi loc t (zip newVals labs)
propagateConstsInst inst = return inst


-- Utils

xor :: Bool -> Bool -> Bool
xor a b = not (a && b) && (a || b)

cmpInt :: LlvmRelOp -> Int32 -> Int32 -> Bool
cmpInt Lth = (<)
cmpInt Le = (<=)
cmpInt Gth = (>)
cmpInt Ge = (>=)
cmpInt Equ = (==)
cmpInt Ne = (/=)

cmpBool :: LlvmRelOp -> Bool -> Bool -> Bool
cmpBool Equ = (==)
cmpBool Ne = (/=)

isReflective :: LlvmRelOp -> Bool
isReflective Lth = False
isReflective Le = True
isReflective Gth = False
isReflective Ge = True
isReflective Equ = True
isReflective Ne = False

computeVal :: Value -> OptConstM Value
computeVal r@(Reg loc t) = do
  (consts, _) <- get
  case M.lookup loc consts of
    Just val -> return val
    Nothing -> return r
computeVal val = return val
