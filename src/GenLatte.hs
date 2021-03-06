module GenLatte where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import qualified Data.Map as M
import Data.Maybe

import AbsLatte
import UtilsLatte
import LlvmLatte

type SymTab = M.Map Ident (Loc, LlvmType)

builtins :: [(Ident, (Loc, LlvmType))]
builtins = [(Ident "printInt", (emptyLoc, LlvmVoid)),
            (Ident "printString", (emptyLoc, LlvmVoid)),
            (Ident "error", (emptyLoc, LlvmVoid)),
            (Ident "readInt", (emptyLoc, LlvmInt)),
            (Ident "readString", (emptyLoc, LlvmStr))]

type GenLlvmM a =
  WriterT [LlvmInst] (StateT (Int, Int) (Reader SymTab)) a

runGenLlvmM :: [Stmt Pos] -> Int -> Int -> SymTab -> [LlvmInst]
runGenLlvmM ss i l m = snd $ fst r where
  r = runReader (runStateT (runWriterT (generateLlvmStmts ss)) (i, l)) m

generateLlvm :: Program Pos -> LlvmProg
generateLlvm (Program _ topDefs) = LlvmProg [] externs defines
  where
    foldPutTopDefs globals (FnDef _ fRet fName _ _) =
      M.insert fName (emptyLoc, toLlvmType fRet) globals
    globals = foldl foldPutTopDefs (M.fromList builtins) topDefs
    runGenIrTopDef (FnDef _ fType (Ident name) args (Block _ stmts)) =
      LlvmDef dType name dArgs insts
      where
        dType = toLlvmType fType
        dArgs = zipWith toLlvmArg args [0 ..]
        toLlvmArg (Arg _ aType _) i = Reg (getArgLoc i) (toLlvmType aType)
        (locals, n) = foldl insertArgs (globals, 0) args
        insertArgs (symTab, i) (Arg _ aType ident) =
          (M.insert ident (getArgLoc i, toLlvmType aType) symTab, i + 1)
        insts = Lab (getLabel (-1)) : body ++ vRet
          where
            body = runGenLlvmM stmts n 0 locals
            -- Ensure that void function returns.
            vRet = case fType of
              (Void _)
                | null body -> [VRetInst]
                | last body == VRetInst -> []
                | otherwise -> [VRetInst]
              _ -> []
    defines = map runGenIrTopDef topDefs

generateLlvmStmts :: [Stmt Pos] -> GenLlvmM ()
generateLlvmStmts [] = return ()

generateLlvmStmts (Empty _ : stmts) = generateLlvmStmts stmts

generateLlvmStmts (BStmt _ (Block _ bStmts) : stmts) = do
  generateLlvmStmts bStmts
  generateLlvmStmts stmts

-- NOTE: Decl has at least one item.
generateLlvmStmts (Decl pos dType (item : items) : stmts) = do
  let (ident, e) = case item of
                     Init _ ident e -> (ident, e)
                     NoInit _ ident -> case dType of
                       Str p -> (ident, EString p "\"\"")
                       Int p -> (ident, ELitInt p 0)
                       Bool p -> (ident, ELitFalse p)
  val <- generateLlvmExp e
  valLoc <- getValLoc val
  (n, l) <- get
  put (n + 1, l)
  -- We can expand other declarations as Decl of [item] is same as [Decl item].
  let decls = map (\nItem -> Decl pos dType [nItem]) items
      tmpLoc = getTmpLoc n
  tell [AssInst (toLlvmType dType) tmpLoc valLoc]
  let updatedSymTab = M.insert ident (tmpLoc, toLlvmType dType)
  local updatedSymTab $ generateLlvmStmts (decls ++ stmts)
  return ()

generateLlvmStmts (Ass _ ident e : stmts) = do
  symTab <- ask
  let (loc, irType) = fromJust $ M.lookup ident symTab
  val <- generateLlvmExp e
  valLoc <- getValLoc val
  tell [AssInst irType loc valLoc]
  generateLlvmStmts stmts

generateLlvmStmts (Incr _ ident : stmts) = do
  symTab <- ask
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
      (loc, irType) = fromJust $ M.lookup ident symTab
  tell [Add newLoc (Reg loc irType) (IntLit 1),
        AssInst LlvmInt loc newLoc]
  generateLlvmStmts stmts

generateLlvmStmts (Decr _ ident : stmts) = do
  symTab <- ask
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
      (loc, irType) = fromJust $ M.lookup ident symTab
  tell [Sub newLoc (Reg loc irType) (IntLit 1),
        AssInst LlvmInt loc newLoc]
  generateLlvmStmts stmts

generateLlvmStmts (Ret _ e : stmts) = do
  val <- generateLlvmExp e
  tell [RetInst val]
  generateLlvmStmts stmts
generateLlvmStmts (VRet _ : stmts) = do
  tell [VRetInst]
  generateLlvmStmts stmts

generateLlvmStmts (Cond _ e stmt : stmts) = do
  (n, l) <- get
  put (n, l + 2)
  let labT = getLabel l
      labF = getLabel $ l + 1
  val <- generateLlvmExp e
  tell [Br val labT labF,
        Lab labT]
  generateLlvmStmts [stmt]
  tell [Goto labF,
        Lab labF]
  generateLlvmStmts stmts

generateLlvmStmts (CondElse _ e stmtT stmtF : stmts) = do
  (n, l) <- get
  put (n, l + 3)
  let labT = getLabel l
      labF = getLabel $ l + 1
      labEnd = getLabel $ l + 2
  val <- generateLlvmExp e
  tell [Br val labT labF,
        Lab labT]
  generateLlvmStmts [stmtT]
  tell [Goto labEnd,
        Lab labF]
  generateLlvmStmts [stmtF]
  tell [Goto labEnd,
        Lab labEnd]
  generateLlvmStmts stmts

generateLlvmStmts (While _ e stmt : stmts) = do
  (n, l) <- get
  put (n, l + 3)
  let labCond = getLabel l
      labBody = getLabel $ l + 1
      labEnd = getLabel $ l + 2
  tell [Goto labCond,
        Lab labCond]
  val <- generateLlvmExp e
  tell [Br val labBody labEnd,
        Lab labBody]
  generateLlvmStmts [stmt]
  tell [Goto labCond,
        Lab labEnd]
  generateLlvmStmts stmts

generateLlvmStmts (SExp _ e : stmts) = do
  generateLlvmExp e
  generateLlvmStmts stmts

generateLlvmExp :: Expr Pos -> GenLlvmM Value

generateLlvmExp (EVar _ ident) = do
  symTab <- ask
  let (loc, irType) = fromJust $ M.lookup ident symTab
  return $ Reg loc irType

generateLlvmExp (ELitInt _ i) = return $ IntLit (fromInteger i)  -- safe cast
generateLlvmExp (ELitTrue _) = return $ BoolLit True
generateLlvmExp (ELitFalse _) = return $ BoolLit False

generateLlvmExp (EApp _ ident@(Ident name) es) = do
  symTab <- ask
  (n, l) <- get
  put (n + 1, l)
  let (_, rType) = fromJust $ M.lookup ident symTab
      newLoc = getLoc n
  vals <- mapM generateLlvmExp es
  tell [Call newLoc rType name vals]
  return $ Reg newLoc rType

generateLlvmExp (EString _ s) = do
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  tell [StrLit newLoc s emptyLoc]
  return $ Reg newLoc LlvmStr

generateLlvmExp (Neg _ e) = do
  val <- generateLlvmExp e
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  tell [Sub newLoc (IntLit 0) val]
  return $ Reg newLoc LlvmInt

generateLlvmExp (Not _ e) = do
  val <- generateLlvmExp e
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  tell [Icmp newLoc Equ val (BoolLit False)]
  return $ Reg newLoc LlvmBool

generateLlvmExp (EMul _ e1 op e2) = do
  val1 <- generateLlvmExp e1
  val2 <- generateLlvmExp e2
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  case op of
    (Times _) ->
      tell [Mul newLoc val1 val2]
    (Div _) ->
      tell [SDiv newLoc val1 val2]
    (Mod _) ->
      tell [SRem newLoc val1 val2]
  return $ Reg newLoc $ getType val1

generateLlvmExp (EAdd _ e1 op e2) = do
  val1 <- generateLlvmExp e1
  val2 <- generateLlvmExp e2
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  case op of
    (Plus _) -> case getType val1 of
      LlvmStr ->
        tell [Call newLoc LlvmStr "_concat" [val1, val2]]
      _ ->
        tell [Add newLoc val1 val2]
    (Minus _) ->
      tell [Sub newLoc val1 val2]
  return $ Reg newLoc $ getType val1

generateLlvmExp (ERel _ e1 op e2) = do
  val1 <- generateLlvmExp e1
  val2 <- generateLlvmExp e2
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  case getType val1 of
    LlvmStr -> case op of
    -- Use pre-defined function to cmp strings.
      EQU _ -> do
        tell [Call newLoc LlvmBool "_streq" [val1, val2]]
        return $ Reg newLoc LlvmBool
      NE _ -> do
        let newLocN = getLoc $ n + 1
        put (n + 1, l)
        tell [Call newLoc LlvmBool "_streq" [val1, val2],
              Icmp newLocN Equ (Reg newLoc LlvmBool) (BoolLit False)]
        return $ Reg newLocN LlvmBool
    _ -> do
      tell [Icmp newLoc (toIrOp op) val1 val2]
      return $ Reg newLoc LlvmBool

generateLlvmExp (EAnd _ e1 e2) = do
  val1 <- generateLlvmExp e1
  val1Loc <- getValLoc val1
  (n, l) <- get
  put (n + 1, l + 3)
  let tmpLoc = getTmpLoc n
      labE1F = getLabel l
      labE2 = getLabel $ l + 1
      labEnd = getLabel $ l + 2
  tell [Br val1 labE2 labE1F,
        Lab labE1F,
        AssInst LlvmBool tmpLoc val1Loc,
        Goto labEnd,
        Lab labE2]
  val2 <- generateLlvmExp e2
  val2Loc <- getValLoc val2
  tell [AssInst LlvmBool tmpLoc val2Loc,
        Goto labEnd,
        Lab labEnd]
  return $ Reg tmpLoc LlvmBool

generateLlvmExp (EOr _ e1 e2) = do
  val1 <- generateLlvmExp e1
  val1Loc <- getValLoc val1
  (n, l) <- get
  put (n + 1, l + 3)
  let tmpLoc = getTmpLoc n
      labE1T = getLabel l
      labE2 = getLabel $ l + 1
      labEnd = getLabel $ l + 2
  tell [Br val1 labE1T labE2,
        Lab labE1T,
        AssInst LlvmBool tmpLoc val1Loc,
        Goto labEnd,
        Lab labE2]
  val2 <- generateLlvmExp e2
  val2Loc <- getValLoc val2
  tell [AssInst LlvmBool tmpLoc val2Loc,
        Goto labEnd,
        Lab labEnd]
  return $ Reg tmpLoc LlvmBool


-- Utils

getValLoc :: Value -> GenLlvmM Loc
getValLoc (Reg valLoc _) = return valLoc
getValLoc val = do
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  case val of
    IntLit i -> tell [Add newLoc val (IntLit 0)]
    BoolLit b -> tell [Add newLoc val (BoolLit False)]
  return newLoc

getLoc :: Int -> Loc
getLoc = _getLoc 'r'

-- Returns loc which will be removed after SSA phase.
getTmpLoc :: Int -> Loc
getTmpLoc = _getLoc 't'

getArgLoc :: Int -> Loc
getArgLoc = _getLoc 'a'

getLabel :: Int -> Label
getLabel = _getLoc 'l'

toLlvmType :: TypePos -> LlvmType
toLlvmType (Int _) = LlvmInt
toLlvmType (Str _) = LlvmStr
toLlvmType (Bool _) = LlvmBool
toLlvmType (Void _) = LlvmVoid

toIrOp :: RelOp Pos -> LlvmRelOp
toIrOp (LTH _) = Lth
toIrOp (LE _) = Le
toIrOp (GTH _) = Gth
toIrOp (GE _) = Ge
toIrOp (EQU _) = Equ
toIrOp (NE _) = Ne

getType :: Value -> LlvmType
getType (Reg _ t) = t
getType (IntLit _)  = LlvmInt
getType (BoolLit _) = LlvmBool
