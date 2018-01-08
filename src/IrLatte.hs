module IrLatte where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Maybe

import AbsLatte
import UtilsLatte

data IrType = IrInt | IrStr | IrBool | IrVoid
  deriving (Eq, Ord, Read)

type Loc = String
-- Value applied to instruction can be a register or a literal value.
data Value = Reg Loc IrType | IntLit Integer | BoolLit Bool
  deriving (Eq, Ord, Read)

type Label = String

data IrRelOp = Lth | Le | Gth | Ge | Equ | Ne
  deriving (Eq, Ord, Read)

data IrInst
  = AssInst IrType Loc Loc  -- Internal inst eliminated after SSA; a := b
  | RetInst Value
  | VRetInst
  | Br Value Label Label
  | Goto Label
  | Lab Label
  | Call Loc IrType String [Value]
  | StrLit Loc String Loc
  | Mul Loc Value Value
  | SDiv Loc Value Value
  | SRem Loc Value Value
  | Add Loc Value Value
  | Sub Loc Value Value
  | Icmp Loc IrRelOp Value Value
  deriving (Eq, Ord, Read)

data BasicBlock = BasicBlock Label [IrInst]

type SymTab = M.Map Ident (Loc, IrType)
type StrMap = M.Map String Loc

emptyLoc :: Loc
emptyLoc = ""

builtins :: [(Ident, (Loc, IrType))]
builtins = [(Ident "printInt", (emptyLoc, IrVoid)),
            (Ident "printString", (emptyLoc, IrVoid)),
            (Ident "error", (emptyLoc, IrVoid)),
            (Ident "readInt", (emptyLoc, IrInt)),
            (Ident "readString", (emptyLoc, IrStr))]

type GenIrM a =
  WriterT [IrInst] (StateT (Int, Int) (Reader (SymTab, StrMap))) a

runGenIrM :: [Stmt Pos] -> Int -> Int -> SymTab -> StrMap -> [IrInst]
runGenIrM stmts i l s m = snd $ fst r where
  r = runReader (runStateT (runWriterT (generateIrStmts stmts)) (i, l)) (s, m)

-- TODO: insert l0 at the beginning of the function
generateIr :: Program Pos -> [[IrInst]]
generateIr (Program _ topDefs) = map runGenIrTopDef topDefs
  where
    foldPutTopDefs globals (FnDef _ fRet fName _ _) =
      M.insert fName (emptyLoc, toIrType fRet) globals
    globals = foldl foldPutTopDefs (M.fromList builtins) topDefs
    strLit = "" : concatMap getStrLiteralsTopDef topDefs
    strLitLoc = map (\(s, i) -> (s, "@.str." ++ show i)) $ zip strLit [0..]
    strLitMap = M.fromList strLitLoc
    runGenIrTopDef (FnDef _ _ _ args (Block _ stmts)) =
      runGenIrM stmts n 0 locals strLitMap
      where
        (locals, n) = foldl insertArgs (globals, 0) args
        insertArgs (symTab, loc) (Arg _ aType ident) =
          (M.insert ident (getArgLoc loc, toIrType aType) globals, loc + 1)

getStrLiteralsTopDef :: TopDef Pos -> [String]
getStrLiteralsTopDef (FnDef _ _ _ _ (Block _ stmts)) =
  getStrLiteralsStmts stmts

getStrLiteralsStmts :: [Stmt Pos] -> [String]
getStrLiteralsStmts = concatMap getStrLiteralsStmt

getStrLiteralsStmt :: Stmt Pos -> [String]
getStrLiteralsStmt (BStmt _ (Block _ stmts)) = getStrLiteralsStmts stmts
getStrLiteralsStmt (Decl _ _ items) = concatMap getStrLiteralsItem items
getStrLiteralsStmt (Ass _ _ e) = getStrLiteralsExp e
getStrLiteralsStmt (Ret _ e) = getStrLiteralsExp e
getStrLiteralsStmt (Cond _ e stmt) =
  getStrLiteralsExp e ++ getStrLiteralsStmt stmt
getStrLiteralsStmt (CondElse _ e stmtT stmtF) =
  getStrLiteralsExp e ++ getStrLiteralsStmt stmtT ++ getStrLiteralsStmt stmtF
getStrLiteralsStmt (While _ e stmt) =
  getStrLiteralsExp e ++ getStrLiteralsStmt stmt
getStrLiteralsStmt (SExp _ e) = getStrLiteralsExp e
getStrLiteralsStmt _ = []

getStrLiteralsItem :: Item Pos -> [String]
getStrLiteralsItem (Init _ _ e) = getStrLiteralsExp e
getStrLiteralsItem _ = []

getStrLiteralsExp :: Expr Pos -> [String]
getStrLiteralsExp (EApp _ _ es) = concatMap getStrLiteralsExp es
getStrLiteralsExp (EString _ s) = [s]
getStrLiteralsExp (Neg _ e) = getStrLiteralsExp e
getStrLiteralsExp (Not _ e) = getStrLiteralsExp e
getStrLiteralsExp (EMul _ e1 _ e2) =
  getStrLiteralsExp e1 ++ getStrLiteralsExp e2
getStrLiteralsExp (EAdd _ e1 _ e2) =
  getStrLiteralsExp e1 ++ getStrLiteralsExp e2
getStrLiteralsExp (ERel _ e1 _ e2) =
  getStrLiteralsExp e1 ++ getStrLiteralsExp e2
getStrLiteralsExp (EAnd _ e1 e2) =
  getStrLiteralsExp e1 ++ getStrLiteralsExp e2
getStrLiteralsExp (EOr _ e1 e2) =
  getStrLiteralsExp e1 ++ getStrLiteralsExp e2
getStrLiteralsExp _ = []

generateIrStmts :: [Stmt Pos] -> GenIrM ()

generateIrStmts (Empty _ : stmts) = generateIrStmts stmts

generateIrStmts (BStmt _ (Block _ bStmts) : stmts) = do
  generateIrStmts bStmts
  generateIrStmts stmts

-- NOTE: Decl has at least one item.
generateIrStmts (Decl pos dType (item : items) : stmts) = do
  let (ident, e) = case item of
                     Init _ ident e -> (ident, e)
                     NoInit _ ident -> case dType of
                       Str p -> (ident, EString p "")
                       Int p -> (ident, ELitInt p 0)
                       Bool p -> (ident, ELitFalse p)
  val <- generateIrExp e
  valLoc <- getValLoc val
  (n, l) <- get
  put (n + 1, l)
  -- We can expand other declarations as Decl of [item] is same as [Decl item].
  let decls = map (\nItem -> Decl pos dType [nItem]) items
      newLoc = getLoc n
  tell [AssInst (toIrType dType) newLoc valLoc]
  let newEnv (s, m) = (M.insert ident (newLoc, toIrType dType) s, m)
  local newEnv $ generateIrStmts (decls ++ stmts)
  return ()

generateIrStmts (Ass _ ident e : stmts) = do
  (symTab, _) <- ask
  let (loc, irType) = fromJust $ M.lookup ident symTab
  val <- generateIrExp e
  valLoc <- getValLoc val
  tell [AssInst irType loc valLoc]
  generateIrStmts stmts

generateIrStmts (Incr _ ident : stmts) = do
  (symTab, _) <- ask
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
      (loc, irType) = fromJust $ M.lookup ident symTab
  tell [Add newLoc (Reg loc irType) (IntLit 1),
        AssInst IrInt loc newLoc]
  generateIrStmts stmts

generateIrStmts (Decr _ ident : stmts) = do
  (symTab, _) <- ask
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
      (loc, irType) = fromJust $ M.lookup ident symTab
  tell [Sub newLoc (Reg loc irType) (IntLit 1),
        AssInst IrInt loc newLoc]
  generateIrStmts stmts

generateIrStmts (Ret _ e : stmts) = do
  val <- generateIrExp e
  tell [RetInst val]
  generateIrStmts stmts
generateIrStmts (VRet _ : stmts) = do
  tell [VRetInst]
  generateIrStmts stmts

generateIrStmts (Cond _ e stmt : stmts) = do
  (n, l) <- get
  put (n, l + 2)
  let labT = getLabel l
      labF = getLabel $ l + 1
  val <- generateIrExp e
  tell [Br val labT labF,
        Lab labT]
  generateIrStmts [stmt]
  tell [Goto labF,  -- goto to avoid empty blocks
        Lab labF]
  generateIrStmts stmts

generateIrStmts (CondElse _ e stmtT stmtF : stmts) = do
  (n, l) <- get
  put (n, l + 3)
  let labT = getLabel l
      labF = getLabel $ l + 1
      labEnd = getLabel $ l + 2
  val <- generateIrExp e
  tell [Br val labT labF,
        Lab labT]
  generateIrStmts [stmtT]
  tell [Goto labEnd,
        Lab labF]
  generateIrStmts [stmtF]
  tell [Goto labEnd,  -- goto to avoid empty blocks
        Lab labEnd]
  generateIrStmts stmts

generateIrStmts (While _ e stmt : stmts) = do
  (n, l) <- get
  put (n, l + 3)
  let labCond = getLabel l
      labBody = getLabel $ l + 1
      labEnd = getLabel $ l + 2
  tell [Goto labCond,  -- goto to avoid empty blocks
        Lab labCond]
  val <- generateIrExp e
  tell [Br val labBody labEnd,
        Lab labBody]
  generateIrStmts [stmt]
  tell [Goto labCond,
        Lab labEnd]
  generateIrStmts stmts

generateIrStmts (SExp _ e : stmts) = do
  generateIrExp e
  generateIrStmts stmts

generateIrExp :: Expr Pos -> GenIrM Value

generateIrExp (EVar _ ident) = do
  (symTab, _) <- ask
  let (loc, irType) = fromJust $ M.lookup ident symTab
  return $ Reg loc irType

generateIrExp (ELitInt _ i) = return $ IntLit i
generateIrExp (ELitTrue _) = return $ BoolLit True
generateIrExp (ELitFalse _) = return $ BoolLit False

generateIrExp (EApp _ ident es) = do
  (symTab, _) <- ask
  (n, l) <- get
  put (n + 1, l)
  let (_, rType) = fromJust $ M.lookup ident symTab
      newLoc = getLoc n
  vals <- mapM generateIrExp es
  tell [Call newLoc rType (show ident) vals]
  return $ Reg newLoc rType

generateIrExp (EString _ str) = do
  (_, strLitMap) <- ask
  (n, l) <- get
  put (n + 1, l)
  let sLoc = fromJust $ M.lookup str strLitMap
      newLoc = getLoc n
  tell [StrLit newLoc str sLoc]
  return $ Reg newLoc IrStr

generateIrExp (Neg _ e) = do
  val <- generateIrExp e
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  tell [Sub newLoc (IntLit 0) val]
  return $ Reg newLoc IrInt

generateIrExp (Not _ e) = do
  val <- generateIrExp e
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  tell [Icmp newLoc Equ val (BoolLit False)]
  return $ Reg newLoc IrBool

generateIrExp (EMul _ e1 op e2) = do
  val1 <- generateIrExp e1
  val2 <- generateIrExp e2
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

generateIrExp (EAdd _ e1 op e2) = do
  val1 <- generateIrExp e1
  val2 <- generateIrExp e2
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  case op of
    (Plus _) -> case getType val1 of
      IrStr ->
        tell [Call newLoc IrStr "_concat" [val1, val2]]
      _ ->
        tell [Add newLoc val1 val2]
    (Minus _) ->
      tell [Sub newLoc val1 val2]
  return $ Reg newLoc $ getType val1

generateIrExp (ERel _ e1 op e2) = do
  val1 <- generateIrExp e1
  val2 <- generateIrExp e2
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  case getType val1 of
    IrStr -> case op of
    -- Use pre-defined function to cmp strings.
      EQU _ -> do
        tell [Call newLoc IrBool "_streq" [val1, val2]]
        return $ Reg newLoc IrBool
      NE _ -> do
        let newLocN = getLoc $ n + 1
        put (n + 1, l)
        tell [Call newLoc IrBool "_streq" [val1, val2],
              Icmp newLocN Equ (Reg newLoc IrBool) (BoolLit False)]
        return $ Reg newLocN IrBool
    _ -> do
      tell [Icmp newLoc (toIrOp op) val1 val2]
      return $ Reg newLoc IrBool

generateIrExp (EAnd _ e1 e2) = do
  val1 <- generateIrExp e1
  val1Loc <- getValLoc val1
  (n, l) <- get
  put (n + 1, l + 3)
  let newLoc = getLoc n
      labE1F = getLabel l
      labE2 = getLabel $ l + 1
      labEnd = getLabel $ l + 2
  tell [Br val1 labE2 labE1F,
        Lab labE1F,
        AssInst IrBool newLoc val1Loc,
        Goto labEnd,
        Lab labE2]
  val2 <- generateIrExp e2
  val2Loc <- getValLoc val1
  tell [AssInst IrBool newLoc val2Loc,
        Lab labEnd]
  return $ Reg newLoc IrBool

generateIrExp (EOr _ e1 e2) = do
  val1 <- generateIrExp e1
  val1Loc <- getValLoc val1
  (n, l) <- get
  put (n + 1, l + 3)
  let newLoc = getLoc n
      labE1T = getLabel l
      labE2 = getLabel $ l + 1
      labEnd = getLabel $ l + 2
  tell [Br val1 labE1T labE2,
        Lab labE1T,
        AssInst IrBool newLoc val1Loc,
        Goto labEnd,
        Lab labE2]
  val2 <- generateIrExp e2
  val2Loc <- getValLoc val1
  tell [AssInst IrBool newLoc val2Loc,
        Lab labEnd]
  return $ Reg newLoc IrBool


-- Utils

getValLoc :: Value -> GenIrM Loc
getValLoc (Reg valLoc _) = return valLoc
getValLoc val = do
  (n, l) <- get
  put (n + 1, l)
  let newLoc = getLoc n
  case val of
    IntLit i -> tell [Add newLoc val (IntLit 0)]
    BoolLit b -> tell [Add newLoc val (BoolLit False)]
  return newLoc

-- Returns next registry location.
_getLoc :: Int -> Char -> Loc
_getLoc n c = '%' : c : show (n + 1)

getLoc :: Int -> Loc
getLoc n = _getLOc 'r' n

getArgLoc :: Int -> Loc
getArgLoc n = _getLOc 'a' n

getLabel :: Int -> Label
getLabel n = _getLOc 'l' n

toIrType :: TypePos -> IrType
toIrType (Int _) = IrInt
toIrType (Str _) = IrStr
toIrType (Bool _) = IrBool
toIrType (Void _) = IrVoid

toIrOp :: RelOp Pos -> IrRelOp
toIrOp (LTH _) = Lth
toIrOp (LE _) = Le
toIrOp (GTH _) = Gth
toIrOp (GE _) = Ge
toIrOp (EQU _) = Equ
toIrOp (NE _) = Ne

getType :: Value -> IrType
getType (Reg _ t) = t
getType (IntLit _)  = IrInt
getType (BoolLit _) = IrBool
