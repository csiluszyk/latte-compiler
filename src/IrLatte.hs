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

data Inst
  = AssInst Loc Value
  | RetInst Value
  | VRetInst
  | Br Value Label Label
  | Call Loc String [Value]
  | StrLit Loc String
  | Mul Loc Value Value
  | SDiv Loc Value Value
  | SRem Loc Value Value
  | Add Loc Value Value
  | Sub Loc Value Value
  | Icmp Loc String Value Value
  deriving (Eq, Ord, Read)

data BasicBlock = BasicBlock Label [Inst]

-- todo: str cmp, str add -> functions

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

type GenIrM a = WriterT [Inst] (StateT Int (Reader (SymTab, StrMap))) a

runGenIrM :: [Stmt Pos] -> Int -> SymTab -> StrMap -> [Inst]
runGenIrM stmts i symTab m = snd $ fst r where
  r = runReader (runStateT (runWriterT (generateIrStmts stmts)) i) (symTab, m)

generateIr :: Program Pos -> [[Inst]]
generateIr (Program _ topDefs) = map runGenIrTopDef topDefs
  where
    foldPutTopDefs globals (FnDef _ fRet fName _ _) =
      M.insert fName (emptyLoc, toIrType fRet) globals
    globals = foldl foldPutTopDefs (M.fromList builtins) topDefs
    strLit = "" : concatMap getStrLiteralsTopDef topDefs
    strLitLoc = map (\(s, i) -> (s, "@.str." ++ show i)) $ zip strLit [0..]
    strLitMap = M.fromList strLitLoc
    runGenIrTopDef (FnDef _ _ _ args (Block _ stmts)) =
      runGenIrM stmts (n - 1) locals strLitMap
      where
        (locals, n) = foldl insertArgs (globals, 0) args
        insertArgs (symTab, loc) (Arg _ aType ident) =
          (M.insert ident (getLocalLoc loc, toIrType aType) globals, loc + 1)

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

generateIrStmts (Decl _ dType items : stmts) = do
  generateIrStmts stmts

generateIrStmts (Ass _ ident e : stmts) = do  -- todo
  generateIrStmts stmts
--  symTab <- ask
--  let (loc, irType) = fromJust $ M.lookup ident symTab
--  val <- generateIrExp e
--  case val of
--    IntLit i ->
--    IntBool i ->
--    otherwise -> tell [AssInst loc val (IntLit 1)]
-- str x = "asdf";
-- y = x;
-- TODO: what if Lit?

generateIrStmts (Incr _ ident : stmts) = do
  (symTab, _) <- ask
  let (loc, irType) = fromJust $ M.lookup ident symTab
  tell [Add loc (Reg loc irType) (IntLit 1)]
  generateIrStmts stmts

generateIrStmts (Decr _ ident : stmts) = do
  (symTab, _) <- ask
  let (loc, irType) = fromJust $ M.lookup ident symTab
  tell [Sub loc (Reg loc irType) (IntLit 1)]
  generateIrStmts stmts

generateIrStmts (Ret _ e : stmts) = do
  val <- generateIrExp e
  tell [RetInst val]
  generateIrStmts stmts
generateIrStmts (VRet _ : stmts) = do
  tell [VRetInst]
  generateIrStmts stmts

generateIrStmts (Cond _ e stmt : stmts) = do
  generateIrStmts stmts

generateIrStmts (CondElse _ e stmtT stmtF : stmts) = do  -- TODO
  generateIrStmts stmts

generateIrStmts (While _ e stmt : stmts) = do  -- TODO
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

generateIrExp (EApp _ ident es) = undefined -- TODO

generateIrExp (EString _ str) = do
  (_, strLitMap) <- ask
  let loc = fromJust $ M.lookup str strLitMap
  tell [StrLit loc str]
  return $ Reg loc IrStr

generateIrExp (Neg _ e) = undefined -- TODO

generateIrExp (Not _ e) = undefined -- TODO

generateIrExp (EMul _ e1 op e2) = do
  val1 <- generateIrExp e1
  val2 <- generateIrExp e2
  n <- get
  put $ n + 1
  let loc = getLoc n
  tell [Mul loc val1 val2]
  case op of
    (Times _) -> tell [Mul loc val1 val2]
    (Div _) -> tell [SDiv loc val1 val2]
    (Mod _) -> tell [SRem loc val1 val2]
  return $ Reg loc $ getType val1

generateIrExp (EAdd _ e1 op e2) = do
  val1 <- generateIrExp e1
  val2 <- generateIrExp e2
  n <- get
  put $ n + 1
  let loc = getLoc n
  tell [Mul loc val1 val2]
  case op of
    (Plus _) -> case getType val1 of
      IrStr -> undefined  -- TODO: func app
      _ -> tell [Add loc val1 val2]
    (Minus _) -> tell [Sub loc val1 val2]
  return $ Reg loc $ getType val1

generateIrExp (ERel _ e1 op e2) = undefined -- TODO

generateIrExp (EAnd _ e1 e2) = undefined -- TODO
-- todo: lazy evalutaions

generateIrExp (EOr _ e1 e2) = undefined -- TODO
-- todo: lazy evalutaions


-- Utils

-- Returns next registry location.
getLoc :: Int -> Loc
getLoc n = '%' : show (n + 1)

getLocalLoc :: Int -> Loc
getLocalLoc n = "%l" ++ show n

toIrType :: TypePos -> IrType
toIrType (Int _) = IrInt
toIrType (Str _) = IrStr
toIrType (Bool _) = IrBool
toIrType (Void _) = IrVoid

getType :: Value -> IrType
getType (Reg _ t) = t
getType (IntLit _)  = IrInt
getType (BoolLit _) = IrBool
