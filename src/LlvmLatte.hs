module LlvmLatte where

import Data.List

type Loc = String
data Value = Reg Loc LlvmType | IntLit Integer | BoolLit Bool
  deriving (Eq, Ord, Read)
instance Show Value where
  show (Reg loc t) = unwords [show t, loc]
  show (IntLit i) = unwords [intType, show i]
  show (BoolLit True) = unwords [boolType, "1"]
  show (BoolLit False) = unwords [boolType, "0"]

type Label = String

data LlvmProg = LlvmProg [StrConst] [Extern] [LlvmDef]
  deriving (Eq, Ord, Read)
instance Show LlvmProg where
  show (LlvmProg strConsts externs defines) = unlines $
    map show strConsts ++ [""] ++ map show externs ++ [""] ++ map show defines

data StrConst = StrConst Loc String
  deriving (Eq, Ord, Read)
instance Show StrConst where
  show (StrConst sLoc s) = unwords [sLoc, "= internal constant", sType, sZ]
    where sType = "[" ++ show (length s + 1) ++ " x " ++ "i8" ++ "]"
          sZ = "c\"" ++ s ++ "\\00\""

data Extern = Extern LlvmType Loc [LlvmType]
  deriving (Eq, Ord, Read)
instance Show Extern where
  show (Extern rType name argTypes) = unwords ["declare", show rType, fun]
    where fun = "@" ++ name ++ "(" ++ unwordsSep (map show argTypes) ++ ")"

externs :: [Extern]
externs = [Extern LlvmVoid "error" [],
           Extern LlvmVoid "printInt" [LlvmInt],
           Extern LlvmVoid "printString" [LlvmStr],
           Extern LlvmInt "readInt" [],
           Extern LlvmStr "_concat" [LlvmStr, LlvmStr],
           Extern LlvmBool "_streq" [LlvmStr, LlvmStr],
           Extern LlvmStr "readString" []]

data LlvmDef = LlvmDef LlvmType Loc [Value] [LlvmInst]
  deriving (Eq, Ord, Read)
instance Show LlvmDef where
  show (LlvmDef rType name args insts) =
    unlines $ [defHead] ++ showInsts ++ ["}"]
    where showInsts = map show insts
          defHead = unwords ["define", show rType, showFun name args, "{"]

data LlvmInst
  = AssInst LlvmType Loc Loc  -- Internal inst eliminated after SSA; a := b
  | RetInst Value
  | VRetInst
  | Br Value Label Label
  | Goto Label
  | Lab Label
  | Call Loc LlvmType String [Value]
  | StrLit Loc String Loc
  | Mul Loc Value Value
  | SDiv Loc Value Value
  | SRem Loc Value Value
  | Add Loc Value Value
  | Sub Loc Value Value
  | Icmp Loc LlvmRelOp Value Value
  | Phi Loc LlvmType Loc Label Loc Label
  deriving (Eq, Ord, Read)
instance Show LlvmInst where
  show inst = unwords parts where
    parts = case inst of
      (AssInst t lLoc rLoc) -> [sep, lLoc, "=", show t, rLoc]
      (RetInst val) -> [sep, "ret", show val]
      VRetInst -> [sep, "ret void"]
      (Br val lT lF) ->
        [sep, "br", unwordsSep [show val, showLabel lT, showLabel lF]]
      (Goto l) -> [sep, "br", showLabel l]
      (Lab l) -> [tail l ++ ":"]
      (Call loc LlvmVoid name vals) ->
        [sep, "call", voidType, showFun name vals]
      (Call loc rType name vals) ->
        [sep, loc, "= call", show rType, showFun name vals]
      (StrLit loc s sLoc) -> [sep, loc, "=", showGetelementptrStr s sLoc]
      (Mul loc v1 v2) -> showBin "mul" loc v1 v2
      (SDiv loc v1 v2) -> showBin "sdiv" loc v1 v2
      (SRem loc v1 v2) -> showBin "srem" loc v1 v2
      (Add loc v1 v2) -> showBin "add" loc v1 v2
      (Sub loc v1 v2) -> showBin "sub" loc v1 v2
      (Icmp loc op v1 v2) ->
        [sep, loc, "= icmp", show op, show v1 ++ ",", showVal v2]
      (Phi loc t l1 lab1 l2 lab2) -> [sep, loc, "= phi", show t, phiLocLab]
        where phiLocLab = unwordsSep [showLocLab l1 lab1, showLocLab l2 lab2]
    showBin name loc v1 v2 = [sep, loc, "=", name, show v1 ++ ",", showVal v2]
    showLabel l = "label " ++ l
    showLocLab loc lab = "[" ++ loc ++ ", " ++ lab ++ "]"
    showGetelementptrStr s sLoc =
      "getelementptr " ++ unwordsSep [sSize, sSize ++ "* " ++ sLoc, zero, zero]
      where sSize = "[" ++ show (length s + 1) ++ " x " ++ "i8" ++ "]"
            zero = show $ IntLit 0
    showVal (Reg loc _) = loc
    showVal (IntLit i) = show i
    showVal (BoolLit True) = "1"
    showVal (BoolLit False) = "0"
    sep = "    "

showFun :: String -> [Value] -> String
showFun name vals = "@" ++ name ++ "(" ++ unwordsSep (map show vals) ++ ")"

data LlvmType = LlvmInt | LlvmStr | LlvmBool | LlvmVoid
  deriving (Eq, Ord, Read)
instance Show LlvmType where
  show LlvmVoid = voidType
  show LlvmBool = boolType
  show LlvmStr = strType
  show LlvmInt = intType

boolType = "i1"
strType = "i8*"
intType = "i32"
voidType = "void"

data LlvmRelOp = Lth | Le | Gth | Ge | Equ | Ne
  deriving (Eq, Ord, Read)
instance Show LlvmRelOp where
  show Lth = "slt"
  show Le = "sle"
  show Gth = "sgt"
  show Ge = "sge"
  show Equ = "eq"
  show Ne = "ne"

unwordsSep :: [String] -> String
unwordsSep = intercalate ", "
