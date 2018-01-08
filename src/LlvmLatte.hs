module LlvmLatte where

type Loc = String
data Value = Reg Loc LlvmType | IntLit Integer | BoolLit Bool
  deriving (Eq, Ord, Read)

type Label = String

data LlvmProg = LlvmProg [StrConst] [Extern] [LlvmDef]
  deriving (Eq, Ord, Read)

data StrConst = StrConst Loc String
  deriving (Eq, Ord, Read)

data Extern = Extern LlvmType Loc [LlvmType]
  deriving (Eq, Ord, Read)

externs :: [Extern]
externs = [Extern LlvmVoid "error" [],
           Extern LlvmVoid "printInt" [LlvmInt],
           Extern LlvmVoid "printString" [LlvmStr],
           Extern LlvmInt "readInt" [],
           Extern LlvmStr "_concat" [LlvmStr, LlvmStr],
           Extern LlvmBool "_streq" [LlvmStr, LlvmStr],
           Extern LlvmStr "readString" []]

data LlvmDef = LlvmDef LlvmType Loc [LlvmArg] [LlvmInst]
  deriving (Eq, Ord, Read)

data LlvmArg = LlvmArg LlvmType Loc
  deriving (Eq, Ord, Read)

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
  deriving (Eq, Ord, Read)

data LlvmType = LlvmInt | LlvmStr | LlvmBool | LlvmVoid
  deriving (Eq, Ord, Read)

data LlvmRelOp = Lth | Le | Gth | Ge | Equ | Ne
  deriving (Eq, Ord, Read)

data BasicBlock = BasicBlock Label [LlvmInst]
