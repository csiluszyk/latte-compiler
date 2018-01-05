module TypeCheckLatte where

import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Maybe

import AbsLatte

type Pos = Maybe (Int, Int)
type TypePos = Type Pos

type SymTab = M.Map Ident TypePos


-- Consts

main :: Ident
main = Ident "main"

-- NOTE: As "return" cannot be used as identifier we'll be using it to store
-- information about current function return type in SymTab.
ret :: Ident
ret = Ident "return"

builtins :: [(Ident, TypePos)]
builtins = [(Ident "printInt",
             Fun (Just (0, 0)) (Void (Just (0, 0))) [Int (Just (0, 0))]),
            (Ident "printString",
             Fun (Just (0, 0)) (Void (Just (0, 0))) [Str (Just (0, 0))]),
            (Ident "error",
             Fun (Just (0, 0)) (Void (Just (0, 0))) []),
            (Ident "readInt",
             Fun (Just (0, 0)) (Int (Just (0, 0))) []),
            (Ident "readString",
             Fun (Just (0, 0)) (Str (Just (0, 0))) [])]


-- Type checking

typeCheck :: Program Pos -> Maybe String
typeCheck (Program pos topDefs) =
  let (globals, topDefsErrs) = typeCheckTopDefs topDefs
  in let defsErrs = typeCheckDefs topDefs globals
  in let errs = topDefsErrs ++ defsErrs
  in case errs of
    [] -> Nothing
    l -> Just $ intercalate "\n" ((show (length l) ++ " errors found:") : l)

typeCheckTopDefs :: [TopDef Pos] -> (SymTab, [String])
typeCheckTopDefs topDefs = (globals, mainErrs ++ reverse errs)
  where
    (globals, errs) = foldl foldTypeCheckFun (M.fromList builtins, []) topDefs
    mainErrs = case M.lookup main globals of
      Just (Fun _ (Int _) []) -> []
      Just (Fun pos _ _) ->
        [showPos pos ++ "function " ++ show main ++ " has wrong signature"]
      Nothing -> [notFoundErr (Just (0, 0)) "function" main]
    foldTypeCheckFun (globals, errs) (FnDef pos fRet fName fArgs _) =
      (newGlobals, voidErrs ++ argsErrs ++ topFunErrs ++ errs)
      where
        argsT = map (\(Arg _ aType _) -> aType) fArgs
        newGlobals = M.insert fName (Fun pos fRet argsT) globals
        topFunErrs
          | M.member fName globals = [alreadyDefinedErr pos fName]
          | otherwise = []
        argsErrs = snd $ foldl foldTypeCheckArgsIdents ([], []) fArgs
        foldTypeCheckArgsIdents (args, errs) (Arg aPos aType aIdent)
          | aIdent `elem` args = (args, alreadyDefinedErr aPos aIdent : errs)
          | otherwise = (aIdent : args, errs)
        voidErrs = foldl foldTypeCheckVoidArgs [] fArgs
        foldTypeCheckVoidArgs errs (Arg aPos (Void _) aIdent) =
          cannotBeVoidErr aPos aIdent : errs
        foldTypeCheckVoidArgs errs Arg {} = errs

typeCheckDefs :: [TopDef Pos] -> SymTab -> [String]
typeCheckDefs topDefs globals = foldr foldTypeCheckDef [] topDefs
  where
    foldTypeCheckDef (FnDef pos rType _ fArgs block@(Block _ stmts)) errs =
      typeCheckErrs ++ unreachErrs ++ isTerminatingErrs ++ errs
      where
        args = map (\(Arg _ aType ident) -> (ident, aType)) fArgs
        bScope = M.fromList $ (ret, rType) : args
        typeCheckErrs = typeCheckBlock block [bScope, globals]
        (isTerminating, unreachs) = typeCheckRetStmts stmts
        unreachErrs = map unreachErr unreachs
        isTerminatingErrs = case rType of
          Void _ -> []
          _ | isTerminating -> []
            | otherwise -> [showPos pos ++ "missing return statement"]


-- DFS AST to determine if function returns and which stmts are unreachable.
typeCheckRetStmts :: [Stmt Pos] -> (Bool, [Stmt Pos])
typeCheckRetStmts [] = (False, [])
typeCheckRetStmts (VRet {} : stmts) = (True, stmts)
typeCheckRetStmts (Ret {} : stmts) = (True, stmts)

typeCheckRetStmts (CondElse _ (ELitTrue _) stmtT stmtF : stmts)
  | tIsTerminating = (True, tUnreachs ++ fUnreachs ++ stmts)
  | otherwise = (isTerminating, tUnreachs ++ fUnreachs ++ unreachs)
  where (tIsTerminating, tUnreachs) = typeCheckRetStmts [stmtT]
        (_, fUnreachs) = typeCheckRetStmts [stmtF]
        (isTerminating, unreachs) = typeCheckRetStmts stmts
typeCheckRetStmts (CondElse _ (ELitFalse _) stmtT stmtF : stmts)
  | fIsTerminating = (True, tUnreachs ++ fUnreachs ++ stmts)
  | otherwise = (isTerminating, tUnreachs ++ fUnreachs ++ unreachs)
  where (_, tUnreachs) = typeCheckRetStmts [stmtT]
        (fIsTerminating, fUnreachs) = typeCheckRetStmts [stmtF]
        (isTerminating, unreachs) = typeCheckRetStmts stmts
typeCheckRetStmts (CondElse _ _ stmtT stmtF : stmts)
  | tIsTerminating && fIsTerminating = (True, tUnreachs ++ fUnreachs ++ stmts)
  | otherwise = (isTerminating, tUnreachs ++ fUnreachs ++ unreachs)
  where (tIsTerminating, tUnreachs) = typeCheckRetStmts [stmtT]
        (fIsTerminating, fUnreachs) = typeCheckRetStmts [stmtF]
        (isTerminating, unreachs) = typeCheckRetStmts stmts

typeCheckRetStmts (BStmt _ (Block _ bStmts) : stmts)
  | bIsTerminating = (True, bUnreachs ++ stmts)
  | otherwise = (isTerminating, bUnreachs ++ unreachs)
  where (bIsTerminating, bUnreachs) = typeCheckRetStmts bStmts
        (isTerminating, unreachs) = typeCheckRetStmts stmts

typeCheckRetStmts (Cond _ (ELitTrue _) stmt : stmts)
  | condIsTerminating = (True, condUnreachs ++ stmts)
  | otherwise = (isTerminating, condUnreachs ++ unreachs)
  where (condIsTerminating, condUnreachs) = typeCheckRetStmts [stmt]
        (isTerminating, unreachs) = typeCheckRetStmts stmts
typeCheckRetStmts (Cond _ _ stmt : stmts) = typeCheckRetCond stmt stmts

typeCheckRetStmts (While _ (ELitTrue _) stmt : stmts) =
  (condIsTerminating, condUnreachs ++ stmts)
  where (condIsTerminating, condUnreachs) = typeCheckRetStmts [stmt]
typeCheckRetStmts (While _ _ stmt : stmts) = typeCheckRetCond stmt stmts
typeCheckRetStmts (_ : stmts) = typeCheckRetStmts stmts

typeCheckRetCond :: Stmt Pos -> [Stmt Pos] -> (Bool, [Stmt Pos])
typeCheckRetCond stmt stmts = (isTerminating, condUnreachs ++ unreachs)
  where (_, condUnreachs) = typeCheckRetStmts [stmt]
        (isTerminating, unreachs) = typeCheckRetStmts stmts


-- [SymTabs] - list of variables declared in outer scopes;
--             last element - globals.
-- Returns type errors in given block.
typeCheckBlock :: Block Pos -> [SymTab] -> [String]
typeCheckBlock (Block _ stmts) symTabs =
  snd $ foldl foldTypeCheckStmt (symTabs, []) stmts
  where
    foldTypeCheckStmt (symTabs, errs) stmt = (newSymTabs, errs ++ newErrs)
      where
        (newSymTabs, newErrs) = typeCheckStmt stmt symTabs

-- Returns updated current scope and list of type errors in given stmt.
typeCheckStmt :: Stmt Pos -> [SymTab] -> ([SymTab], [String])

typeCheckStmt (Empty pos) symTabs = (symTabs, [])

typeCheckStmt (BStmt pos block) symTabs = (symTabs, errs)
  where errs = typeCheckBlock block (M.empty : symTabs)

typeCheckStmt (Decl pos (Void _) _) symTabs =
  (symTabs, [showPos pos ++ "variables cannot be type of void"])
typeCheckStmt (Decl pos dType items) symTabs@(scope : parents) =
  (newScope : parents, errs)
  where
    foldTypeCheckItems (currScope, currErrs) (NoInit currPos ident)
      | M.member ident currScope =
        (currScope, currErrs ++ [alreadyDefinedErr currPos ident])
      | otherwise = (M.insert ident dType currScope, currErrs)
    foldTypeCheckItems (currScope, currErrs) (Init currPos ident expr)
      | M.member ident currScope =
        (currScope, currErrs ++ [alreadyDefinedErr currPos ident])
      | otherwise = case typeCheckExpr expr (currScope : parents) of
        Right t ->
          if cmpTypes dType t then (newCurrScope, currErrs)
          else (newCurrScope, currErrs ++ [nonMatchingTypesErr currPos])
        Left l -> (newCurrScope, currErrs ++ l)
      where newCurrScope = M.insert ident dType currScope
    (newScope, errs) = foldl foldTypeCheckItems (scope, []) items

typeCheckStmt (Ass pos ident expr) symTabs = case getType ident symTabs of
  Just t1 -> case typeCheckExpr expr symTabs of
    Right t2 -> if cmpTypes t1 t2 then (symTabs, [])
                else (symTabs, [nonMatchingTypesErr pos])
    Left l -> (symTabs, l)
  Nothing -> (symTabs, [notFoundErr pos "variable" ident])

typeCheckStmt (Incr pos ident) symTabs = case getType ident symTabs of
  Just (Int _) -> (symTabs, [])
  Just _ -> (symTabs, [showPos pos ++ "only int variables can be incremented"])
  Nothing -> (symTabs, [notFoundErr pos "variable" ident])

typeCheckStmt (Decr pos ident) symTabs = case getType ident symTabs of
  Just (Int _) -> (symTabs, [])
  Just _ -> (symTabs, [showPos pos ++ "only int variables can be decremented"])
  Nothing -> (symTabs, [notFoundErr pos "variable" ident])

typeCheckStmt (Ret pos expr) symTabs =
  let retT = fromJust $ getType ret symTabs
  in case retT of
    Void _ -> (symTabs, [showPos pos ++
      "cannot result a value from a method with void result type"])
    _ -> case typeCheckExpr expr symTabs of
      Right t ->
        if cmpTypes retT t then (symTabs, [])
        else (symTabs, [showPos pos ++ "incompatible return type"])
      Left l -> (symTabs, l)

typeCheckStmt (VRet pos) symTabs = case fromJust $ getType ret symTabs of
  Void _ -> (symTabs, [])
  _ -> (symTabs, [showPos pos ++ "missing return value"])

typeCheckStmt (Cond pos expr stmt) symTabs =
  let (_, stmtErrs) = typeCheckStmt stmt (M.empty : symTabs)
  in typeCheckCondStmt pos expr symTabs stmtErrs

typeCheckStmt (CondElse pos expr stmtT stmtF) symTabs =
  let (_, stmtTErrs) = typeCheckStmt stmtT (M.empty : symTabs)
      (_, stmtFErrs) = typeCheckStmt stmtF (M.empty : symTabs)
  in let stmtErrs = stmtTErrs ++ stmtFErrs
  in typeCheckCondStmt pos expr symTabs stmtErrs

typeCheckStmt (While pos expr stmt) symTabs =
  let (_, stmtErrs) = typeCheckStmt stmt (M.empty : symTabs)
  in typeCheckCondStmt pos expr symTabs stmtErrs

typeCheckStmt (SExp pos expr) symTabs = case typeCheckExpr expr symTabs of
  Right _ -> (symTabs, [])
  Left l -> (symTabs, l)

typeCheckCondStmt ::
  Pos -> Expr Pos -> [SymTab] -> [String] -> ([SymTab], [String])
typeCheckCondStmt pos expr symTabs stmtErrs =
  case typeCheckExpr expr symTabs of
    Right (Bool _) -> (symTabs, stmtErrs)
    Right _ ->
      (symTabs, (showPos pos ++ "condition must type of bool") : stmtErrs)
    Left l -> (symTabs, l ++ stmtErrs)


-- [SymTabs] - list of variables declared in outer scopes;
--             last element - globals.
-- If given Expr is type correct returns its type and error(s) otherwise.
typeCheckExpr :: Expr Pos -> [SymTab] -> Either [String] TypePos

typeCheckExpr (EVar pos ident) symTabs = case getType ident symTabs of
  Just Fun {} -> Left [showPos pos ++ show ident ++ "is not a variable"]
  Just t -> Right t
  Nothing -> Left [notFoundErr pos "variable" ident]

typeCheckExpr (ELitInt pos i) symTabs
  | i < fromIntegral (minBound :: Int32) =
    Left [showPos pos ++ "integer number too small"]
  | i > fromIntegral (maxBound :: Int32) =
    Left [showPos pos ++ "integer number too large"]
  | otherwise = Right $ Int pos

typeCheckExpr (ELitTrue pos) symTabs = Right $ Bool pos

typeCheckExpr (ELitFalse pos) symTabs = Right $ Bool pos

typeCheckExpr (EApp pos ident exprs) symTabs = case getType ident symTabs of
  Just (Fun _ rType types)
    | length types > length exprs ->
      Left [showPos pos ++ "too few argumetns passed"]
    | length types < length exprs ->
      Left [showPos pos ++ "too many argumetns passed"]
    | otherwise -> case foldr foldCmpTypes [] (zip types exprs) of
      [] -> Right rType
      errs -> Left errs
    where foldCmpTypes (t1, expr) errs =
            case typeCheckExpr expr symTabs of
              Right t2 -> if cmpTypes t1 t2 then errs
                          else nonMatchingTypesErr pos : errs
              Left l -> l ++ errs
  Just t -> Left [showPos pos ++ show ident ++ "is not a function"]
  Nothing -> Left [notFoundErr pos "function" ident]

typeCheckExpr (EString pos _) symTabs = Right $ Str pos

typeCheckExpr (Neg pos expr) symTabs = case typeCheckExpr expr symTabs of
  Right (Int _) -> Right $ Int pos
  Right _ -> Left [showPos pos ++ "only int expressions can be negated"]
  Left l -> Left l

typeCheckExpr (Not pos expr) symTabs = case typeCheckExpr expr symTabs of
  Right (Bool _) -> Right $ Bool pos
  Right _ -> Left [showPos pos ++ "only bool expressions can be notted"]
  Left l -> Left l

typeCheckExpr (EMul pos expr1 _ expr2) symTabs =
  typeCheckIntExprs pos expr1 expr2 symTabs

typeCheckExpr (EAdd pos expr1 (Minus _) expr2) symTabs =
  typeCheckIntExprs pos expr1 expr2 symTabs
typeCheckExpr (EAdd pos expr1 (Plus _) expr2) symTabs =
  case (typeCheckExpr expr1 symTabs, typeCheckExpr expr2 symTabs) of
    (Right (Int _), Right (Int _)) -> Right $ Int pos
    (Right (Int _), Right _) -> Left [isNotErr pos "second" "int"]
    (Right (Int _), Left s2) -> Left s2

    (Right (Str _), Right (Str _)) -> Right $ Str pos
    (Right (Str _), Right _) -> Left [isNotErr pos "second" "str"]
    (Right (Str _), Left s2) -> Left s2

    (Right _, Right (Int _)) -> Left [isNotErr pos "first" "int"]
    (Right _, Right (Str _)) -> Left [isNotErr pos "first" "str"]
    (Right _, Right _) -> Left [isNotErr pos "both" "int/str"]
    (Right _, Left s2) -> Left $ isNotErr pos "first" "int/str" : s2

    (Left s1, Right (Int _)) -> Left s1
    (Left s1, Right (Str _)) -> Left s1
    (Left s1, Right _) -> Left $ s1 ++ [isNotErr pos "second" "int/str"]
    (Left s1, Left s2) -> Left $ s1 ++ s2

typeCheckExpr (ERel pos expr1 (EQU _) expr2) symTabs =
  typeCheckTypeExprs pos expr1 expr2 symTabs
typeCheckExpr (ERel pos expr1 (NE _) expr2) symTabs =
  typeCheckTypeExprs pos expr1 expr2 symTabs
typeCheckExpr (ERel pos expr1 _ expr2) symTabs =
  case (typeCheckExpr expr1 symTabs, typeCheckExpr expr2 symTabs) of
    (Right (Int _), Right (Int _)) -> Right $ Bool pos
    (Right (Int _), Right _) -> Left [isNotErr pos "second" "int"]
    (Right (Int _), Left s2) -> Left s2

    (Right _, Right (Int _)) -> Left [isNotErr pos "first" "int"]
    (Right _, Right _) -> Left [isNotErr pos "both" "int"]
    (Right _, Left s2) -> Left $ isNotErr pos "first" "int" : s2

    (Left s1, Right (Int _)) -> Left s1
    (Left s1, Right _) -> Left $ s1 ++ [isNotErr pos "second" "int"]
    (Left s1, Left s2) -> Left $ s1 ++ s2

typeCheckExpr (EAnd pos expr1 expr2) symTabs =
  typeCheckBoolExprs pos expr1 expr2 symTabs

typeCheckExpr (EOr pos expr1 expr2) symTabs =
  typeCheckBoolExprs pos expr1 expr2 symTabs

typeCheckIntExprs::
  Pos -> Expr Pos -> Expr Pos -> [SymTab] -> Either [String] TypePos
typeCheckIntExprs pos expr1 expr2 symTabs =
  case (typeCheckExpr expr1 symTabs, typeCheckExpr expr2 symTabs) of
    (Right (Int _), Right (Int _)) -> Right $ Int pos
    (Right (Int _), Right _) -> Left [isNotErr pos "second" "int"]
    (Right (Int _), Left s2) -> Left s2

    (Right _, Right (Int _)) -> Left [isNotErr pos "first" "int"]
    (Right _, Right _) -> Left [isNotErr pos "both" "int"]
    (Right _, Left s2) -> Left $ isNotErr pos "first" "int" : s2

    (Left s1, Right (Int _)) -> Left s1
    (Left s1, Right _) -> Left $ s1 ++ [isNotErr pos "second" "int"]
    (Left s1, Left s2) -> Left $ s1 ++ s2

typeCheckBoolExprs::
  Pos -> Expr Pos -> Expr Pos -> [SymTab] -> Either [String] TypePos
typeCheckBoolExprs pos expr1 expr2 symTabs =
  case (typeCheckExpr expr1 symTabs, typeCheckExpr expr2 symTabs) of
    (Right (Bool _), Right (Bool _)) -> Right $ Bool pos
    (Right (Bool _), Right _) -> Left [isNotErr pos "second" "bool"]
    (Right (Bool _), Left s2) -> Left s2

    (Right _, Right (Bool _)) -> Left [isNotErr pos "first" "bool"]
    (Right _, Right _) -> Left [isNotErr pos "both" "bool"]
    (Right _, Left s2) -> Left $ isNotErr pos "first" "bool" : s2

    (Left s1, Right (Bool _)) -> Left s1
    (Left s1, Right _) -> Left $ s1 ++ [isNotErr pos "second" "bool"]
    (Left s1, Left s2) -> Left $ s1 ++ s2

typeCheckTypeExprs::
  Pos -> Expr Pos -> Expr Pos -> [SymTab] -> Either [String] TypePos
typeCheckTypeExprs pos expr1 expr2 symTabs =
  case (typeCheckExpr expr1 symTabs, typeCheckExpr expr2 symTabs) of
    (Right t1, Right t2) | cmpTypes t1 t2 -> Right $ Bool pos
                         | otherwise -> Left [nonMatchingTypesErr pos]
    (Right _, Left s2) -> Left s2
    (Left s1, Right _) -> Left s1
    (Left s1, Left s2) -> Left $ s1 ++ s2


-- Utils

showPos :: Pos -> String
showPos pos = show (fromJust pos) ++ ": "

-- Traverses symTabs and looking for a given ident.
getType :: Ident -> [SymTab] -> Maybe TypePos
getType _ [] = Nothing
getType ident (symTab : symTabs) = case M.lookup ident symTab of
  Just t -> Just t
  Nothing -> getType ident symTabs

-- Checks if given types are equal (excluding internal Fun type).
cmpTypes :: TypePos -> TypePos -> Bool
cmpTypes (Int _) (Int _) = True
cmpTypes (Bool _) (Bool _) = True
cmpTypes (Str _) (Str _) = True
cmpTypes (Void _) (Void _) = True
cmpTypes _ _ = False

getPosStmt :: Stmt Pos -> Pos
getPosStmt (Empty pos) = pos
getPosStmt (BStmt pos _) = pos
getPosStmt (Decl pos _ _) = pos
getPosStmt (Ass pos _ _) = pos
getPosStmt (Incr pos  _) = pos
getPosStmt (Decr pos  _) = pos
getPosStmt (Ret pos _) = pos
getPosStmt (VRet pos) = pos
getPosStmt (Cond pos _ _) = pos
getPosStmt (CondElse pos _ _ _) = pos
getPosStmt (While pos _ _) = pos
getPosStmt (SExp pos _) = pos


-- Errors

alreadyDefinedErr :: Pos -> Ident -> String
alreadyDefinedErr pos ident = showPos pos ++ show ident ++ " already defined"

cannotBeVoidErr :: Pos -> Ident -> String
cannotBeVoidErr pos ident =
  showPos pos ++ show ident ++ " cannot be of type void"

notFoundErr :: Pos -> String -> Ident -> String
notFoundErr pos typeS ident =
  showPos pos ++ typeS ++ " " ++ show ident ++ " not found"

isNotErr :: Pos -> String -> String -> String
isNotErr pos posS typeS =
  showPos pos ++ posS ++ " expression is not type of " ++ typeS

nonMatchingTypesErr :: Pos -> String
nonMatchingTypesErr pos = showPos pos ++ "types don't match"

unreachErr :: Stmt Pos -> String
unreachErr stmt = showPos (getPosStmt stmt) ++ "is unreachable"
