module TypeCheckLatte where

import Control.Monad
import Control.Monad.Error
import qualified Data.Map as M
import Data.List
import Control.Monad.State

import AbsLatte
import UtilsLatte

type SymTab = [M.Map Ident (Type (Maybe (Int, Int)))]

main :: Ident
main = Ident "main"

builtins :: [(Ident, Type (Maybe (Int, Int)))]
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

typeCheck :: Program (Maybe (Int, Int)) -> Maybe String
typeCheck (Program pos topDefs) = case typeCheckTopDefs topDefs of
  [] -> Nothing
  l -> Just $ intercalate "\n" l

typeCheckTopDefs :: [TopDef (Maybe (Int, Int))] -> [String]
typeCheckTopDefs topDefs = mainErrs ++ reverse errors
  where
    (globals, errors) = foldl checkFunName (M.fromList builtins, []) topDefs
    mainErrs = case M.lookup main globals of
      Just (Fun _ (Int _) []) -> []
      Just (Fun pos _ _) ->
        [showPos pos ++ ": Function " ++ show main ++ " has wrong signature."]
      Nothing -> ["(0, 0): Function " ++ show main ++ " does not exist."]
    checkFunName (globals, errors) (FnDef pos fRet fName fArgs _)
      | M.member fName globals = (globals, err : errors)
      | otherwise = (M.insert fName (Fun pos fRet args) globals, errors)
      where
        err = showPos pos ++ ": Function " ++ show fName ++ " already exists."
        args = map (\(Arg _ aType _) -> aType) fArgs
