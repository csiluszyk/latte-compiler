import System.Environment (getArgs, getProgName)
import System.IO (
  withFile, hGetContents, Handle, IOMode (ReadMode), stdin, stderr, hPutStrLn
  )
import System.Exit (exitFailure, exitSuccess)

import ErrM
import LexLatte
import ParLatte
import AbsLatte

import TypeCheckLatte (typeCheck)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> compile stdin  -- TODO
    [filename] -> withFile filename ReadMode compile
    _ -> usage

usage :: IO ()
usage = getProgName >>= (\s -> putStrLn $ "Usage: " ++ s ++ " FILENAME")

compile :: Handle -> IO ()
compile hFile = do
  fileContent <- hGetContents hFile
  case pProgram (tokens fileContent) of
    Bad s -> do
      hPutStrLn stderr "BAD"
      hPutStrLn stderr "Parse failed..."
      hPutStrLn stderr s
      exitFailure
    Ok prog ->
      case typeCheck prog of
        Left err -> do
          hPutStrLn stderr "BAD"
          hPutStrLn stderr err
          exitFailure
        Right symTab -> do
          -- result :: [Value]
          -- print symTab
          exitSuccess
