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
import GenLatte (generateLlvm)
import SsaLatte (toSsa)

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
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr "Parse failed..."
      hPutStrLn stderr s
      exitFailure
    Ok prog ->
      case typeCheck prog of
        Just err -> do
          hPutStrLn stderr "ERROR"
          hPutStrLn stderr err
          exitFailure
        Nothing -> do
          hPutStrLn stderr "OK"
          print $ toSsa $ generateLlvm prog
          exitSuccess
