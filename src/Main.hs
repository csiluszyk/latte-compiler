import System.Environment (getArgs, getProgName)
import System.IO (
  withFile, hGetContents, Handle, IOMode (ReadMode), stdin, stderr, hPutStrLn
  )
import System.FilePath (takeBaseName, takeDirectory)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Process (system)
import Paths_latte

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
    [filePath] -> withFile filePath ReadMode $ compile dirName fBase
      where fBase = takeBaseName filePath
            dirName = takeDirectory filePath
    _ -> usage

usage :: IO ()
usage = getProgName >>= (\s -> putStrLn $ "Usage: " ++ s ++ " FILENAME")

compile :: String -> String -> Handle -> IO ()
compile dirName fBase hFile = do
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
          let outBase = dirName ++ "/" ++ fBase ++ "."
              outLlPath = outBase ++ "ll"
              outBcPath = outBase ++ "bc"

          writeFile outLlPath $ show (toSsa $ generateLlvm prog)

          runtimePath <- getDataFileName "runtime.bc"
          let llvmAs = unwords ["llvm-as -o", outBcPath, outLlPath]
          putStrLn $ "$ " ++ llvmAs ++ "\n"
          rcAs <- system llvmAs

          case rcAs of
            ExitSuccess -> do
              let llvmLink =
                    unwords ["llvm-link -o", outBcPath, outBcPath, runtimePath]
              putStrLn $ "$ " ++ llvmLink
              rcLink <- system llvmLink
              exitWith rcLink
            _ -> exitWith rcAs
