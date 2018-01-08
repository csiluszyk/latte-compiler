{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Control.Applicative
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import Test.Framework
import Test.Framework.BlackBoxTest
import Test.Framework.TestManager

main :: IO ()
main = do
  args <- getArgs
  ft <- frontendTestsBB
  (printSummary, ecode) <- runTestWithArgs' args ft
--  bt <- llvmTestsBB
--  (printSummary', ecode') <- runTestWithArgs' args bt
  printSummary
--  printSummary'
--  case ecode of
--    ExitSuccess -> exitWith ecode'
--    _ -> exitWith ecode
  exitWith ecode  -- TODO

frontendTestsBB :: IO TestSuite
frontendTestsBB = makeTestSuite "Frontend Black Box Tests" <$>
  blackBoxTests "test/tests" "./latc_llvm" ".lat" defaultBBTArgs

llvmTestsBB :: IO TestSuite
llvmTestsBB = makeTestSuite "LLVM Backend Black Box Tests" <$>
  blackBoxTests "test/tests" "lli" ".bc" args
  where args = defaultBBTArgs {
    bbtArgs_stdinSuffix = ".input",
    bbtArgs_stdoutSuffix = ".output"
    }