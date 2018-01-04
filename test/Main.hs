{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Control.Monad
import Test.Framework
import Test.Framework.BlackBoxTest

main :: IO ()
{-main = sequence [frontendTestsBB, llvmTestsBB] >>= htfMain-}
main = sequence [frontendTestsBB] >>= htfMain

frontendTestsBB :: IO TestSuite
frontendTestsBB =
   liftM (makeTestSuite "Frontend Black Box Tests") $
   blackBoxTests "test/tests" "./latc" ".lat" defaultBBTArgs

{-llvmTestsBB :: IO TestSuite-}
{-llvmTestsBB =-}
   {-liftM (makeTestSuite "LLVM Backend Black Box Tests") $-}
   {-blackBoxTests "test/tests" "lli" ".bc" args-}
  {-where-}
    {-args =-}
       {-defaultBBTArgs-}
       {-{bbtArgs_stdinSuffix = ".input", bbtArgs_stdoutSuffix = ".output"}-}
