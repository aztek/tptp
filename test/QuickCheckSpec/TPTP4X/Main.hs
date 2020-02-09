{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module       : Main
-- Description  : Test that tptp4X accepts pretty-printed randomly generated
--                TPTP and TSTP input.
-- Copyright    : (c) Evgenii Kotelnikov, 2020
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Main where

import Control.Monad (unless)
import System.Exit (ExitCode(..), exitFailure)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck (Property, Args(..), stdArgs, (===), whenFail,
                        ioProperty, forAllProperties, quickCheckWithResult)

import Data.TPTP
import Data.TPTP.Pretty

import Generators ()


-- * Helper functions

runTPTP4X :: String -> IO (ExitCode, String, String)
runTPTP4X = readProcessWithExitCode "test-data/tptp4X" ["-q3", "-"]


-- * Properties

tptp4X :: Pretty e => e -> Property
tptp4X e = ioProperty $ do
  (exitCode, stdOut, stdErr) <- runTPTP4X (show $ pretty e)
  return . whenFail (putStrLn ("tptp4X: " ++ stdOut) >>
                     putStrLn ("tptp4X: " ++ stdErr))
         $ exitCode === ExitSuccess

prop_tptp4X_TPTP :: TPTP -> Property
prop_tptp4X_TPTP = tptp4X

prop_tptp4X_TSTP :: TSTP -> Property
prop_tptp4X_TSTP = tptp4X


-- * Runner

return []

main :: IO ()
main = do
  success <- $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=10000}
  unless success exitFailure
