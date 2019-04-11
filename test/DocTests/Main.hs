module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "--fast", "src/Data/TPTP.hs"]
