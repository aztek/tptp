{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import qualified Data.Text.IO as Text.IO
import Data.TPTP.Parse.Text

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM p (x:xs) = do
  q <- p x
  if q then allM p xs else return False

parseFile :: FilePath -> IO Bool
parseFile fp = do
  putStr $ fp ++ "\t"
  input <- Text.IO.readFile fp
  case parseDerivationOnly input of
    Left e  -> putStrLn "FAIL" >> putStrLn ("Error: " ++ e) >> return False
    Right _ -> putStrLn "OK" >> return True

parseDirectory :: FilePath -> IO Bool
parseDirectory dp = do
  putStrLn $ dp ++ ":"
  files <- listDirectory dp
  allM parseFile [dp ++ "/" ++ f | f <- files] <* putStrLn ""

subdirectories :: FilePath -> IO [FilePath]
subdirectories dp = do
  files <- listDirectory dp
  return [dp ++ "/" ++ f | f <- files]

main :: IO Bool
main = do
  dirs <- subdirectories "test-data/tptp"
  allM parseDirectory dirs
