{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- |
-- Module       : UnitTests
-- Description  : Run the TPTP parser on selected examples from the TPTP World.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module UnitTests (tests) where

import Distribution.TestSuite (Test(..), TestInstance(..),
                               Progress(..), Result(..))

import System.Directory (listDirectory)

#if !MIN_VERSION_base(4, 8, 0)
import Data.Functor ((<$>))
#endif

import Data.Text (Text)
import qualified Data.Text.IO as Text.IO (readFile)
import Data.List (intercalate)
import Control.Monad.Extra (concatMapM)

import Data.TPTP.Parse.Text

testDataDir :: FilePath
testDataDir = "test-data"

listTestDirectory :: FilePath -> IO [FilePath]
listTestDirectory d = listDirectory (testDataDir ++ "/" ++ d)

readTestFile :: FilePath -> IO Text
readTestFile f = Text.IO.readFile (testDataDir ++ "/" ++ f)

parseFile :: FilePath -> IO Result
parseFile path = buildResult . parseTPTPOnly <$> readTestFile path
  where
    buildResult (Left e)  = Error e
    buildResult (Right _) = Pass

type TestCase = (FilePath, FilePath, FilePath)

testFile :: TestCase -> Test
testFile (space, lang, file) = Test $ TestInstance {
  run = Finished <$> parseFile path,
  name = path,
  tags = [space, lang],
  options = [],
  setOption = const . const $ Left "not supported"
} where path = intercalate "/" [space, lang, file]

listSpaces :: IO [FilePath]
listSpaces = listTestDirectory ""

listLangs :: FilePath -> IO [(FilePath, FilePath)]
listLangs s = fmap (s,) <$> listTestDirectory s

listFiles :: (FilePath, FilePath) -> IO [(FilePath, FilePath, FilePath)]
listFiles (s, l) = fmap (s, l,) <$> listTestDirectory (s ++ "/" ++ l)

cases :: IO [TestCase]
cases = listSpaces >>= concatMapM listLangs >>= concatMapM listFiles

tests :: IO [Test]
tests =  fmap testFile <$> cases
