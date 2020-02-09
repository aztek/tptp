{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Monad (filterM)
import Control.Monad.Extra (concatMapM)
#if !MIN_VERSION_base(4, 8, 0)
import Data.Functor ((<$>))
#endif
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO (readFile)

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath.Posix (joinPath, (</>))

import Distribution.TestSuite (Test(..), TestInstance(..),
                               Progress(..), Result(..))

import Data.TPTP (TPTP(..), TSTP(..), SZS(..))
import Data.TPTP.Parse.Text (parseTPTPOnly, parseTSTPOnly)


testDataDir :: FilePath
testDataDir = "test-data"

listTestDirectory :: FilePath -> IO [FilePath]
listTestDirectory d = listDirectory (testDataDir </> d)

readTestFile :: FilePath -> IO Text
readTestFile f = Text.IO.readFile (testDataDir </> f)

testParsingTPTP :: FilePath -> IO Result
testParsingTPTP path = buildResult . parseTPTPOnly <$> readTestFile path
  where
    buildResult = \case
      Left e -> Error e
      Right (TPTP []) -> Error "empty list of parsed units"
      Right _ -> Pass

testParsingTSTP :: FilePath -> IO Result
testParsingTSTP path = buildResult . parseTSTPOnly <$> readTestFile path
  where
    buildResult = \case
      Left e -> Error e
      Right (TSTP _ []) -> Error "empty list of parsed units"
      Right (TSTP (SZS (Just _) (Just _)) _) -> Pass
      Right (TSTP _ _) -> Error "failed to parse SZS ontology"

type TestCase = (FilePath, FilePath, FilePath)

testCasePath :: TestCase -> FilePath
testCasePath (space, lang, file) = joinPath [space, lang, file]

runTestCase :: (FilePath, FilePath, FilePath) -> IO Result
runTestCase testCase@(space, _, _) = test (testCasePath testCase)
  where
    test = case space of
      "szs" -> testParsingTSTP
      _     -> testParsingTPTP

testFile :: TestCase -> Test
testFile testCase@(space, lang, _) = Test $ TestInstance {
  run       = Finished <$> runTestCase testCase,
  name      = testCasePath testCase,
  tags      = [space, lang],
  options   = [],
  setOption = const . const $ Left "not supported"
}

listSpaces :: IO [FilePath]
listSpaces = listDirectory testDataDir >>= filterM doesDirectoryExist

listLangs :: FilePath -> IO [(FilePath, FilePath)]
listLangs s = fmap (s,) <$> (listTestDirectory s >>= filterM doesDirectoryExist)

listFiles :: (FilePath, FilePath) -> IO [(FilePath, FilePath, FilePath)]
listFiles (s, l) = fmap (s, l,) <$> listTestDirectory (s </> l)

cases :: IO [TestCase]
cases = listSpaces >>= concatMapM listLangs >>= concatMapM listFiles

tests :: IO [Test]
tests = fmap testFile <$> cases
