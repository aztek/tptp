{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck hiding (Function, function)

import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Data.TSTP
import Data.TSTP.Parse.Combinators
import Data.TSTP.Pretty

import Generators ()

-- * Helper functions

-- | Idempotent parsing / pretty printing
ipp :: (Show a, Eq a, Pretty a) => Parser a -> a -> Property
ipp p a =
  whenFail (print t) $ case parseOnly p t of
    Left err -> whenFail (putStrLn $ "Parsing error: " ++ err) False
    Right a' -> a' === a
  where
    t = renderStrict $ layoutPretty defaultLayoutOptions (pretty a)

-- * Properties

-- ** Names

prop_ipp_Atom :: Atom -> Property
prop_ipp_Atom = ipp atom

prop_ipp_Var :: Var -> Property
prop_ipp_Var = ipp var

prop_ipp_Function :: Name Function -> Property
prop_ipp_Function = ipp function

prop_ipp_Predicate :: Name Predicate -> Property
prop_ipp_Predicate = ipp predicate

-- ** Sorts and types

prop_ipp_Sort :: Name Sort -> Property
prop_ipp_Sort = ipp sort

prop_ipp_Type :: Type -> Property
prop_ipp_Type = ipp type_

-- ** First-order logic

prop_ipp_Term :: Term -> Property
prop_ipp_Term = ipp term

prop_ipp_Literal :: Literal -> Property
prop_ipp_Literal = ipp literal

prop_ipp_Clause :: Clause -> Property
prop_ipp_Clause = ipp clause

prop_ipp_UnsortedFO :: UnsortedFirstOrder -> Property
prop_ipp_UnsortedFO = ipp (firstOrder unsorted)

prop_ipp_SortedFO :: SortedFirstOrder -> Property
prop_ipp_SortedFO = ipp (firstOrder sorted)

-- ** Formula annotations

prop_ipp_Parent :: Parent -> Property
prop_ipp_Parent = ipp parent

prop_ipp_GeneralData :: GeneralData -> Property
prop_ipp_GeneralData = ipp generalData

prop_ipp_GeneralTerm :: GeneralTerm -> Property
prop_ipp_GeneralTerm = ipp generalTerm

prop_ipp_Info :: Info -> Property
prop_ipp_Info = ipp info

prop_ipp_Source :: Source -> Property
prop_ipp_Source = ipp source

-- ** Derivations

prop_ipp_Unit :: Unit -> Property
prop_ipp_Unit = ipp unit

prop_ipp_Derivation :: Derivation -> Property
prop_ipp_Derivation = ipp derivation

-- * Runner

return []

main :: IO Bool
main = $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=1000}
