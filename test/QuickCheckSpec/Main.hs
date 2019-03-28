{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Test.QuickCheck hiding (Function, function, Positive)

import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Data.TSTP
import Data.TSTP.Internal
import Data.TSTP.Parse.Combinators
import Data.TSTP.Pretty

import Generators ()

-- * Helper functions

-- | Idempotent parsing / pretty printing modulo normalization
ippModulo :: (Show a, Eq a, Pretty a) => (a -> a) -> Parser a -> a -> Property
ippModulo normalize p a =
  whenFail (print t) $ case parseOnly p t of
    Left err -> whenFail (putStrLn $ "Parsing error: " ++ err) False
    Right a' -> normalize a' === normalize a
  where
    t = renderStrict $ layoutPretty defaultLayoutOptions (pretty a)

-- | Idempotent parsing / pretty printing
ipp :: (Show a, Eq a, Pretty a) => Parser a -> a -> Property
ipp = ippModulo id

-- | 'reassociate' makes applications of associative connectives
-- left associative
reassociate :: FirstOrder s -> FirstOrder s
reassociate = \case
  Atomic l -> Atomic l
  Negated f -> Negated (reassociate f)
  Quantified q vs f -> Quantified q vs (reassociate f)
  Connected f c (Connected g c' h) | c == c' && isAssociative c ->
    reassociate (Connected (Connected f c g) c h)
  Connected f c g -> Connected (reassociate f) c (reassociate g)

normalizeFormula :: Formula -> Formula
normalizeFormula = \case
  CNF c  -> CNF c
  FOF uf -> FOF (reassociate uf)
  TFF sf -> TFF (reassociate sf)

normalizeSource :: Source -> Source
normalizeSource = \case
  Theory f i -> Theory f (fmap normalizeInfo i)
  Creator f i -> Creator f (fmap normalizeInfo i)
  Introduced i inf -> Introduced i (fmap normalizeInfo inf)
  Inference f i ps -> Inference f (normalizeInfo i) (fmap normalizeParent ps)
  Sources ss -> Sources (fmap normalizeSource ss)
  s -> s

normalizeParent :: Parent -> Parent
normalizeParent (Parent s gts) = Parent (normalizeSource s) (fmap normalizeGeneralTerm gts)

normalizeGeneralData :: GeneralData -> GeneralData
normalizeGeneralData = \case
  GeneralFunction f gts -> GeneralFunction f (fmap normalizeGeneralTerm gts)
  GeneralFormula f -> GeneralFormula (normalizeFormula f)
  GeneralBind v f -> GeneralBind v (normalizeFormula f)
  gd -> gd

normalizeGeneralTerm :: GeneralTerm -> GeneralTerm
normalizeGeneralTerm = \case
  GeneralData gd gt -> GeneralData (normalizeGeneralData gd) (fmap normalizeGeneralTerm gt)
  GeneralList gts -> GeneralList (fmap normalizeGeneralTerm gts)

normalizeInfo :: Info -> Info
normalizeInfo (Info gts) = Info (fmap normalizeGeneralTerm gts)

normalizeUnit :: Unit -> Unit
normalizeUnit (Unit n r f ann) = Unit n r (normalizeFormula f) (normalizeAnn ann)
  where
    normalizeAnn = fmap $ \(s, i) -> (normalizeSource s, fmap normalizeInfo i)

normalizeDerivation :: Derivation -> Derivation
normalizeDerivation (Derivation us) = Derivation (fmap normalizeUnit us)

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
prop_ipp_UnsortedFO = ippModulo reassociate (firstOrder unsorted)

prop_ipp_SortedFO :: SortedFirstOrder -> Property
prop_ipp_SortedFO = ippModulo reassociate (firstOrder sorted)

-- ** Formula annotations

prop_ipp_Parent :: Parent -> Property
prop_ipp_Parent = ippModulo normalizeParent parent

prop_ipp_GeneralData :: GeneralData -> Property
prop_ipp_GeneralData = ippModulo normalizeGeneralData generalData

prop_ipp_GeneralTerm :: GeneralTerm -> Property
prop_ipp_GeneralTerm = ippModulo normalizeGeneralTerm generalTerm

prop_ipp_Info :: Info -> Property
prop_ipp_Info = ippModulo normalizeInfo info

prop_ipp_Source :: Source -> Property
prop_ipp_Source = ippModulo normalizeSource source

-- ** Derivations

prop_ipp_Unit :: Unit -> Property
prop_ipp_Unit = ippModulo normalizeUnit unit

prop_ipp_Derivation :: Derivation -> Property
prop_ipp_Derivation = ippModulo normalizeDerivation derivation

-- * Runner

return []

main :: IO Bool
main = $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=1000}
