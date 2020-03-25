{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

-- |
-- Module       : Main
-- Description  : QuickCheck specification for the tptp library.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--
-- Defines properties of the tptp library and runs QuickCheck on them.
--

module Main where

#if MIN_VERSION_base(4, 8, 0)
import Prelude hiding ((<*))
#endif

import Control.Applicative ((<*))
import Control.Monad (unless)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL (cons)
import Data.Text (Text)

import Data.Attoparsec.Text (Parser, parseOnly, endOfInput)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Test.QuickCheck (Gen, Property, Args(..), stdArgs, (===), whenFail,
                        forAll, forAllProperties, quickCheckWithResult)
import Test.QuickCheck.Poly (A)

import System.Exit (exitFailure)

import Data.TPTP hiding (clause)
import Data.TPTP.Parse.Combinators
import Data.TPTP.Pretty

import ArbitrarilyPretty
import Generators ()
import Normalizers


-- * Helper functions

-- | Generalized idempotent parsing / pretty printing.
ippWith :: (Show e, Show e', Eq e')
        => (e -> Gen (Doc a))  -- ^ Pretty printer.
        -> (Doc a -> Gen Text) -- ^ Renderer.
        -> Parser e            -- ^ Parser.
        -> (e -> e')           -- ^ Normalizer.
        -> e -> Property
ippWith pprint render parse normalize expr =
  forAll (pprint expr) $ \doc  ->
  forAll (render doc)  $ \text ->
    case parseOnly (parse <* endOfInput) text of
      Left err    -> whenFail (putStrLn $ "Parsing error: " ++ err) False
      Right expr' -> normalize expr' === normalize expr

defaultRender :: Doc a -> Gen Text
defaultRender = return . renderStrict . layoutPretty defaultLayoutOptions

defaultNormalize :: e -> e
defaultNormalize = id

-- | Idempotent parsing / pretty printing modulo normalization.
ippModulo :: (Show e, Eq e, Pretty e) => (e -> e) -> Parser e -> e -> Property
ippModulo = flip $ ippWith (return . pretty) defaultRender

-- | Idempotent parsing / pretty printing.
ipp :: (Show e, Eq e, Pretty e) => Parser e -> e -> Property
ipp = ippModulo defaultNormalize

-- | Idempotent arbitrary parsing / pretty printing modulo normalization.
aippModulo :: (Show e, Eq e, ArbitrarilyPretty e, Show e', Eq e')
           => (e -> e') -> Parser e -> e -> Property
aippModulo = flip $ ippWith apretty defaultRender

-- | Idempotent parsing / pretty printing.
aipp :: (Show e, Eq e, ArbitrarilyPretty e) => Parser e -> e -> Property
aipp = aippModulo defaultNormalize

-- | Idempotent parsing / pretty printing with superfluous parenthesis
-- modulo normalization.
spAippModulo :: (Show e, Eq e, ArbitrarilyPretty (SuperfluousParenthesis e))
             => (e -> e) -> Parser e -> e -> Property
spAippModulo normalize parser expr =
  aippModulo (normalize . unSuperfluousParenthesis)
             (fmap SuperfluousParenthesis parser)
             (SuperfluousParenthesis expr)

-- | Idempotent parsing / pretty printing with superfluous parenthesis.
spAipp :: (Show e, Eq e, ArbitrarilyPretty (SuperfluousParenthesis e))
       => Parser e -> e -> Property
spAipp = spAippModulo defaultNormalize


-- * Properties

-- ** Generators

-- *** Well-formed names

prop_validAtom :: Atom -> Bool
prop_validAtom (Atom t) = isValidAtom t

prop_validVar :: Var -> Bool
prop_validVar (Var t) = isValidVar t

prop_validDistinctObject :: DistinctObject -> Bool
prop_validDistinctObject (DistinctObject t) = isValidDistinctObject t

-- *** Auxiliary data structures

prop_randomSplit :: NonEmpty A -> Property
prop_randomSplit list = forAll (randomSplit list)
                      $ \(prefix, suffix) -> appendr prefix suffix === list
  where
    appendr l nel = foldr NEL.cons nel l

prop_randomTree :: NonEmpty A -> Property
prop_randomTree list = forAll (randomTree list)
                     $ \tree -> toList tree === toList list


-- ** Names

prop_ipp_Atom :: Atom -> Property
prop_ipp_Atom = ipp atom

prop_ipp_Var :: Var -> Property
prop_ipp_Var = ipp var

prop_ipp_DistinctObject :: DistinctObject -> Property
prop_ipp_DistinctObject = ipp distinctObject

prop_ipp_Function :: Name Function -> Property
prop_ipp_Function = ipp function

prop_ipp_Predicate :: Name Predicate -> Property
prop_ipp_Predicate = ipp predicate


-- ** Sorts and types

prop_ipp_Sort :: Name Sort -> Property
prop_ipp_Sort = ipp sort

prop_ipp_TFF1Sort :: TFF1Sort -> Property
prop_ipp_TFF1Sort = ipp tff1Sort

prop_ipp_Type :: Type -> Property
prop_ipp_Type = ippModulo normalizeType type_


-- ** First-order logic

prop_ipp_Number :: Number -> Property
prop_ipp_Number = ipp number

prop_ipp_Term :: Term -> Property
prop_ipp_Term = ipp term

prop_ipp_Literal :: Literal -> Property
prop_ipp_Literal = ipp literal

prop_ipp_Clause :: Clause -> Property
prop_ipp_Clause = ipp clause

prop_ipp_UnsortedFO :: UnsortedFirstOrder -> Property
prop_ipp_UnsortedFO = ippModulo reassociate unsortedFirstOrder

prop_ipp_MonomorphicFO :: MonomorphicFirstOrder -> Property
prop_ipp_MonomorphicFO = ippModulo reassociate monomorphicFirstOrder

prop_ipp_PolymorphicFO :: PolymorphicFirstOrder -> Property
prop_ipp_PolymorphicFO = ippModulo reassociate polymorphicFirstOrder


-- ** Units

prop_ipp_Unit :: Unit -> Property
prop_ipp_Unit = ippModulo normalizeUnit unit

prop_ipp_TPTP :: TPTP -> Property
prop_ipp_TPTP = ippModulo normalizeTPTP tptp

prop_ipp_TSTP :: TSTP -> Property
prop_ipp_TSTP = ippModulo normalizeTSTP tstp


-- ** Annotations

prop_ipp_Parent :: Parent -> Property
prop_ipp_Parent = ippModulo normalizeParent parent

prop_ipp_Source :: Source -> Property
prop_ipp_Source = ippModulo normalizeSource source

prop_ipp_Info :: Info -> Property
prop_ipp_Info = ippModulo normalizeInfo info


-- * Superfluous parenthesis

prop_ipp_sp_Term :: Term -> Property
prop_ipp_sp_Term = spAipp term

prop_ipp_sp_Literal :: Literal -> Property
prop_ipp_sp_Literal = spAipp literal

prop_ipp_sp_Clause :: Clause -> Property
prop_ipp_sp_Clause = spAipp clause

prop_ipp_sp_TFF1Sort :: TFF1Sort -> Property
prop_ipp_sp_TFF1Sort = spAipp tff1Sort

prop_ipp_sp_Type :: Type -> Property
prop_ipp_sp_Type = spAippModulo normalizeType type_

prop_ipp_sp_UnsortedFO :: UnsortedFirstOrder -> Property
prop_ipp_sp_UnsortedFO = spAippModulo reassociate unsortedFirstOrder

prop_ipp_sp_MonomorphicFO :: MonomorphicFirstOrder -> Property
prop_ipp_sp_MonomorphicFO = spAippModulo reassociate monomorphicFirstOrder

prop_ipp_sp_PolymorphicFO :: PolymorphicFirstOrder -> Property
prop_ipp_sp_PolymorphicFO = spAippModulo reassociate polymorphicFirstOrder


-- * Runner

return []

main :: IO ()
main = do
  success <- $forAllProperties $ quickCheckWithResult stdArgs{maxSuccess=1000}
  unless success exitFailure

