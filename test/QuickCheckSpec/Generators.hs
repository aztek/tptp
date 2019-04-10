{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module       : Generators
-- Description  : QuickCheck generators for datatypes in the tptp library.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Generators () where

import GHC.Generics
import Generic.Random
import Test.QuickCheck hiding (Sorted, Function)

import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (pack)

import Data.TPTP

-- * Helpers

instance Arbitrary s => Arbitrary (NonEmpty s) where
  arbitrary = genericArbitraryRec (1 % ())

deriving instance Generic (Name s)
instance Arbitrary s => Arbitrary (Name s) where
  arbitrary = genericArbitraryU

shrinkMaybe :: (a -> [a]) -> Maybe a -> [Maybe a]
shrinkMaybe s = \case
  Nothing -> []
  Just a  -> Nothing : fmap Just (s a)

lowerAlpha, upperAlpha, printable, numeric, alphaNumeric :: Gen Char
lowerAlpha   = choose ('a', 'z')
upperAlpha   = choose ('A', 'Z')
numeric      = choose ('0', '9')
printable    = choose (' ', '~')
alphaNumeric = oneof [pure '_', lowerAlpha, upperAlpha, numeric]


-- * Names

instance Arbitrary Atom where
  arbitrary = Atom . pack <$> oneof [
      (:) <$> lowerAlpha <*> listOf alphaNumeric,
      listOf1 printable
    ]

instance Arbitrary Var where
  arbitrary = Var . pack <$> ((:) <$> upperAlpha <*> listOf alphaNumeric)

instance Arbitrary DistinctObject where
  arbitrary = DistinctObject . pack <$> listOf printable

deriving instance Generic Function
instance Arbitrary Function where
  arbitrary = genericArbitraryU

deriving instance Generic Predicate
instance Arbitrary Predicate where
  arbitrary = genericArbitraryU

deriving instance Generic Sort
instance Arbitrary Sort where
  arbitrary = genericArbitraryU


-- * First-order logic

instance Arbitrary Scientific where
  arbitrary = Sci.scientific <$> arbitrary <*> arbitrary

instance Arbitrary Number where
  arbitrary = oneof [
      IntegerConstant  <$> arbitrary,
      RationalConstant <$> arbitrary <*> arbitrary `suchThat` (> 0),
      RealConstant     <$> arbitrary
    ]

deriving instance Generic Term
instance Arbitrary Term where
  arbitrary = genericArbitraryRec (2 % 3 % 1 % 1 % ())
  shrink = \case
    Function f ts -> ts ++ (Function f <$> shrinkList shrink ts)
    _ -> []

deriving instance Generic Literal
instance Arbitrary Literal where
  arbitrary = genericArbitraryU
  shrink = \case
    Predicate p ts -> Predicate p <$> shrinkList shrink ts
    Equality a s b -> Equality <$> shrink a <*> pure s <*> shrink b
    _ -> []

deriving instance Generic Sign
instance Arbitrary Sign where
  arbitrary = genericArbitraryU

deriving instance Generic Clause
instance Arbitrary Clause where
  arbitrary = genericArbitraryU
  shrink (Clause ls) = Clause <$> shrink ls

deriving instance Generic Quantifier
instance Arbitrary Quantifier where
  arbitrary = genericArbitraryU

deriving instance Generic Connective
instance Arbitrary Connective where
  arbitrary = genericArbitraryU

deriving instance Generic Unsorted
instance Arbitrary Unsorted where
  arbitrary = genericArbitraryU

deriving instance Generic Sorted
instance Arbitrary Sorted where
  arbitrary = genericArbitraryU
  shrink (Sorted s) = Sorted <$> shrinkMaybe shrink s

deriving instance Generic (FirstOrder s)
instance Arbitrary s => Arbitrary (FirstOrder s) where
  arbitrary = genericArbitraryRec (3 % 2 % 2 % 1 % ())
  shrink = \case
    Atomic l -> Atomic <$> shrink l
    Negated f -> f : (Negated <$> shrink f)
    Quantified q vs f -> f : (Quantified q vs <$> shrink f)
    Connected f c g -> f : g : (Connected <$> shrink f <*> pure c <*> shrink g)


-- * Units

deriving instance Generic Formula
instance Arbitrary Formula where
  arbitrary = genericArbitraryU
  shrink = \case
    CNF c -> CNF <$> shrink c
    FOF f -> FOF <$> shrink f
    TFF f -> TFF <$> shrink f

deriving instance Generic Role
instance Arbitrary Role where
  arbitrary = genericArbitraryU

deriving instance Generic Type
instance Arbitrary Type where
  arbitrary = genericArbitraryU
  shrink = \case
    TFFType as r -> flip TFFType r <$> shrinkList shrink as

deriving instance Generic Declaration
instance Arbitrary Declaration where
  arbitrary = genericArbitraryU
  shrink = \case
    Sort _      -> []
    Typing  n t -> Typing  n <$> shrink t
    Formula r f -> Formula r <$> shrink f

deriving instance Generic Unit
instance Arbitrary Unit where
  arbitrary = genericArbitraryU
  shrink = \case
    Include _  -> []
    Unit n d a -> Unit n <$> shrink d <*> shrinkAnnotation a
      where
        shrinkAnnotation = shrinkMaybe $ bitraverse shrink (shrinkMaybe shrink)

deriving instance Generic Derivation
instance Arbitrary Derivation where
  arbitrary = genericArbitraryU
  shrink (Derivation us) = Derivation <$> shrinkList shrink us


-- * Annotations

deriving instance Generic Intro
instance Arbitrary Intro where
  arbitrary = genericArbitraryU

deriving instance Generic Source
instance Arbitrary Source where
  arbitrary = genericArbitraryRec (2 % 2 % 2 % 2 % 2 % 2 % 2 % 1 % ())
  shrink = \case
    Theory  n info -> Theory  n <$> shrinkMaybe shrink info
    Creator n info -> Creator n <$> shrinkMaybe shrink info
    Introduced i info -> Introduced i <$> shrinkMaybe shrink info
    Inference n i ps -> Inference n <$> shrink i <*> shrinkList shrink ps
    Sources ss -> Sources <$> shrink ss
    _ -> []

deriving instance Generic Parent
instance Arbitrary Parent where
  arbitrary = genericArbitraryRec (1 % ())
  shrink (Parent s gts) = Parent <$> shrink s <*> shrinkList shrink gts

deriving instance Generic GeneralData
instance Arbitrary GeneralData where
  arbitrary = genericArbitraryRec (1 % 2 % 2 % 2 % 2 % 1 % ())
  shrink = \case
    GeneralFunction f gts -> GeneralFunction f <$> shrinkList shrink gts
    GeneralFormula f      -> GeneralFormula <$> shrink f
    GeneralBind v f       -> GeneralBind v <$> shrink f
    _ -> []

deriving instance Generic GeneralTerm
instance Arbitrary GeneralTerm where
  arbitrary = genericArbitraryRec (1 % 1 % ())
  shrink = \case
    GeneralData gd gt -> GeneralData <$> shrink gd <*> shrinkMaybe shrink gt
    GeneralList gts   -> GeneralList <$> shrinkList shrink gts

deriving instance Generic Info
instance Arbitrary Info where
  arbitrary = genericArbitraryRec (1 % ())
  shrink (Info gts) = Info <$> shrinkList shrink gts
