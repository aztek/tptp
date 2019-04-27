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
import Data.Maybe (maybeToList)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (Text, pack, cons)

import Data.TPTP
import Data.TPTP.Internal


-- * Helpers

instance Arbitrary s => Arbitrary (NonEmpty s) where
  arbitrary = genericArbitraryRec (1 % ())

deriving instance Generic (Name s)
instance (Named s, Enum s, Bounded s, Arbitrary s) => Arbitrary (Name s) where
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

lowerWord, upperWord, listOfPrintable, listOfPrintable1 :: Gen Text
lowerWord = cons <$> lowerAlpha <*> (pack <$> listOf alphaNumeric)
upperWord = cons <$> upperAlpha <*> (pack <$> listOf alphaNumeric)
listOfPrintable  = pack <$> listOf  printable
listOfPrintable1 = pack <$> listOf1 printable


-- * Names

instance Arbitrary Atom where
  arbitrary = Atom <$> oneof [lowerWord, listOfPrintable1]

instance Arbitrary Var where
  arbitrary = Var <$> upperWord

instance Arbitrary DistinctObject where
  arbitrary = DistinctObject <$> listOfPrintable

deriving instance Generic (Reserved s)
instance (Arbitrary s, Named s, Enum s, Bounded s) => Arbitrary (Reserved s) where
  arbitrary = oneof [
      Standard <$> arbitrary,
      extended <$> lowerWord
    ]

deriving instance Generic Function
instance Arbitrary Function where
  arbitrary = genericArbitraryU

deriving instance Generic Predicate
instance Arbitrary Predicate where
  arbitrary = genericArbitraryU


-- * Sorts and types

deriving instance Generic Sort
instance Arbitrary Sort where
  arbitrary = genericArbitraryU

deriving instance Generic TFF1Sort
instance Arbitrary TFF1Sort where
  arbitrary = genericArbitraryRec (1 % 1 % ())
  shrink = \case
    SortVariable{} -> []
    TFF1Sort f ss -> (TFF1Sort f <$> shrinkList shrink ss) ++ ss

deriving instance Generic Type
instance Arbitrary Type where
  arbitrary = genericArbitraryU
  shrink = \case
    Type as r -> Type <$> shrinkList shrink as <*> pure r
    TFF1Type vs as r ->
      TFF1Type <$> shrink vs <*> shrinkList shrink as <*> shrink r


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

deriving instance Generic (Sorted s)
instance Arbitrary s => Arbitrary (Sorted s) where
  arbitrary = genericArbitraryU
  shrink (Sorted s) = Sorted <$> shrinkMaybe shrink s

deriving instance Generic QuantifiedSort
instance Arbitrary QuantifiedSort where
  arbitrary = genericArbitraryU
  shrink _ = []

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
    CNF  c -> CNF  <$> shrink c
    FOF  f -> FOF  <$> shrink f
    TFF0 f -> TFF0 <$> shrink f
    TFF1 f -> TFF1 <$> shrink f

deriving instance Generic Role
instance Arbitrary Role where
  arbitrary = genericArbitraryU

deriving instance Generic Declaration
instance Arbitrary Declaration where
  arbitrary = oneof [
      Sort    <$> arbitrary <*> choose (0, 3),
      Typing  <$> arbitrary <*> arbitrary,
      Formula <$> arbitrary <*> arbitrary
    ]
  shrink = \case
    Sort    a n -> Sort    a <$> shrink n
    Typing  n t -> Typing  n <$> shrink t
    Formula r f -> Formula r <$> shrink f

deriving instance Generic Unit
instance Arbitrary Unit where
  arbitrary = genericArbitraryU
  shrink = \case
    Include f ns -> Include f <$> shrink ns
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
  arbitrary = genericArbitraryRec (1 % 1 % 1 % 1 % 1 % 1 % 1 % ())
  shrink = \case
    UnknownSource -> []
    UnitSource{} -> []
    File{} -> []
    Theory  n info -> Theory  n <$> shrinkMaybe shrink info
    Creator n info -> Creator n <$> shrinkMaybe shrink info
    Introduced i info -> Introduced i <$> shrinkMaybe shrink info
    Inference n i ps ->
      Inference n <$> shrink i <*> concatMap (shrinkList shrink) (shrink ps)

deriving instance Generic Parent
instance Arbitrary Parent where
  arbitrary = genericArbitraryRec (1 % ())
  shrink (Parent s gts) = Parent <$> shrink s <*> shrinkList shrink gts

deriving instance Generic GeneralData
instance Arbitrary GeneralData where
  arbitrary = genericArbitraryRec (1 % 2 % 2 % 2 % 2 % 1 % ())
  shrink = \case
    GeneralFunction   f gts -> GeneralFunction f <$> shrinkList shrink gts
    GeneralFormula        f -> GeneralFormula <$> shrink f
    GeneralBind v  (Left t) -> GeneralBind v . Left  <$> shrink t
    GeneralBind v (Right f) -> GeneralBind v . Right <$> shrink f
    _ -> []

deriving instance Generic GeneralTerm
instance Arbitrary GeneralTerm where
  arbitrary = genericArbitraryRec (1 % 1 % ())
  shrink = \case
    GeneralData gd gt ->
      (GeneralData <$> shrink gd <*> shrinkMaybe shrink gt) ++ maybeToList gt
    GeneralList gts   -> (GeneralList <$> shrinkList shrink gts) ++ gts

deriving instance Generic Info
instance Arbitrary Info where
  arbitrary = genericArbitraryRec (1 % ())
  shrink (Info gts) = Info <$> shrinkList shrink gts
