{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Generators where

import GHC.Generics
import Generic.Random
import Test.QuickCheck hiding (Sorted, Function)

import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (pack)

import Data.TSTP

-- * Helpers

instance Arbitrary s => Arbitrary (NonEmpty s) where
  arbitrary = genericArbitraryRec (1 % ())

alphaNumeric :: String
alphaNumeric = '_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- * Names

instance Arbitrary Atom where
  arbitrary = Atom . pack <$> oneof [lowerWord, singleQuoted]
    where
      lowerWord = (:) <$> elements ['a'..'z']
                      <*> listOf (elements alphaNumeric)
      singleQuoted = listOf1 (elements sg)
      sg = [chr 0o40..chr 0o46] ++ [chr 0o50..chr 0o176]

instance Arbitrary Var where
  arbitrary = Var . pack <$> upperWord
    where
      upperWord = (:) <$> elements ['A'..'Z']
                      <*> listOf (elements alphaNumeric)

deriving instance Generic StandardFunction
instance Arbitrary StandardFunction where
  arbitrary = genericArbitraryU

deriving instance Generic Function
instance Arbitrary Function where
  arbitrary = genericArbitraryU

deriving instance Generic StandardPredicate
instance Arbitrary StandardPredicate where
  arbitrary = genericArbitraryU

deriving instance Generic Predicate
instance Arbitrary Predicate where
  arbitrary = genericArbitraryU

-- * Sorts and types

deriving instance Generic StandardSort
instance Arbitrary StandardSort where
  arbitrary = genericArbitraryU

deriving instance Generic Sort
instance Arbitrary Sort where
  arbitrary = genericArbitraryU

deriving instance Generic Type
instance Arbitrary Type where
  arbitrary = genericArbitraryU
  shrink (Mapping as r) = flip Mapping r <$> shrinkList shrink as

-- * First-order logic

deriving instance Generic Term
instance Arbitrary Term where
  arbitrary = genericArbitraryRec (2 % 3 % 1 % ())
  shrink = \case
    Function f ts -> ts ++ (Function f <$> shrinkList shrink ts)
    _ -> []

deriving instance Generic Literal
instance Arbitrary Literal where
  arbitrary = genericArbitraryU
  shrink = \case
    Predicate p ts -> Predicate p <$> shrinkList shrink ts
    Equality a s b -> do a' <- shrink a
                         b' <- shrink b
                         return (Equality a' s b')
    _ -> []

deriving instance Generic Sign
instance Arbitrary Sign where
  arbitrary = genericArbitraryU

deriving instance Generic Clause
instance Arbitrary Clause where
  arbitrary = genericArbitraryU
  -- shrink (Clause ls) = Clause <$> shrinkList shrink ls

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
  shrink = \case
    Sorted (Just _) -> [Sorted Nothing]
    _ -> []

deriving instance Generic (FirstOrder s)
instance Arbitrary s => Arbitrary (FirstOrder s) where
  arbitrary = genericArbitraryRec (3 % 2 % 2 % 1 % ())
  shrink = \case
    Atomic l -> Atomic <$> shrink l
    Quantified q vs f -> f : (Quantified q vs <$> shrink f)
    Negated f -> f : (Negated <$> shrink f)
    Connected f c g -> [f, g] ++ do f' <- shrink f
                                    g' <- shrink g
                                    return (Connected f' c g')

deriving instance Generic Formula
instance Arbitrary Formula where
  arbitrary = genericArbitraryU
  shrink = \case
    CNF c -> CNF <$> shrink c
    FOF f -> FOF <$> shrink f
    TFF f -> TFF <$> shrink f

-- * Formula annotations

deriving instance Generic StandardRole
instance Arbitrary StandardRole where
  arbitrary = genericArbitraryU

deriving instance Generic Role
instance Arbitrary Role where
  arbitrary = genericArbitraryU

deriving instance Generic Intro
instance Arbitrary Intro where
  arbitrary = genericArbitraryU

deriving instance Generic Source
instance Arbitrary Source where
  arbitrary = genericArbitraryRec (2 % 2 % 2 % 2 % 2 % 2 % 2 % 1 % ())
  shrink = \case
    Theory  n (Just _) -> [Theory  n Nothing]
    Creator n (Just _) -> [Creator n Nothing]
    Introduced i (Just _) -> [Introduced i Nothing]
    Inference n i ps -> Inference n <$> shrink i <*> shrinkList shrink ps
    Sources ss -> Sources <$> shrink ss
    _ -> []

deriving instance Generic Parent
instance Arbitrary Parent where
  arbitrary = genericArbitraryRec (1 % ())
  shrink (Parent s gts) = Parent s <$> shrinkList shrink gts

deriving instance Generic GeneralData
instance Arbitrary GeneralData where
  arbitrary = genericArbitraryRec (1 % 2 % 2 % 2 % 2 % ())
  shrink = \case
    GeneralFunction f gts -> GeneralFunction f <$> shrinkList shrink gts
    GeneralVariable _     -> []
    GeneralNumber _       -> []
    GeneralFormula f      -> GeneralFormula <$> shrink f
    GeneralBind v f       -> GeneralBind v <$> shrink f

deriving instance Generic GeneralTerm
instance Arbitrary GeneralTerm where
  arbitrary = genericArbitraryRec (1 % 1 % ())
  shrink = \case
    GeneralData gd Nothing   -> GeneralData <$> shrink gd <*> return Nothing
    GeneralData gd (Just gt) -> GeneralData <$> shrink gd <*> fmap Just (shrink gt)
    GeneralList gts          -> GeneralList <$> shrinkList shrink gts

deriving instance Generic Info
instance Arbitrary Info where
  arbitrary = genericArbitraryRec (1 % ())
  shrink (Info gts) = Info <$> shrinkList shrink gts

-- * Derivations

deriving instance Generic Unit
instance Arbitrary Unit where
  arbitrary = genericArbitraryRec (1 % ())
  shrink (Unit nm r f ann) = do { f' <- shrink f; return (Unit nm r f' ann) }

deriving instance Generic Derivation
instance Arbitrary Derivation where
  arbitrary = genericArbitraryU
  shrink (Derivation us) = Derivation <$> shrinkList shrink us
