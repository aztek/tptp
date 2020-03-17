{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

-- |
-- Module       : ArbitrarilyPretty
-- Description  : Generators for randomized pretty outputs of TPTP expressions.
-- Copyright    : (c) Evgenii Kotelnikov, 2020
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--
-- Defines a class 'ArbitrarilyPretty' used for randomized pretty printing along
-- with instances of this class implementing substandard ways to pretty print
-- a TPTP expression.
--

module ArbitrarilyPretty (
  ArbitrarilyPretty(..),
  SuperfluousParenthesis(..),
  randomSplit,
  randomTree
) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL (splitAt, fromList, length)

#if MIN_VERSION_base(4, 8, 0)
import Prelude hiding ((<$>), (<*>))
#else
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup ((<>))
#endif

import Control.Applicative ((<$>), (<*>))

import Test.QuickCheck (Gen, choose, frequency)

import Data.Text.Prettyprint.Doc (Doc, (<+>), parens, tupled, surround, space)

import Data.TPTP
import Data.TPTP.Pretty


-- | The class whose instances provide various ways of randomly pretty print
-- an expression.
class ArbitrarilyPretty e where
  apretty :: forall a. e -> Gen (Doc a)


-- ** Superfluous parenthesis

-- | An auxiliary wrapper used to generate pretty printed TPTP expressions
-- with superfluous parenthesis.
newtype SuperfluousParenthesis a = SuperfluousParenthesis {
  unSuperfluousParenthesis :: a
} deriving (Eq, Show, Ord, Functor, Traversable, Foldable)

-- | Randomly pretty print a TPTP expression with superflouous parenthesis
-- somewhere inside the expression but not on the outer level.
sppretty :: ArbitrarilyPretty (SuperfluousParenthesis e)
         => e -> forall a. Gen (Doc a)
sppretty = apretty . SuperfluousParenthesis

-- | Map a given 'Doc' generator to a one that randomly wraps the document
-- in zero, one or two pairs of parenthesis.
superfluousParenthesis :: Gen (Doc a) -> Gen (Doc a)
superfluousParenthesis g = frequency [
    (2, g),
    (2, parens <$> g),
    (1, parens . parens <$> g)
  ]

-- | An auxiliary wrapper used to pretty print applications of function and
-- predicate symbols with superfluous parenthesis.
data Application s e = Application_ s [e]
  deriving (Eq, Ord, Show)

-- | Pretty print an application of a symbol to a list of arguments.
application :: Pretty f => f -> [Doc a] -> Doc a
application f [] = pretty f
application f as = pretty f <> tupled as

instance (ArbitrarilyPretty (SuperfluousParenthesis e), Pretty s) =>
          ArbitrarilyPretty (SuperfluousParenthesis (Application s e)) where
  apretty (SuperfluousParenthesis (Application_ s ts)) =
    application s <$> mapM (superfluousParenthesis . sppretty) ts

-- | An auxiliary wrapper used to pretty print applications of infix operators
-- with superfluous parenthesis.
data Infix e o = Infix o e e
  deriving (Eq, Ord, Show)

instance (ArbitrarilyPretty (SuperfluousParenthesis e), Pretty o) =>
          ArbitrarilyPretty (SuperfluousParenthesis (Infix e o)) where
  apretty (SuperfluousParenthesis (Infix o a b)) =
        surround (space <> pretty o <> space)
    <$> superfluousParenthesis (sppretty a)
    <*> superfluousParenthesis (sppretty b)

data Prefix e o = Prefix o e
  deriving (Eq, Ord, Show)

instance (ArbitrarilyPretty (SuperfluousParenthesis e), Pretty o) =>
          ArbitrarilyPretty (SuperfluousParenthesis (Prefix e o)) where
  apretty (SuperfluousParenthesis (Prefix o e)) =
    (pretty o <+>) <$> superfluousParenthesis (sppretty e)

instance ArbitrarilyPretty (SuperfluousParenthesis Term) where
  apretty (SuperfluousParenthesis t) = case t of
    Function  f ts -> sppretty (Application_ f ts)
    Variable     v -> return (pretty v)
    Number       i -> return (pretty i)
    DistinctTerm d -> return (pretty d)

instance ArbitrarilyPretty (SuperfluousParenthesis Literal) where
  apretty (SuperfluousParenthesis l) = case l of
    Predicate p ts -> sppretty (Application_ p ts)
    Equality a s b -> sppretty (Infix s a b)

instance ArbitrarilyPretty (SuperfluousParenthesis (Sign, Literal)) where
  apretty (SuperfluousParenthesis (s, l)) = case s of
    Positive -> sppretty l
    Negative -> sppretty (Prefix '~' l)

-- | Randomply split a non-empty list into two lists.
randomSplit :: NonEmpty a -> Gen ([a], NonEmpty a)
randomSplit list = do
  index <- choose (0, NEL.length list - 1)
  let (prefix, suffix) = NEL.splitAt index list
  return (prefix, NEL.fromList suffix)

-- | The binary tree - an auxiliary data structure used to represent a tree of
-- of disjunctions.
data BinTree t
  = Leaf t
  | Fork (BinTree t) (BinTree t)
  deriving (Eq, Ord, Show, Functor, Traversable, Foldable)

-- | Randomly split a non-empty list into a binary tree such that collecting
-- the leafs of that tree from left to right results in the original list.
--
-- This splitting is used to generate randomly nested disjunctions with
-- superfluous parenthesis compared to an equivalent clause.
randomTree :: NonEmpty a -> Gen (BinTree a)
randomTree = \case
  a :| [] -> return (Leaf a)
  a :| a' : as -> do
    (prefix, suffix) <- randomSplit (a' :| as)
    Fork <$> randomTree (a :| prefix) <*> randomTree suffix

instance ArbitrarilyPretty (SuperfluousParenthesis l) =>
         ArbitrarilyPretty (SuperfluousParenthesis (BinTree l)) where
  apretty (SuperfluousParenthesis t) = case t of
    Leaf   l -> sppretty l
    Fork a b -> sppretty (Infix Disjunction a b)

instance ArbitrarilyPretty (SuperfluousParenthesis Clause) where
  apretty (SuperfluousParenthesis (Clause ls)) = randomTree ls >>= sppretty
