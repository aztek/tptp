{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Monoid (mempty)
#endif

import qualified Data.Foldable as F (toList)
import Data.List (genericReplicate, intersperse)
import Data.Maybe (maybeToList)
import Data.Text (Text)

#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup ((<>))
#endif

import Control.Applicative ((<$>), (<*>))

import Test.QuickCheck (Gen, choose, frequency)

import Data.Text.Prettyprint.Doc (
    Doc, (<+>), parens, tupled, list, surround, sep, space
  )

import Data.TPTP
import Data.TPTP.Pretty


-- | The class whose instances provide various ways of randomly pretty print
-- an expression.
class ArbitrarilyPretty e where
  apretty :: forall a. e -> Gen (Doc a)


-- * Helpers

-- | Randomply split a non-empty list into two lists.
randomSplit :: NonEmpty a -> Gen ([a], NonEmpty a)
randomSplit lst = do
  index <- choose (0, NEL.length lst - 1)
  let (prefix, suffix) = NEL.splitAt index lst
  return (prefix, NEL.fromList suffix)

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

star, arrow, tType, colon, negation :: Text
star = "*"
arrow = ">"
tType = "$tType"
colon = ":"
negation = "~"


-- * Superfluous parenthesis

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


-- ** SuperfluousParenthesis instances for common types

instance (ArbitrarilyPretty (SuperfluousParenthesis a),
          ArbitrarilyPretty (SuperfluousParenthesis b)) =>
          ArbitrarilyPretty (SuperfluousParenthesis (Either a b)) where
  apretty (SuperfluousParenthesis e) = either sppretty sppretty e

instance ArbitrarilyPretty (SuperfluousParenthesis Text) where
  apretty (SuperfluousParenthesis t) = return (pretty t)


-- ** Auxiliary data types and their SuperfluousParenthesis instances

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

-- | An auxiliary wrapper used to pretty print applications of prefix operators
-- with superfluous parenthesis.
data Prefix e o = Prefix o e
  deriving (Eq, Ord, Show)

instance (ArbitrarilyPretty (SuperfluousParenthesis e), Pretty o) =>
          ArbitrarilyPretty (SuperfluousParenthesis (Prefix e o)) where
  apretty (SuperfluousParenthesis (Prefix o e)) =
    (pretty o <+>) <$> superfluousParenthesis (sppretty e)

-- | An auxiliary wrapper used to pretty print expressions with superfluous
-- parenthesis with a condition that mandates at least one pair of parenthesis.
data ParensUnless e = ParensUnless Bool e
  deriving (Show, Eq, Ord)

instance ArbitrarilyPretty (SuperfluousParenthesis e) =>
         ArbitrarilyPretty (SuperfluousParenthesis (ParensUnless e)) where
  apretty (SuperfluousParenthesis (ParensUnless p e)) =
    if p then sppretty e else parens <$> sppretty e

-- | An auxiliary data structure used to pretty print a binary tree of disjunctions.
data BinTree t
  = Leaf t
  | Fork (BinTree t) (BinTree t)
  deriving (Eq, Ord, Show, Functor, Traversable, Foldable)

instance ArbitrarilyPretty (SuperfluousParenthesis l) =>
         ArbitrarilyPretty (SuperfluousParenthesis (BinTree l)) where
  apretty (SuperfluousParenthesis t) = case t of
    Leaf   l -> sppretty l
    Fork a b -> sppretty (Infix Disjunction a b)

-- | An auxiliary wrapper used to pretty print a typing of a symbol or a variable
-- with superfluous parenthesis around the type.
data Typing a t = Typing_ a t
  deriving (Eq, Show, Ord)

instance (ArbitrarilyPretty (SuperfluousParenthesis t), Pretty a) =>
          ArbitrarilyPretty (SuperfluousParenthesis (Typing a t)) where
  apretty (SuperfluousParenthesis (Typing_ a t)) =
    (pretty a <>) <$> sppretty (Prefix colon t)

-- | An auxiliary wrapper used to pretty print a mapping type with superfluous
-- parenthesis around the argument type and the return type.
data Mapping s = Mapping [s] s
  deriving (Eq, Show, Ord, Functor, Traversable, Foldable)

instance ArbitrarilyPretty (SuperfluousParenthesis s) =>
         ArbitrarilyPretty (SuperfluousParenthesis (Mapping s)) where
  apretty (SuperfluousParenthesis (Mapping [] s)) = sppretty s
  apretty (SuperfluousParenthesis m) = do
    Mapping as s <- mapM (superfluousParenthesis . sppretty) m
    return (parens (starSeparated as) <+> pretty arrow <+> s)
    where
      starSeparated = sep . intersperse (pretty star)


-- ** First-order logic

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
    Negative -> sppretty (Prefix negation l)

instance ArbitrarilyPretty (SuperfluousParenthesis Clause) where
  apretty (SuperfluousParenthesis (Clause ls)) = randomTree ls >>= sppretty

unitary :: FirstOrder s -> Bool
unitary = \case
  Atomic{}     -> True
  Negated{}    -> True
  Quantified{} -> True
  Connected{}  -> False

under :: Connective -> FirstOrder s -> Bool
under c = \case
  Connected _ c' _ -> c' == c && isAssociative c
  f -> unitary f

instance ArbitrarilyPretty (SuperfluousParenthesis s) =>
         ArbitrarilyPretty (SuperfluousParenthesis (FirstOrder s)) where
  apretty (SuperfluousParenthesis f) = case f of
    Atomic  l -> sppretty l
    Negated g -> sppretty (Prefix negation (ParensUnless (unitary g) g))
    Connected g c h -> sppretty (Infix c (ParensUnless (under c g) g)
                                         (ParensUnless (under c h) h))
    Quantified q vs g -> do
      let var (v, s) = (pretty v <>) <$> sppretty s
      vs' <- mapM var (F.toList vs)
      g' <- sppretty (Prefix colon (ParensUnless (unitary g) g))
      return (pretty q <+> list vs' <> g')


-- ** Sorts and types

instance ArbitrarilyPretty (SuperfluousParenthesis Unsorted) where
  apretty (SuperfluousParenthesis u) = return (pretty u)

instance ArbitrarilyPretty (SuperfluousParenthesis s) =>
         ArbitrarilyPretty (SuperfluousParenthesis (Sorted s)) where
  apretty (SuperfluousParenthesis (Sorted ms)) = case ms of
    Nothing -> return mempty
    Just s  -> sppretty (Prefix colon s)

instance ArbitrarilyPretty (SuperfluousParenthesis (Name Sort)) where
  apretty (SuperfluousParenthesis n) = return (pretty n)

instance ArbitrarilyPretty (SuperfluousParenthesis QuantifiedSort) where
  apretty (SuperfluousParenthesis s) = return (pretty s)

instance ArbitrarilyPretty (SuperfluousParenthesis TFF1Sort) where
  apretty (SuperfluousParenthesis s) = case s of
    SortVariable v -> return (pretty v)
    TFF1Sort  f ss -> sppretty (Application_ f ss)

instance ArbitrarilyPretty (SuperfluousParenthesis Type) where
  apretty (SuperfluousParenthesis t) = case t of
    Type        as r -> sppretty (Mapping as r)
    TFF1Type [] as r -> sppretty (Mapping as r)
    TFF1Type vs as r -> do
      vs' <- mapM sppretty (fmap (flip Typing_ tType) vs)
      t' <- sppretty (Prefix colon (ParensUnless (null as) (TFF1Type [] as r)))
      return ("!>" <+> list vs' <> t')


-- ** Units

instance ArbitrarilyPretty (SuperfluousParenthesis Formula) where
  apretty (SuperfluousParenthesis f) = case f of
    CNF  c -> sppretty c
    FOF  g -> sppretty g
    TFF0 g -> sppretty g
    TFF1 g -> sppretty g

instance ArbitrarilyPretty (SuperfluousParenthesis Declaration) where
  apretty (SuperfluousParenthesis d) = case d of
    Formula _ f -> sppretty f
    Typing  s t -> sppretty (Typing_ s t)
    Sort    s n -> sppretty (Typing_ s (Mapping (genericReplicate n tType) tType))

instance ArbitrarilyPretty (SuperfluousParenthesis Unit) where
  apretty (SuperfluousParenthesis u) = case u of
    Include{} -> return (pretty u)
    Unit nm decl a -> do
      decl' <- superfluousParenthesis (sppretty decl)
      let args = pretty nm : prettyRole decl : decl' : ann
      return (application (declarationLanguage decl) args <> ".")
      where
        prettyRole = \case
          Sort{}      -> "type"
          Typing{}    -> "type"
          Formula r _ -> pretty (name r)

        ann = case a of
          Just (s, i) -> pretty s : maybeToList (fmap prettyList i)
          Nothing -> []
