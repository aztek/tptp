{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module       : Data.TPTP.Pretty
-- Description  : Pretty printers for the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TPTP.Pretty (
  Pretty(..)
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (genericReplicate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

#if __GLASGOW_HASKELL__ >= 803
import Prelude hiding ((<>))
#endif

import Data.TPTP
import Data.TPTP.Internal
import Data.Text.Prettyprint.Doc

-- * Helper functions

sepBy :: [Doc ann] -> Doc ann -> Doc ann
sepBy as s = hsep (punctuate s as)

sepBy1 :: NonEmpty (Doc ann) -> Doc ann -> Doc ann
sepBy1 as s = hsep (punctuate s (NEL.toList as))

application :: Pretty f => f -> [Doc ann] -> Doc ann
application f [] = pretty f
application f as = pretty f <> parens (as `sepBy` comma)


-- * Names

quoted :: Char -> Text -> Text
quoted q = T.cons q . flip T.snoc q
         . T.replace (T.singleton q) (T.pack ['\\', q])
         . T.replace "\\" "\\\\"

newtype SingleQuoted = SingleQuoted Text
  deriving (Eq, Show, Ord)

instance Pretty SingleQuoted where
  pretty (SingleQuoted t) = pretty (quoted '\'' t)

instance Pretty Atom where
  pretty (Atom s)
    | isLowerWord s = pretty s
    | otherwise = pretty (SingleQuoted s)
    where
      isLowerWord w = isAsciiLower (T.head w) && T.all isAlphaNum (T.tail w)
      isAlphaNum c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

instance Pretty Var where
  pretty (Var s) = pretty s

newtype DoubleQuoted = DoubleQuoted Text
  deriving (Eq, Show, Ord)

instance Pretty DoubleQuoted where
  pretty (DoubleQuoted t) = pretty (quoted '"' t)

instance Pretty DistinctObject where
  pretty (DistinctObject s) = pretty (DoubleQuoted s)

newtype DollarWord = DollarWord Text
  deriving (Eq, Show, Ord)

instance Pretty DollarWord where
  pretty (DollarWord w) = pretty (T.cons '$' w)

tType :: DollarWord
tType = DollarWord "tType"

instance Named s => Pretty (Name s) where
  pretty = \case
    Reserved s -> pretty (DollarWord (name s))
    Defined  a -> pretty a


-- * Sorts and types

instance Pretty TFF1Sort where
  pretty = \case
    SortVariable v -> pretty v
    TFF1Sort f ss -> application f (fmap pretty ss)

prettyMapping :: Pretty a => [a] -> a -> Doc ann
prettyMapping as r = args <> pretty r
  where
    args = case as of
      []  -> mempty
      [a] -> pretty a <+> ">" <> space
      _   -> parens (fmap pretty as `sepBy` (space <> "*")) <+> ">" <> space

instance Pretty Type where
  pretty = \case
    Type as r -> prettyMapping as r
    TFF1Type vs as r -> prefix <> if null as then matrix else parens matrix
      where
        prefix = case NEL.nonEmpty vs of
          Nothing  -> mempty
          Just vs' -> "!>" <+> brackets (vars vs') <> ":" <> space
        vars vs' = fmap prettyVar vs' `sepBy1` comma
        prettyVar v = pretty v <> ":" <+> pretty tType
        matrix = prettyMapping as r


-- * First-order logic

instance Pretty Number where
  pretty = \case
    IntegerConstant i    -> pretty i
    RationalConstant n d -> pretty n <> "/" <> pretty d
    RealConstant r       -> pretty (show r)

instance Pretty Term where
  pretty = \case
    Function f ts  -> application f (fmap pretty ts)
    Variable v     -> pretty v
    Number i       -> pretty i
    DistinctTerm d -> pretty d

instance Pretty Literal where
  pretty = \case
    Predicate p ts -> application p (fmap pretty ts)
    Equality a s b -> pretty a <+> pretty s <+> pretty b

instance Pretty Sign where
  pretty = pretty . name

instance Pretty Clause where
  pretty = \case
    Clause ls -> fmap p ls `sepBy1` (space <> pretty Disjunction)
      where
        p (Positive, l) = pretty l
        p (Negative, l) = "~" <+> parens (pretty l)

instance Pretty Quantifier where
  pretty = pretty . name

instance Pretty Connective where
  pretty = pretty . name

instance Pretty Unsorted where
  pretty = const mempty

instance Pretty s => Pretty (Sorted s) where
  pretty (Sorted s) = maybe mempty (\a -> ":" <+> pretty a) s

instance Pretty QuantifiedSort where
  pretty = const (pretty tType)

instance Pretty (Either QuantifiedSort TFF1Sort) where
  pretty = either pretty pretty

unitary :: FirstOrder s -> Bool
unitary = \case
  Atomic{}     -> True
  Negated{}    -> True
  Quantified{} -> True
  Connected{}  -> False

pretty' :: Pretty s => FirstOrder s -> Doc ann
pretty' f
  | unitary f = pretty f
  | otherwise = parens (pretty f)

instance Pretty s => Pretty (FirstOrder s) where
  pretty = \case
    Atomic l -> pretty l
    Negated f -> "~" <+> pretty' f
    Connected f c g -> pretty'' f <+> pretty c <+> pretty'' g
      where
        -- Nested applications of associative connectives do not require
        -- parenthesis. Otherwise, the connectives do not have precedence
        pretty'' e@(Connected _ c' _)
          | c' == c && isAssociative c = pretty e
        pretty'' e = pretty' e
    Quantified q vs f -> pretty q <+> vs' <> ":" <+> pretty' f
      where
        vs' = brackets (fmap var vs `sepBy1` comma)
        var (v, s) = pretty v <> pretty s


-- ** Units

instance Pretty Language where
  pretty = pretty . name

instance Pretty Formula where
  pretty = \case
    CNF  c -> pretty c
    FOF  f -> pretty f
    TFF0 f -> pretty f
    TFF1 f -> pretty f

instance Pretty UnitName where
  pretty = either pretty pretty

instance Pretty Unit where
  pretty = \case
    Include (Atom f) ns -> application (Atom "include") args <> "."
      where
        args  = pretty (SingleQuoted f) : names
        names = [brackets (fmap pretty ns `sepBy` comma) | not (null ns)]
    Unit n d a -> application (language d) (nm : decl ++ ann) <> "."
      where
        nm = pretty n

        decl = case d of
          Sort   s ar -> ["type", pretty s <> ":" <+> sortConstructor ar]
          Typing  s t -> ["type", pretty s <> ":" <+> pretty t]
          Formula r f -> [pretty (name r), pretty f]

        sortConstructor ar = prettyMapping (genericReplicate ar tType) tType

        ann = case a of
          Just (s, Just i)  -> [pretty s, pretty i]
          Just (s, Nothing) -> [pretty s]
          Nothing -> []

instance Pretty Derivation where
  pretty (Derivation us) = sep (fmap pretty us)


-- * Annotations

instance Pretty Intro where
  pretty = pretty . name

instance Pretty Expression where
  pretty = \case
    Logical f -> application (DollarWord . name $ formulaLanguage f) [pretty f]
    Term    t -> application (DollarWord "fot") [pretty t]

instance Pretty GeneralData where
  pretty = \case
    GeneralFunction f gts -> application f (fmap pretty gts)
    GeneralVariable   v -> pretty v
    GeneralNumber     n -> pretty n
    GeneralExpression e -> pretty e
    GeneralBind     v e -> application (Atom "bind") [pretty v, pretty e]
    GeneralDistinct   d -> pretty d

instance Pretty GeneralTerm where
  pretty = \case
    GeneralData gd gt -> pretty gd <> maybe mempty (\t -> ":" <> pretty t) gt
    GeneralList gts   -> prettyList gts

  prettyList gts = brackets (fmap pretty gts `sepBy` comma)

instance Pretty Info where
  pretty (Info gts) = brackets (fmap pretty gts `sepBy` comma)

instance Pretty Parent where
  pretty (Parent s gts) = pretty s
                       <> if null gts then mempty else ":" <> prettyList gts

instance Pretty Source where
  pretty = \case
    File (Atom n) i -> source "file"       (SingleQuoted n) i
    Theory     n  i -> source "theory"     n i
    Creator    n  i -> source "creator"    n i
    Introduced n  i -> source "introduced" (name n) i
    Inference  n  i ps -> application (Atom "inference") [
        pretty n, pretty i, brackets (fmap pretty ps `sepBy` comma)
      ]
    UnitSource un -> pretty un
    UnknownSource -> "unknown"
    where
      source :: (Pretty a, Pretty b) => Text -> a -> Maybe b -> Doc ann
      source f n i = application f (pretty n : maybeToList (fmap pretty i))

  prettyList ss = brackets (fmap pretty ss `sepBy` comma)
