{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module       : Data.TSTP.Pretty
-- Description  : Pretty printers for the TSTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TSTP.Pretty (
  Pretty(..)
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import qualified Data.Text as T

#if __GLASGOW_HASKELL__ >= 803
import Prelude hiding ((<>))
#endif

import Data.TSTP
import Data.TSTP.Internal
import Data.Text.Prettyprint.Doc

-- * Helper functions

sepBy :: [Doc ann] -> Doc ann -> Doc ann
sepBy as s = hsep (punctuate s as)

sepBy1 :: NonEmpty (Doc ann) -> Doc ann -> Doc ann
sepBy1 as s = hsep (punctuate s (NEL.toList as))

instance Named s => Pretty (Name s) where
  pretty = \case
    Reserved s -> pretty (T.cons '$' $ name s)
    Defined a  -> pretty a

keyword :: Named s => Name s -> Doc ann
keyword = \case
  Reserved s -> pretty (name s)
  Defined a  -> pretty a

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

-- * Sorts and types

instance Pretty Type where
  pretty = \case
    Mapping []  r -> pretty r
    Mapping [a] r -> pretty a  <+> ">" <+> pretty r
    Mapping as  r -> parens ts <+> ">" <+> pretty r
      where ts = fmap pretty as `sepBy` (space <> "*")

-- * First-order logic

instance Pretty Number where
  pretty = \case
    IntegerConstant i    -> pretty i
    RationalConstant n d -> pretty n <> "/" <> pretty d
    RealConstant r       -> pretty (show r)

instance Pretty Term where
  pretty = \case
    Function f []  -> pretty f
    Function f ts  -> pretty f <> parens (fmap pretty ts `sepBy` comma)
    Variable v     -> pretty v
    Number i       -> pretty i
    DistinctTerm d -> pretty d

instance Pretty Literal where
  pretty = \case
    Predicate p [] -> pretty p
    Predicate p ts -> pretty p <> parens (fmap pretty ts `sepBy` comma)
    Equality a s b -> pretty a <+> pretty s <+> pretty b
    Tautology      -> "$true"
    Falsum         -> "$false"

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

instance Pretty Sorted where
  pretty = \case
    Sorted Nothing  -> mempty
    Sorted (Just s) -> ":" <+> pretty s

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
        vs' = brackets $ fmap var vs `sepBy1` comma
        var (v, s) = pretty v <> pretty s

instance Pretty Formula where
  pretty = \case
    CNF c -> pretty c
    FOF f -> pretty f
    TFF f -> pretty f

-- * Derivations

instance Pretty Language where
  pretty = pretty . name

instance Pretty Intro where
  pretty = pretty . name

instance Pretty GeneralData where
  pretty = \case
    GeneralFunction f gts -> pretty f <> maybe mempty args (NEL.nonEmpty gts)
      where args ts = parens (fmap pretty ts `sepBy1` comma)
    GeneralVariable v -> pretty v
    GeneralNumber n -> pretty n
    GeneralFormula f -> prettyFormula f
    GeneralBind v f -> "bind" <> parens (pretty v <> comma <+> prettyFormula f)
    GeneralDistinct d -> pretty d
    where
      prettyFormula f = "$" <> pretty (language f) <> parens (pretty f)

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
    Introduced n  i -> source "introduced" n i
    Inference n i ps -> "inference" <> parens (args `sepBy` comma)
      where args = [pretty n, pretty i, brackets (fmap pretty ps `sepBy` comma)]
    UnknownSource -> "unknown"
    Sources ss -> prettyList (NEL.toList ss)
    Dag n -> pretty n
    where
      source :: (Pretty a, Pretty b) => Text -> a -> Maybe b -> Doc ann
      source f n i = pretty f <> parens (pretty n <> optional i)

      optional :: Pretty b => Maybe b -> Doc ann
      optional = maybe mempty (\a -> comma <+> pretty a)

  prettyList ss = brackets (fmap pretty ss `sepBy` comma)

instance Pretty Unit where
  pretty (Unit nm r formula ann) = lang <> parens (args `sepBy` comma) <> "."
    where
      lang = pretty (language formula)
      args = [either pretty pretty nm, keyword r, pretty formula]
          ++ case ann of
               Just (s, Just i)  -> [pretty s, pretty i]
               Just (s, Nothing) -> [pretty s]
               Nothing -> []

instance Pretty Derivation where
  pretty (Derivation us) = sep (fmap pretty us)
