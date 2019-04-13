{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module       : Data.TPTP.Internal
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TPTP.Internal where

import Data.Text (Text)

import Data.TPTP

-- * Names

class Named a where
  name :: a -> Text

instance Named Function where
  name = \case
    Uminus     -> "uminus"
    Sum        -> "sum"
    Difference -> "difference"
    Product    -> "product"
    Quotient   -> "quotient"
    QuotientE  -> "quotient_e"
    QuotientT  -> "quotient_t"
    QuotientF  -> "quotient_f"
    RemainderE -> "remainder_e"
    RemainderT -> "remainder_t"
    RemainderF -> "remainder_f"
    Floor      -> "floor"
    Ceiling    -> "ceiling"
    Truncate   -> "truncate"
    Round      -> "round"
    ToInt      -> "to_int"
    ToRat      -> "to_rat"
    ToReal     -> "to_real"

instance Named Predicate where
  name = \case
    Distinct  -> "distinct"
    Less      -> "less"
    Lesseq    -> "lesseq"
    Greater   -> "greater"
    Greatereq -> "greatereq"
    IsInt     -> "is_int"
    IsRat     -> "is_rat"

instance Named Sort where
  name = \case
    I    -> "i"
    O    -> "o"
    Int  -> "int"
    Real -> "real"
    Rat  -> "rat"

instance Named Sign where
  name = \case
    Positive -> "="
    Negative -> "!="

instance Named Quantifier where
  name = \case
    Forall -> "!"
    Exists -> "?"

instance Named Connective where
  name = \case
    Conjunction -> "&"
    Disjunction -> "|"
    Implication -> "=>"
    Equivalence -> "<=>"
    ExclusiveOr -> "<~>"
    NegatedConjunction  -> "~&"
    NegatedDisjunction  -> "~|"
    ReversedImplication -> "<="

isAssociative :: Connective -> Bool
isAssociative = \case
  Conjunction -> True
  Disjunction -> True
  Implication -> False
  Equivalence -> False
  ExclusiveOr -> False
  NegatedConjunction  -> False
  NegatedDisjunction  -> False
  ReversedImplication -> False

instance Named Role where
  name = \case
    Axiom             -> "axiom"
    Hypothesis        -> "hypothesis"
    Definition        -> "definition"
    Assumption        -> "assumption"
    Lemma             -> "lemma"
    Theorem           -> "theorem"
    Corollary         -> "corollary"
    Conjecture        -> "conjecture"
    NegatedConjecture -> "negated_conjecture"
    Plain             -> "plain"
    FiDomain          -> "fi_domain"
    FiFunctors        -> "fi_functors"
    FiPredicates      -> "fi_predicates"
    Unknown           -> "unknown"

instance Named Intro where
  name = \case
    ByDefinition  -> "definition"
    AxiomOfChoice -> "axiom_of_choice"
    ByTautology   -> "tautology"
    ByAssumption  -> "assumption"

-- * TPTP languages

-- | The language of logical formulas supported by TPTP.
data Language
  = CNF_ -- ^ Clausal normal form
  | FOF_ -- ^ Unsorted first-order form
  | TFF_ -- ^ Sorted first-order form
  -- | THF_ -- ^ Higher-order form
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Named Language where
  name = \case
    CNF_ -> "cnf"
    FOF_ -> "fof"
    TFF_ -> "tff"
    -- THF_ -> "thf"

language :: Declaration -> Language
language = \case
  Sort    _ _ -> TFF_
  Typing  _ t -> typeLanguage t
  Formula _ f -> formulaLanguage f

formulaLanguage :: Formula -> Language
formulaLanguage = \case
  CNF{} -> CNF_
  FOF{} -> FOF_
  TFF{} -> TFF_

typeLanguage :: Type -> Language
typeLanguage = \case
  Monomorphic{} -> TFF_
  Polymorphic{} -> TFF_
