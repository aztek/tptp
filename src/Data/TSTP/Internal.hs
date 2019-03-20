{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Data.TSTP.Internal where

import Data.Char (chr)
import Data.Text (Text)

import Data.TSTP

-- * Names

class Named a where
  name :: a -> Text

instance Named StandardFunction where
  name = \case
    Uminus     -> "$uminus"
    Sum        -> "$sum"
    Difference -> "$difference"
    Product    -> "$product"
    Quotient   -> "$quotient"
    QuotientE  -> "$quotient_e"
    QuotientT  -> "$quotient_t"
    QuotientF  -> "$quotient_f"
    RemainderE -> "$remainder_e"
    RemainderT -> "$remainder_t"
    RemainderF -> "$remainder_f"
    Floor      -> "$floor"
    Ceiling    -> "$ceiling"
    Truncate   -> "$truncate"
    Round      -> "$round"
    ToInt      -> "$to_int"
    ToRat      -> "$to_rat"
    ToReal     -> "$to_real"

instance Named StandardPredicate where
  name = \case
    Distinct  -> "$distinct"
    Less      -> "$less"
    Lesseq    -> "$lesseq"
    Greater   -> "$greater"
    Greatereq -> "$greatereq"
    IsInt     -> "$is_int"
    IsRat     -> "$is_rat"

instance Named StandardSort where
  name = \case
    I    -> "$i"
    O    -> "$o"
    Int  -> "$int"
    Real -> "$real"
    Rat  -> "$rat"

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

instance Named StandardRole where
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
    RoleType          -> "type"
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

-- * Lexem classes in TPTP

lowerAlpha :: String
lowerAlpha = ['a'..'z']

upperAlpha :: String
upperAlpha = ['A'..'Z']

numeric :: String
numeric    = ['0'..'9']

alphaNumeric :: String
alphaNumeric = '_' : lowerAlpha ++ upperAlpha ++ numeric

sg :: String
sg = [chr 0o40..chr 0o46] ++ [chr 0o50..chr 0o176]

-- * TSTP languages

-- | The language of logical formulas supported by TSTP.
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

language :: Formula -> Language
language = \case
  CNF{} -> CNF_
  FOF{} -> FOF_
  TFF{} -> TFF_
  -- THF{} -> THF_
