{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module       : Data.TPTP
-- Description  : Data type definitions for the syntax of the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--
-- See [the BNF grammar](http://tptp.cs.miami.edu/TPTP/SyntaxBNF.html)
-- definition of the TPTP language for details.
--

module Data.TPTP (
  -- * Languages
  Language(..),

  -- * Names
  Atom(..),
  isValidAtom,

  Var(..),
  isValidVar,

  DistinctObject(..),
  isValidDistinctObject,

  Reserved(..),
  extended,
  isValidReserved,

  Named(..),

  Function(..),
  Predicate(..),

  Name(..),

  -- * Sorts and types
  Sort(..),
  TFF1Sort(..),
  monomorphizeTFF1Sort,
  Type(..),
  tff1Type,

  -- * First-order logic
  Number(..),
  Term(..),
  Literal(..),
  Sign(..),
  Clause(..),
  Quantifier(..),
  Connective(..),
  isAssociative,
  FirstOrder(..),
  Unsorted(..),
  Sorted(..),
  QuantifiedSort(..),
  UnsortedFirstOrder,
  SortedFirstOrder,
  MonomorphicFirstOrder,
  PolymorphicFirstOrder,
  monomorphizeFirstOrder,

  -- * Units
  Formula(..),
  formulaLanguage,
  Role(..),
  Declaration(..),
  declarationLanguage,
  UnitName,
  Unit(..),
  TPTP(..),

  -- * Annotations
  Intro(..),
  Source(..),
  Status(..),
  Parent(..),
  Expression(..),
  Info(..),
  Annotation
) where

import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.String (IsString, fromString)
import qualified Data.Text as Text (all, null, head, tail)
import Data.Text (Text)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :load Data.TPTP.Pretty
-- >>> import Test.QuickCheck


-- * Languages

-- | The language of logical formulas available in TPTP.
data Language
  = CNF_ -- ^ __CNF__ - the language of clausal normal forms of
         -- unsorted first-order logic.
  | FOF_ -- ^ __FOF__ - the language of full unsorted first-order logic.
  | TFF_ -- ^ __TFF__ - the language of full sorted first-order logic,
         -- both monomorphic (TFF0) and polymorphic (TFF1).
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Named Language where
  name = \case
    CNF_ -> "cnf"
    FOF_ -> "fof"
    TFF_ -> "tff"


-- * Names

-- | The atomic word in the TPTP language - a non-empty string of space or
-- visible characters from the ASCII range 0x20 to 0x7E. If the string satisfies
-- the regular expression @[a-z][a-zA-Z0-9_]*@ it is displayed in the TPTP
-- language as is, otherwise it is displayed in single quotes with the
-- characters @'@ and @\\@ escaped using @\\@.
--
-- >>> print (pretty (Atom "fxYz42"))
-- fxYz42
--
-- >>> print (pretty (Atom "f-'function symbol'"))
-- 'f-\'function symbol\''
--
newtype Atom = Atom Text
  deriving (Eq, Show, Ord, IsString)

-- | Check whether a given character is in the ASCII range 0x20 to 0x7E.
isAsciiPrint :: Char -> Bool
isAsciiPrint c = isAscii c && isPrint c

-- | Check whether a given string is a valid atom.
--
-- >>> isValidAtom ""
-- False
--
-- >>> isValidAtom "\r\n"
-- False
--
-- >>> isValidAtom "fxYz42"
-- True
--
-- >>> isValidAtom "f-'function symbol'"
-- True
isValidAtom :: Text -> Bool
isValidAtom t = not (Text.null t)
             && Text.all isAsciiPrint t

-- | The variable in the TPTP language - a string that satisfies the regular
-- expression @[A-Z][a-zA-Z0-9_]*@.
newtype Var = Var Text
  deriving (Eq, Show, Ord, IsString)

-- | Check whether a given character matches the regular expression
-- @[a-zA-Z0-9_]@.
isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

-- | Check whether a given string is a valid variable.
--
-- >>> isValidVar ""
-- False
--
-- >>> isValidVar "x"
-- False
--
-- >>> isValidVar "X"
-- True
--
-- >>> isValidVar "Cat"
-- True
--
-- >>> isValidVar "C@t"
-- False
isValidVar :: Text -> Bool
isValidVar t = not (Text.null t)
            && isAsciiUpper (Text.head t)
            && Text.all isAlphaNumeric (Text.tail t)

-- | The distinct object in the TPTP language - a (possibly empty) string of
-- space or visible characters from the ASCII range 0x20 to 0x7E. The string is
-- always displayed in the TPTP language in double quotes with the characters
-- @"@ and @\\@ escaped using @\\@.
--
-- >>> print (pretty (DistinctObject "Godel's incompleteness theorem"))
-- "Godel's incompleteness theorem"
--
-- Distinct objects are different from atoms in that they implicitly carry
-- semantic inequality. The TPTP documentation says the following about distinct
-- objects.
--
-- /Distinct objects are different from (but may be equal to) other tokens,/
-- /e.g.,/ @"cat"@ /is different from/ @\'cat\'@ /and/ @cat@. /Distinct objects/
-- /are always interpreted as themselves, so if they are different they are/
-- /unequal, e.g.,/ @\"Apple\" != \"Microsoft\"@ /is implicit./
newtype DistinctObject = DistinctObject Text
  deriving (Eq, Show, Ord, IsString)

-- | Check whether a given string is a valid distinct object.
--
-- >>> isValidDistinctObject ""
-- True
--
-- >>> isValidDistinctObject "Godel's incompleteness theorem"
-- True
--
-- >>> isValidDistinctObject "\r\n"
-- False
isValidDistinctObject :: Text -> Bool
isValidDistinctObject = Text.all isAsciiPrint

-- | The identifier reserved in the TPTP specification and theorem proving
-- systems that implement it. Reserved identifiers are used to represent
-- function symbols, predicate symbols, sorts, formula roles and others.
-- Reserved identifiers are non-empty strings that satisfy the regular
-- expression @[a-z][a-zA-Z0-9_]*@. Reserved identifiers of functions,
-- predicates, and sorts, used as names, are in addition prepended by @$@.
--
-- >>> print (pretty (Standard I))
-- i
--
-- >>> print (pretty (Standard Axiom))
-- axiom
--
-- >>> print (pretty (Extended "negated_lemma" :: Reserved Role))
-- negated_lemma
data Reserved s
  = Standard s    -- ^ The identifier contained in the TPTP specification.
  | Extended Text -- ^ The identifier not contained in the standard TPTP but
                  -- implemented by some theorem prover. For example, Vampire
                  -- implements uses the sort constructor @$array@.
  deriving (Eq, Show, Ord)

-- | A smart 'Extended' constructor - only uses 'Extended' if the given string
-- does not correspond to any of the standard identifiers.
--
-- >>> extended "int" :: Reserved Sort
-- Standard Int
--
-- >>> extended "array" :: Reserved Sort
-- Extended "array"
extended :: (Named a, Enum a, Bounded a) => Text -> Reserved a
extended t
  | Just a <- find (\a -> name a == t) [minBound..] = Standard a
  | otherwise = Extended t

instance (Named a, Enum a, Bounded a) => IsString (Reserved a) where
  fromString = extended . fromString

-- | Check whether a given string is a valid reserved identifier.
--
-- >>> isValidReserved ""
-- False
--
-- >>> isValidReserved "x"
-- True
--
-- >>> isValidReserved "X"
-- False
--
-- >>> isValidReserved "cat"
-- True
--
-- >>> isValidReserved "c@t"
-- False
--
-- >>> isValidReserved "$int"
-- False
--
isValidReserved :: Text -> Bool
isValidReserved t = not (Text.null t)
                 && isAsciiLower (Text.head t)
                 && Text.all isAlphaNumeric (Text.tail t)

-- | The class 'Named' allows assigning concrete names to reserved constants
-- in the TPTP language.
class Named a where
  name :: a -> Text

-- | The standard function symbol in TPTP.
-- Represents an operation in a first-order theory of arithmetic.
data Function
  = Uminus
  | Sum
  | Difference
  | Product
  | Quotient
  | QuotientE
  | QuotientT
  | QuotientF
  | RemainderE
  | RemainderT
  | RemainderF
  | Floor
  | Ceiling
  | Truncate
  | Round
  | ToInt
  | ToRat
  | ToReal
  deriving (Eq, Show, Ord, Enum, Bounded)

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

-- | The standard predicate symbol in TPTP.
data Predicate
  = Tautology
  | Falsum
  | Distinct
  | Less
  | Lesseq
  | Greater
  | Greatereq
  | IsInt
  | IsRat
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Named Predicate where
  name = \case
    Tautology -> "true"
    Falsum    -> "false"
    Distinct  -> "distinct"
    Less      -> "less"
    Lesseq    -> "lesseq"
    Greater   -> "greater"
    Greatereq -> "greatereq"
    IsInt     -> "is_int"
    IsRat     -> "is_rat"

-- | The name of a function symbol, a predicate symbol, a sort, a formula role
-- or other.
--
-- > >>> print (pretty (Reserved (Standard I)))
-- > $i
--
-- > >>> print (pretty (Reserved (Extended "array" :: Reserved Sort)))
-- > $array
--
-- >>> print (pretty (Defined (Atom "array") :: Name Sort))
-- array
data Name s
  = Reserved (Reserved s) -- ^ The name reserved in the TPTP specification.
                          -- This name is parsed and pretty printed with the
                          -- leading @$@ character.
  | Defined Atom          -- ^ The name defined by the user.
  deriving (Eq, Show, Ord)

-- | The 'IsString' instance of 'Name' opts for using the 'Defined' constructor.
instance IsString (Name s) where
  fromString = Defined . fromString


-- * Sorts and types

-- | The standard sort in TPTP.
data Sort
  = I    -- ^ The sort of individuals.
  | O    -- ^ The sort of booleans.
  | Int  -- ^ The sort of integers.
  | Real -- ^ The sort of real numbers.
  | Rat  -- ^ The sort of rational numbers.
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Named Sort where
  name = \case
    I    -> "i"
    O    -> "o"
    Int  -> "int"
    Real -> "real"
    Rat  -> "rat"

-- | The sort in sorted rank-1 polymorphic logic with sort constructors (TFF1) -
-- an application of a sort constructor to zero or more sorts or a sort variable
-- that comes from a sort quantifier. A zero-arity sort application is simply a
-- sort.
--
-- Every TFF0 sort is also a TFF1 sort, but not the other way around.
data TFF1Sort
  = SortVariable Var
  | TFF1Sort (Name Sort) [TFF1Sort]
  deriving (Eq, Show, Ord)

-- | Attempt to convert a given TFF1 sort to TFF0. This function succeeds iff
-- the given sort is a sort constructor with zero arity.
monomorphizeTFF1Sort :: TFF1Sort -> Maybe (Name Sort)
monomorphizeTFF1Sort = \case
  TFF1Sort f [] -> Just f
  _ -> Nothing

-- | The type of a function or a predicate symbol in a sorted first-order logic
-- (TFF0 or TFF1). Each TFF0 type is also a TFF1 type, but not the other way
-- around.
data Type
  -- | The type of a function or a predicate symbol in the sorted monomorphic
  -- first-order logic (TFF0). It is a mapping of zero or more sorts to a sort.
  -- The empty list of argument sorts marks the type of a constant symbol.
  = Type [Name Sort] (Name Sort)

  -- | The type of a function or a predicate symbol in the sorted rank-1
  -- polymorphic first-order logic (TFF1). It is a (possibly quantified)
  -- mapping of zero or more TFF1 sorts to a TFF1 sort. The empty list of sort
  -- variables marks a monomorphic TFF1 type. The empty list of argument sorts
  -- marks the type of a constant symbol.
  | TFF1Type [Var] [TFF1Sort] TFF1Sort

  deriving (Eq, Show, Ord)

-- | A smart constructor of a TFF1 type. 'tff1Type' constructs a TFF0 type with
-- its arguments, if it is possible, and otherwise constructs a TFF1 type.
tff1Type :: [Var] -> [TFF1Sort] -> TFF1Sort -> Type
tff1Type [] ss s
  | Just ss' <- traverse monomorphizeTFF1Sort ss
  , Just s'  <- monomorphizeTFF1Sort s = Type ss' s'
tff1Type vs ss s = TFF1Type vs ss s


-- * First-order logic

-- | The integer, rational, or real constant.
data Number
  = IntegerConstant Integer
  -- ^ A positive or negative integer.
  | RationalConstant Integer Integer
  -- ^ A rational number, represented as a pair of its numerator (positive or
  -- negative integer, possibly zero) and denominator (strictly positive
  -- non-zero integer).
  | RealConstant Scientific
  -- ^ A real number, written in the scientific notation.
  deriving (Eq, Show, Ord)

-- | The term in first-order logic extended with arithmetic.
data Term
  = Function (Name Function) [Term]
    -- ^ Application of a function symbol. The empty list of arguments
    -- represents a constant function symbol.
  | Variable Var
    -- ^ A quantified variable.
  | Number Number
    -- ^ An integer, rational or real constant.
  | DistinctTerm DistinctObject
    -- ^ A distinct object.
  deriving (Eq, Show, Ord)

-- | The sign of first-order literals and equality.
data Sign
  = Positive
  | Negative
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Named Sign where
  name = \case
    Positive -> "="
    Negative -> "!="

-- | The literal in first-order logic.
-- The logical tautology is represented as
-- 'Predicate (Reserved (Standard Tautology)) []'
-- and the logical falsum is represented as
-- 'Predicate (Reserved (Standard Falsum)) []'.
data Literal
  = Predicate (Name Predicate) [Term]
    -- ^ Application of a predicate symbol.
  | Equality Term Sign Term
    -- ^ Equality or inequality.
  deriving (Eq, Show, Ord)

-- | The clause in first-order logic - implicitly universally-quantified
-- disjunction of one or more signed literals. Semantically, a clause is allowed
-- to be empty in which case it is the logical falsum. However, the TPTP syntax
-- does not allow empty clauses, instead the unit clause @$false@ must be used.
newtype Clause = Clause (NonEmpty (Sign, Literal))
  deriving (Eq, Show, Ord)

-- | The quantifier in first-order logic.
data Quantifier
  = Forall -- ^ The universal quantifier.
  | Exists -- ^ The existential quantifier.
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Named Quantifier where
  name = \case
    Forall -> "!"
    Exists -> "?"

-- | The connective in full first-order logic.
data Connective
  = Conjunction
  | Disjunction
  | Implication
  | Equivalence
  | ExclusiveOr
  | NegatedConjunction
  | NegatedDisjunction
  | ReversedImplication
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Check associativity of a given connective.
--
-- >>> isAssociative Implication
-- False
--
-- >>> isAssociative Conjunction
-- True
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

-- | The formula in sorted or unsorted first-order logic.
-- Syntactically, the difference between sorted and unsorted formulas is that
-- quantified variables in the former might be annotated with their respective
-- sorts. The type parameter @s@ represents the sort annotation - it is empty
-- for unsorted logic and non-empty for sorted logic.
data FirstOrder s
  = Atomic Literal
  | Negated (FirstOrder s)
  | Connected (FirstOrder s) Connective (FirstOrder s)
  | Quantified Quantifier (NonEmpty (Var, s)) (FirstOrder s)
  deriving (Eq, Show, Ord, Functor, Traversable, Foldable)

-- | The (empty) sort annotation in unsorted first-order logic.
newtype Unsorted = Unsorted ()
  deriving (Eq, Show, Ord)

-- | The formula in unsorted first-order logic.
type UnsortedFirstOrder = FirstOrder Unsorted

-- | The sort annotation in sorted first-order logic. The TPTP language allows
-- a sort annotation to be omitted, in such case the sort of the variable is
-- assumed to be @$i@.
newtype Sorted s = Sorted (Maybe s)
  deriving (Eq, Show, Ord, Functor, Traversable, Foldable)

-- | An alias for 'MonomorphicFirstOrder'.
type SortedFirstOrder = MonomorphicFirstOrder

-- | The formula in sorted monomorphic first-order logic.
type MonomorphicFirstOrder = FirstOrder (Sorted (Name Sort))

-- | The marker of quantified sort.
newtype QuantifiedSort = QuantifiedSort ()
  deriving (Eq, Show, Ord)

-- | The formula in sorted polymorphic first-order logic.
type PolymorphicFirstOrder = FirstOrder (Sorted (Either QuantifiedSort TFF1Sort))

-- | Attempt to monomorphize a polymorphic sorted first-order formula.
-- This function succeeds iff each of the quantifiers only uses sort
-- constructors with zero arity.
monomorphizeFirstOrder :: PolymorphicFirstOrder -> Maybe MonomorphicFirstOrder
monomorphizeFirstOrder = traverse . traverse
                       $ either (const Nothing) monomorphizeTFF1Sort


-- * Units

-- | The formula in either of the supported TPTP languages.
data Formula
  = CNF Clause
  | FOF UnsortedFirstOrder
  | TFF0 MonomorphicFirstOrder
  | TFF1 PolymorphicFirstOrder
  deriving (Eq, Show, Ord)

-- | The TPTP language of a given TPTP formula.
formulaLanguage :: Formula -> Language
formulaLanguage = \case
  CNF{}  -> CNF_
  FOF{}  -> FOF_
  TFF0{} -> TFF_
  TFF1{} -> TFF_

-- | The predefined role of a formula in a derivation. Theorem provers might
-- introduce other roles.
data Role
  = Axiom
  | Hypothesis
  | Definition
  | Assumption
  | Lemma
  | Theorem
  | Corollary
  | Conjecture
  | NegatedConjecture
  | Plain
  | FiDomain
  | FiFunctors
  | FiPredicates
  | Unknown
  deriving (Eq, Show, Ord, Enum, Bounded)

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

-- | The logical declaration.
data Declaration
  = Sort Atom Integer
  -- ^ Introduction of a sort contructor. The non-negative integer argument
  -- denotes the arity of the constructor. A constructor with zero arity is
  -- simply a sort.
  | Typing Atom Type
  -- ^ Assignment of a type to a symbol.
  | Formula (Reserved Role) Formula
  -- ^ Logical formula marked with its role.
  deriving (Eq, Show, Ord)

-- | The TPTP language of a given TPTP declaration.
declarationLanguage :: Declaration -> Language
declarationLanguage = \case
  Sort{}      -> TFF_
  Typing{}    -> TFF_
  Formula _ f -> formulaLanguage f

-- | The name of a unit - either an atom or an integer.
type UnitName = Either Atom Integer

-- | The unit of the TPTP input.
data Unit
  = Include Atom (Maybe (NonEmpty UnitName))
  -- ^ The @include@ statement.
  | Unit UnitName Declaration (Maybe Annotation)
  -- ^ The named and possibly annotated logical declaration.
  deriving (Eq, Show, Ord)

-- | The TPTP input - zero or more TPTP units.
newtype TPTP = TPTP {
  units :: [Unit]
} deriving (Eq, Show, Ord)


-- * Annotations

-- | The marking of the way a formula is introduced in a TSTP proof.
-- TPTP recognizes several standard intros and theorem proving systems might use
-- other ones.
data Intro
  = ByDefinition
  | ByAxiomOfChoice
  | ByTautology
  | ByAssumption
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Named Intro where
  name = \case
    ByDefinition    -> "definition"
    ByAxiomOfChoice -> "axiom_of_choice"
    ByTautology     -> "tautology"
    ByAssumption    -> "assumption"

-- | The source of a unit in a TSTP proof. Most commonly a formula is either
-- defined in a 'File' or is the result of an 'Inference'.
data Source
  = File Atom (Maybe UnitName)
  | Theory Atom (Maybe [Info])
  | Creator Atom (Maybe [Info])
  | Introduced (Reserved Intro) (Maybe [Info])
  | Inference Atom [Info] [Parent]
  | UnitSource UnitName
  | UnknownSource
  deriving (Eq, Show, Ord)

-- | The status of an inference.
-- See <http://www.tptp.org/Seminars/SZSOntologies/Summary.html The SZS Ontologies>
-- for details.
data Status
  = SUC | UNP | SAP | ESA | SAT | FSA | THM | EQV | TAC | WEC | ETH | TAU | WTC
  | WTH | CAX | SCA | TCA | WCA | CUP | CSP | ECS | CSA | CTH | CEQ | UNC | WCC
  | ECT | FUN | UNS | WUC | WCT | SCC | UCA | NOC
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Named Status where
  name = \case
    SUC -> "suc"
    UNP -> "unp"
    SAP -> "sap"
    ESA -> "esa"
    SAT -> "sat"
    FSA -> "fsa"
    THM -> "thm"
    EQV -> "eqv"
    TAC -> "tac"
    WEC -> "wec"
    ETH -> "eth"
    TAU -> "tau"
    WTC -> "wtc"
    WTH -> "wth"
    CAX -> "cax"
    SCA -> "sca"
    TCA -> "tca"
    WCA -> "wca"
    CUP -> "cup"
    CSP -> "csp"
    ECS -> "ecs"
    CSA -> "csa"
    CTH -> "cth"
    CEQ -> "ceq"
    UNC -> "unc"
    WCC -> "wcc"
    ECT -> "ect"
    FUN -> "fun"
    UNS -> "uns"
    WUC -> "wuc"
    WCT -> "wct"
    SCC -> "scc"
    UCA -> "uca"
    NOC -> "noc"

-- | The parent of a formula in an inference.
data Parent = Parent Source [Info]
  deriving (Eq, Show, Ord)

-- | An expression is either a formula or a term.
-- Expressions occur in TSTP proofs.
data Expression
  = Logical Formula
  | Term Term
  deriving (Eq, Show, Ord)

-- | The information about a formula.
data Info
  = Description Atom
  | Iquote Atom
  | Status (Reserved Status)
  | Assumptions (NonEmpty UnitName)
  | NewSymbols Atom [Either Var Atom]
  | Refutation Atom
  | Expression Expression
  | Bind Var Expression
  | Application Atom [Info]
  | InfoNumber Number
  | Infos [Info]
  deriving (Eq, Show, Ord)

-- | The annotation of a unit. Most commonly, annotations are attached to units
-- in TSTP proofs.
type Annotation = (Source, Maybe [Info])
