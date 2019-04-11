-- |
-- Module       : Data.TPTP
-- Description  : Data type definitions for the syntax of the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--
-- The implementation of this module follows the
-- [BNF grammar](http://tptp.cs.miami.edu/TPTP/SyntaxBNF.html)
-- definition of the TPTP language.
--

module Data.TPTP (
  -- * Names
  Atom(..),
  isValidAtom,

  Var(..),
  isValidVar,

  DistinctObject(..),
  isValidDistinctObject,

  Name(..),
  Function(..),
  Predicate(..),
  Sort(..),

  -- * First-order logic
  Number(..),
  Term(..),
  Literal(..),
  Sign(..),
  Clause(..),
  Quantifier(..),
  Connective(..),
  FirstOrder(..),
  Unsorted(..),
  Sorted(..),
  UnsortedFirstOrder,
  SortedFirstOrder,

  -- * Units
  Formula(..),
  Role(..),
  Type(..),
  Declaration(..),
  Unit(..),
  Derivation(..),

  -- * Annotations
  Intro(..),
  Source(..),
  Parent(..),
  GeneralTerm(..),
  GeneralData(..),
  Info(..),
  Annotation
) where

import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import qualified Data.Text as Text
import Data.Text (Text)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :load Data.TPTP.Pretty
-- >>> import Test.QuickCheck


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
  deriving (Eq, Show, Ord)

-- | Check whether a given character is in the ASCII range 0x20 to 0x7E.
isAsciiPrint :: Char -> Bool
isAsciiPrint c = isAscii c && isPrint c

-- | Check whether a given string is a valid atom.
--
-- prop> isValidAtom "" == False
-- prop> isValidAtom "\r\n" == False
-- prop> isValidAtom "fxYz42" == True
-- prop> isValidAtom "f-'function symbol'" == True
isValidAtom :: Text -> Bool
isValidAtom t = not (Text.null t)
             && Text.all isAsciiPrint t

-- | The variable in the TPTP language - a string that satisfies the regular
-- expression @[A-Z][a-zA-Z0-9_]*@.
newtype Var = Var Text
  deriving (Eq, Show, Ord)

-- | Check whether a given character matches the regular expression
-- @[a-zA-Z0-9_]@.
isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

-- | Check whether a given string is a valid variable.
--
-- prop> isValidVar "" == False
-- prop> isValidVar "x" == False
-- prop> isValidVar "X" == True
-- prop> isValidVar "Cat" == True
-- prop> isValidVar "C@t" == False
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
  deriving (Eq, Show, Ord)

-- | Check whether a given string is a valid distinct object.
--
-- prop> isValidDistinctObject "" == True
-- prop> isValidDistinctObject "Godel's incompleteness theorem" == True
-- prop> isValidDistinctObject "\r\n" == False
isValidDistinctObject :: Text -> Bool
isValidDistinctObject = Text.all isAsciiPrint

-- | The name of a function symbol, a predicate symbol or a sort in TPTP.
data Name s
  = Reserved s   -- ^ The name reserved in the TPTP specification.
  | Defined Atom -- ^ The name defined by the user.
  deriving (Eq, Show, Ord)

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

-- | The standard predicate symbol in TPTP.
-- Represents an operation in a first-order theory of arithmetic.
data Predicate
  = Distinct
  | Less
  | Lesseq
  | Greater
  | Greatereq
  | IsInt
  | IsRat
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | The standard sort in TPTP.
data Sort
  = I    -- ^ The type of individuals.
  | O    -- ^ The type of booleans.
  | Int  -- ^ The type of integers.
  | Real -- ^ The type of real numbers.
  | Rat  -- ^ The type of rational numbers.
  deriving (Eq, Show, Ord, Enum, Bounded)


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

-- | The literal in first-order logic.
data Literal
  = Predicate (Name Predicate) [Term]
    -- ^ Application of a predicate symbol.
  | Equality Term Sign Term
    -- ^ Equality or inequality.
  | Tautology
    -- ^ The logical truth, represented in TPTP as @$true@.
  | Falsum
    -- ^ The logical contradiction, represented in TPTP as @$false@.
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
  deriving (Eq, Show, Ord)

-- | The (empty) sort annotation in unsorted first-order logic.
newtype Unsorted = Unsorted ()
  deriving (Eq, Show, Ord)

-- | The formula in unsorted first-order logic.
type UnsortedFirstOrder = FirstOrder Unsorted

-- | The sort annotation in sorted first-order logic. The TPTP language allows
-- a sort annotation to be omitted, in such case the sort of the variable is
-- assumed to be @$i@.
newtype Sorted = Sorted (Maybe (Name Sort))
  deriving (Eq, Show, Ord)

-- | The formula in sorted first-order logic.
type SortedFirstOrder = FirstOrder Sorted


-- * Units

-- | The formula in either of the supported TPTP languages.
data Formula
  = CNF Clause
  | FOF UnsortedFirstOrder
  | TFF SortedFirstOrder
  deriving (Eq, Show, Ord)

-- | The predefined role of a formula in a TPTP derivation. Theorem provers
-- might introduce other roles.
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

-- | The type in TPTP is a mapping of one or more sorts to a sort.
-- Types are assigned to function and predicate symbols in the sorted
-- languages of TPTP.
data Type
  = TFFType [Name Sort] (Name Sort)
  -- ^ The type of a function or a predicate symbol in the sorted
  -- first-order logic.
  deriving (Eq, Show, Ord)

-- | The logical declaration.
data Declaration
  = Sort Atom
  -- ^ Introduction of a sort.
  | Typing Atom Type
  -- ^ Assignment of a type to a symbol.
  | Formula (Name Role) Formula
  -- ^ Logical formula marked with its role.
  deriving (Eq, Show, Ord)

-- | The unit of TPTP input.
data Unit
  = Include Atom
  -- ^ The @include@ statement.
  | Unit (Either Atom Integer) Declaration (Maybe Annotation)
  -- ^ THe named and possibly annotated logical declaration.
  deriving (Eq, Show, Ord)

-- | The derivation in TPTP - the list of zero or more units.
newtype Derivation = Derivation {
  units :: [Unit]
} deriving (Eq, Show, Ord)


-- * Annotations

data Intro
  = ByDefinition
  | AxiomOfChoice
  | ByTautology
  | ByAssumption
  deriving (Eq, Show, Ord, Enum, Bounded)

data Source
  = File Atom (Maybe Atom)
  | Theory Atom (Maybe Info)
  | Creator Atom (Maybe Info)
  | Introduced Intro (Maybe Info)
  | Inference Atom Info [Parent]
  | Dag Atom
  | UnknownSource
  | Sources (NonEmpty Source)
  deriving (Eq, Show, Ord)

data Parent = Parent Source [GeneralTerm]
  deriving (Eq, Show, Ord)

data GeneralData
  = GeneralFunction Atom [GeneralTerm]
  | GeneralVariable Var
  | GeneralNumber Number
  | GeneralFormula Formula
  | GeneralBind Var Formula
  | GeneralDistinct DistinctObject
  deriving (Eq, Show, Ord)

data GeneralTerm
  = GeneralData GeneralData (Maybe GeneralTerm)
  | GeneralList [GeneralTerm]
  deriving (Eq, Show, Ord)

newtype Info = Info [GeneralTerm]
  deriving (Eq, Show, Ord)

type Annotation = (Source, Maybe Info)
