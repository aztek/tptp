{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module       : Data.TPTP.Parse.Combinators
-- Description  : Parser combinators for the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TPTP.Parse.Combinators (
  -- * Whitespace
  whitespace,

  -- * Names
  atom,
  var,
  distinctObject,
  function,
  predicate,

  -- * Sorts and types
  sort,
  tff1Sort,
  type_,

  -- * First-order logic
  number,
  term,
  literal,
  clause,
  unsortedFirstOrder,
  sortedFirstOrder,
  monomorphicFirstOrder,
  polymorphicFirstOrder,

  -- * Units
  unit,
  tptp,

  -- * Annotations
  intro,
  parent,
  source,
  info
) where

import Control.Applicative ((<|>), optional)

import Data.Attoparsec.Text as Atto hiding (Number, number)
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (sortBy, genericLength)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL (fromList, toList)

import qualified Data.Scientific as Sci (base10Exponent, coefficient)

import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack, cons)

import Data.TPTP hiding (name)
import qualified Data.TPTP as TPTP (name)


-- * Helper functions

-- | Consume a single line comment - characters between @%@ and newline.
comment :: Parser ()
comment = char '%' *> skipWhile (not . isEndOfLine)
                   *> (endOfLine <|> endOfInput)
                  <?> "comment"

-- | Consume a block comments - characters between /* and */.
blockComment :: Parser ()
blockComment = string "/*" *> bc <?> "block comment"
  where
    bc = skipWhile (/= '*') *> (string "*/" $> () <|> bc)

-- | Consume white space and trailing comments.
whitespace :: Parser ()
whitespace =  skipSpace *> skipMany ((comment <|> blockComment) *> skipSpace)
          <?> "whitespace"

-- | @lexeme@ makes a given parser consume trailing whitespace. This function is
-- needed because off-the-shelf attoparsec parsers do not do it.
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
{-# INLINE lexeme #-}

-- | Parse an unsigned integer.
integer :: Parser Integer
integer = lexeme decimal <?> "integer"
{-# INLINE integer #-}

token :: Text -> Parser Text
token t = lexeme (string t) <?> "token " ++ Text.unpack t
{-# INLINE token #-}

op :: Char -> Parser Char
op c = lexeme (char c) <?> "operator " ++ [c]
{-# INLINE op #-}

parens :: Parser a -> Parser a
parens p = op '(' *> p <* op ')' <?> "parens"
{-# INLINE parens #-}

optionalParens :: Parser a -> Parser a
optionalParens p = parens p <|> p
{-# INLINE optionalParens #-}

brackets :: Parser a -> Parser a
brackets p = op '[' *> p <* op ']' <?> "brackets"
{-# INLINE brackets #-}

bracketList :: Parser a -> Parser [a]
bracketList p = brackets (p `sepBy` op ',') <?> "bracket list"
{-# INLINE bracketList #-}

bracketList1 :: Parser a -> Parser (NonEmpty a)
bracketList1 p =  NEL.fromList <$> brackets (p `sepBy1` op ',')
              <?> "bracket list 1"
{-# INLINE bracketList1 #-}

application :: Parser f -> Parser a -> Parser (f, [a])
application f a = (,) <$> f <*> option [] (parens (a `sepBy1` op ','))
{-# INLINE application #-}

labeled :: Text -> Parser a -> Parser a
labeled l p = token l *> parens p
{-# INLINE labeled #-}

comma :: Parser a -> Parser a
comma p = op ',' *> p
{-# INLINE comma #-}

maybeP :: Parser a -> Parser (Maybe a)
maybeP = optional . comma
{-# INLINE maybeP #-}

enum :: (Named a, Enum a, Bounded a) => Parser a
enum = choice
     $ fmap (\(n, c) -> token n $> c <?> "reserved " ++ Text.unpack n)
     $ sortBy (flip compare `on` fst)
     $ fmap (\c -> (TPTP.name c, c)) [minBound..]


-- * Parser combinators

-- ** Names

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

isAsciiPrint :: Char -> Bool
isAsciiPrint c = isAscii c && isPrint c

lowerWord, upperWord :: Parser Text
lowerWord = Text.cons <$> satisfy isAsciiLower <*> Atto.takeWhile isAlphaNumeric
upperWord = Text.cons <$> satisfy isAsciiUpper <*> Atto.takeWhile isAlphaNumeric

quoted :: Char -> Parser Text
quoted q =  Text.pack <$> (char q *> manyTill escaped (char q))
        <?> "quoted " ++ [q]
  where
    escaped =  char '\\' *> (char q $> q <|> char '\\' $> '\\')
           <|> satisfy isAsciiPrint

-- | Parse an atomic word. Single-quoted atoms are parsed without the single
-- quotes and with the characters @'@ and @\\@ unescaped.
atom :: Parser Atom
atom = Atom <$> lexeme (quoted '\'' <|> lowerWord) <?> "atom"
{-# INLINE atom #-}

-- | Parse a variable.
var :: Parser Var
var = Var <$> lexeme upperWord <?> "var"
{-# INLINE var #-}

-- | Parse a distinct object. Double quotes are not preserved and the characters
-- @'@ and @\\@ are unescaped.
distinctObject :: Parser DistinctObject
distinctObject = DistinctObject <$> lexeme (quoted '"') <?> "distinct object"
{-# INLINE distinctObject #-}

-- | Parse a reserved word.
reserved :: (Named a, Enum a, Bounded a) => Parser (Reserved a)
reserved = extended <$> lexeme lowerWord <?> "reserved"
{-# INLINE reserved #-}

name :: (Named a, Enum a, Bounded a) => Parser (Name a)
name =  Reserved <$> (char '$' *> reserved)
    <|> Defined  <$> atom
    <?> "name"

-- | Parser a function name.
function :: Parser (Name Function)
function = name <?> "function"
{-# INLINE function #-}

-- | Parse a predicate name.
predicate :: Parser (Name Predicate)
predicate = name <?> "predicate"
{-# INLINE predicate #-}


-- ** Sorts and typess

-- | Parse a sort.
sort :: Parser (Name Sort)
sort = name <?> "sort"
{-# INLINE sort #-}

-- | Parse a sort in sorted polymorphic logic.
tff1Sort :: Parser TFF1Sort
tff1Sort =  SortVariable <$> var
        <|> uncurry TFF1Sort <$> application sort tff1Sort
        <?> "tff1 sort"

mapping :: Parser a -> Parser ([a], a)
mapping s = (,) <$> option [] (args <* op '>') <*> s
  where
    args = fmap (:[]) s <|> parens (s `sepBy1` op '*')

-- | Parse a type.
type_ :: Parser Type
type_ =  uncurry . tff1Type
     <$> (maybe [] NEL.toList <$> optional prefix) <*> matrix
     <?> "type"
  where
    prefix = token "!>" *> bracketList1 sortVar <* op ':'
    sortVar = var <* op ':' <* token "$tType"
    matrix = optionalParens (mapping tff1Sort)


-- ** First-order logic

-- | Parse a number.
number :: Parser Number
number =  RationalConstant <$> signed integer <* char '/' <*> integer
      <|> real <$> lexeme scientific
      <?> "number"
  where
    real n
      | Sci.base10Exponent n == 0 = IntegerConstant (Sci.coefficient n)
      | otherwise = RealConstant n

-- | Parse a term.
term :: Parser Term
term =  parens term
    <|> uncurry Function <$> application function term
    <|> Variable         <$> var
    <|> Number           <$> number
    <|> DistinctTerm     <$> distinctObject
    <?> "term"

-- | Parse the equality and the unequality sign.
eq :: Parser Sign
eq = enum <?> "eq"
{-# INLINE eq #-}

-- | Parse a literal.
literal :: Parser Literal
literal =  parens literal
       <|> Equality <$> term <*> eq <*> term
       <|> uncurry Predicate <$> application predicate term
       <?> "literal"

-- | Parse the negation sign.
sign :: Parser Sign
sign = option Positive (op '~' $> Negative)
{-# INLINE sign #-}

-- | Parse a signed literal.
signedLiteral :: Parser (Sign, Literal)
signedLiteral = (,) <$> sign <*> literal <?> "signed literal"
{-# INLINE signedLiteral #-}

-- | Parse a clause.
clause :: Parser Clause
clause =  parens clause
      <|> Clause . NEL.fromList <$> signedLiteral `sepBy1` op '|'
      <?> "clause"

-- | Parse a quantifier.
quantifier :: Parser Quantifier
quantifier = enum <?> "quantifier"
{-# INLINE quantifier #-}

-- | Parse a logical connective.
connective :: Parser Connective
connective = enum <?> "connective"
{-# INLINE connective #-}

-- | Given a parser for sort annotations, parse a formula in first-order logic.
firstOrder :: Parser s -> Parser (FirstOrder s)
firstOrder p = do
  f <- unitary
  option f (Connected f <$> connective <*> firstOrder p)
  where
    unitary =  parens (firstOrder p)
           <|> Atomic     <$> literal
           <|> Quantified <$> quantifier <*> vs <* op ':' <*> unitary
           <|> Negated    <$> (op '~' *> unitary)
           <?> "unitary first order"

    vs = bracketList1 v
    v = (,) <$> var <*> p

-- | Parse a formula in unsorted first-order logic.
unsortedFirstOrder :: Parser UnsortedFirstOrder
unsortedFirstOrder = firstOrder unsorted
  where unsorted = pure (Unsorted ()) <?> "unsorted"

sorted :: Parser s -> Parser (Sorted s)
sorted s = Sorted <$> optional (op ':' *> s) <?> "sorted"

-- | An alias for 'monomorphicFirstOrder'.
sortedFirstOrder :: Parser SortedFirstOrder
sortedFirstOrder = monomorphicFirstOrder

-- | Parse a formula in sorted monomorphic first-order logic.
monomorphicFirstOrder :: Parser MonomorphicFirstOrder
monomorphicFirstOrder = firstOrder (sorted sort) <?> "tff0"

quantifiedSort :: Parser QuantifiedSort
quantifiedSort = token "$tType" $> QuantifiedSort ()

-- | Parse a formula in sorted polymorphic first-order logic.
polymorphicFirstOrder :: Parser PolymorphicFirstOrder
polymorphicFirstOrder =  firstOrder (sorted (eitherP quantifiedSort tff1Sort))
                     <?> "tff1"


-- ** Units

-- | Parse a formula in a given TPTP language.
formula :: Language -> Parser Formula
formula = \case
  CNF_ -> CNF <$> clause <?> "cnf"
  FOF_ -> FOF <$> unsortedFirstOrder <?> "fof"
  TFF_ -> tff <$> polymorphicFirstOrder <?> "tff"
  where
    tff f = case monomorphizeFirstOrder f of
     Just f' -> TFF0 f'
     Nothing -> TFF1 f

-- | Parse a formula role.
role :: Parser (Reserved Role)
role = reserved <?> "role"
{-# INLINE role #-}

-- | Parse the name of a TPTP language.
language :: Parser Language
language = enum <?> "language"
{-# INLINE language #-}

-- | Parse a TPTP declaration in a given language.
declaration :: Language -> Parser Declaration
declaration l =  token "type" *> comma (optionalParens typeDeclaration)
             <|> Formula <$> role <*> comma (formula l)
             <?> "declaration"

-- | Parse a declaration with the @type@ role - either a typing relation or
-- a sort declaration.
typeDeclaration :: Parser Declaration
typeDeclaration =  Sort   <$> atom <* op ':' <*> arity
               <|> Typing <$> atom <* op ':' <*> type_
               <?> "type declaration"
  where
    arity = genericLength . fst <$> mapping (token "$tType")

-- | Parse a unit name.
unitName :: Parser (Either Atom Integer)
unitName = eitherP atom (signed integer) <?> "unit name"
{-# INLINE unitName #-}

-- | Parse a list of unit names.
unitNames :: Parser (NonEmpty UnitName)
unitNames = bracketList1 unitName <?> "unit names"
{-# INLINE unitNames #-}

-- | Parse an @include@ statement.
include :: Parser Unit
include =  labeled "include" (Include <$> atom <*> maybeP unitNames) <* op '.'
       <?> "include"

-- | Parse an annotated unit.
annotatedUnit :: Parser Unit
annotatedUnit = do
  l <- language
  let n = unitName
  let d = declaration l
  let a = maybeP annotation
  parens (Unit <$> n <*> comma d <*> a) <* op '.'
  <?> "annotated unit"

-- | Parse a TPTP unit.
unit :: Parser Unit
unit = include <|> annotatedUnit <?> "unit"

-- | Parse a TPTP input.
tptp :: Parser TPTP
tptp = TPTP <$> manyTill unit endOfInput <?> "derivation"


-- ** Annotations

-- | Parse an introduction marking.
intro :: Parser Intro
intro = enum <?> "intro"
{-# INLINE intro #-}

-- | Parse a unit of information about a formula.
info :: Parser Info
info =  labeled "description" (Description <$> atom)
    <|> labeled "iquote"      (Iquote      <$> atom)
    <|> labeled "status"      (Status      <$> reserved)
    <|> labeled "assumptions" (Assumptions <$> unitNames)
    <|> labeled "refutation"  (Refutation  <$> atom)
    <|> labeled "new_symbols" (NewSymbols  <$> atom <*> comma symbols)
    <|> labeled "bind"        (Bind <$> var <*> comma expr)
    <|> Expression <$> expr
    <|> uncurry Application <$> application atom info
    <|> InfoNumber <$> number
    <|> Infos <$> infos
  where
    symbols = bracketList (eitherP var atom)

infos :: Parser [Info]
infos = bracketList info <?> "infos"
{-# INLINE infos #-}

-- | Parse and expression
expr :: Parser Expression
expr =  char '$' *> (labeled "fot" (Term <$> term)
                <|>  Logical <$> (language >>= parens . formula))
    <?> "expression"

-- | Parse a parent.
parent :: Parser Parent
parent = Parent <$> source <*> option [] (op ':' *> infos) <?> "parent"

-- | Parse the source of a unit.
source :: Parser Source
source =  token "unknown" $> UnknownSource
      <|> labeled "file"       (File       <$> atom     <*> maybeP unitName)
      <|> labeled "theory"     (Theory     <$> atom     <*> maybeP infos)
      <|> labeled "creator"    (Creator    <$> atom     <*> maybeP infos)
      <|> labeled "introduced" (Introduced <$> reserved <*> maybeP infos)
      <|> labeled "inference"  (Inference  <$> atom     <*> comma  infos
                                           <*> comma (bracketList parent))
      <|> UnitSource <$> unitName
      <?> "source"

-- | Parse an annotation.
annotation :: Parser Annotation
annotation = (,) <$> source <*> maybeP infos <?> "annotation"
