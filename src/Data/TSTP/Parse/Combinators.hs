{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module       : Data.TSTP.Parse.Combinators
-- Description  : Parser combinators for the TSTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TSTP.Parse.Combinators where

import Control.Applicative ((<|>), many, optional)

import Data.Attoparsec.Text as A hiding (Number, number)
import Data.Char (isAsciiLower, isAsciiUpper, isAlphaNum)
import Data.Functor (($>))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL

import Data.Text (Text)
import qualified Data.Text as T

import Data.TSTP
import Data.TSTP.Internal hiding (name)
import qualified Data.TSTP.Internal as I

-- * Helper functions

-- | Consume a single line comment - characters between @%@ and newline.
comment :: Parser ()
comment = char '%' *> skipWhile (not . isEndOfLine)
                   *> (endOfLine <|> endOfInput)
                  <?> "comment"

-- | Consume white space and trailing comments.
whitespace :: Parser ()
whitespace = skipSpace *> skipMany (comment *> skipSpace) <?> "whitespace"

-- | 'lexem' makes a given parser consume trailing whitespace. This function is
-- needed because off-the-shelf attoparsec parsers do not do it.
lexem :: Parser a -> Parser a
lexem p = p <* whitespace

-- | Parse an unsigned integer.
integer :: Parser Integer
integer = lexem decimal <?> "integer"

token :: Text -> Parser Text
token t = lexem (string t) <?> "token " ++ T.unpack t

op :: Char -> Parser Char
op c = lexem (char c) <?> "operator " ++ [c]

parens :: Parser a -> Parser a
parens p = op '(' *> p <* op ')' <?> "parens"

brackets :: Parser a -> Parser a
brackets p = op '[' *> p <* op ']' <?> "brackets"

enum :: (Named a, Enum a, Bounded a) => Parser a
enum = lexem
     $ choice
     $ fmap (\(n, c) -> string n $> c <?> "reserved " ++ T.unpack n)
     $ L.sortBy (\(a, _) (b, _) -> b `compare` a)
     $ fmap (\c -> (I.name c, c)) [minBound..]

name :: (Named a, Enum a, Bounded a) => Parser (Name a)
name =  Reserved <$> (char '$' *> enum)
    <|> Defined  <$> atom

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlphaNum c || c == '_'

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = optional (op ',' *> p)

-- * Parser combinators

-- ** Names

-- | Parse an atomic word. Single-quoted atoms are parsed without the single
-- quotes and with the characters @'@ and @\\@ unescaped.
atom :: Parser Atom
atom = Atom <$> lexem (singleQuoted <|> lowerWord) <?> "atom"
  where
    singleQuoted = T.pack <$> (char '\'' *> many sq <* char '\'')
    sq = string "\\'" $> '\'' <|> string "\\\\" $> '\\' <|> A.satisfy isSg
    isSg c = c >= ' ' && c <= '~' && c /= '\''
    lowerWord = T.cons <$> A.satisfy isAsciiLower <*> A.takeWhile isAlphaNumeric

-- | Parse a variable.
var :: Parser Var
var = Var <$> lexem upperWord <?> "var"
  where
    upperWord = T.cons <$> A.satisfy isAsciiUpper <*> A.takeWhile isAlphaNumeric

-- | Parse a distinct object. Double quotes are not preserved and the characters
-- @'@ and @\\@ are unescaped.
distinctObject :: Parser DistinctObject
distinctObject = DistinctObject <$> lexem doubleQuoted <?> "distinct object"
  where
    doubleQuoted = T.pack <$> (char '"' *> many dq <* char '"')
    dq = string "\\\"" $> '"' <|> string "\\\\" $> '\\' <|> A.satisfy isDg
    isDg c = c >= ' ' && c <= '~' && c /= '"'

-- | Parser a function name.
function :: Parser (Name Function)
function = name <?> "function"

-- | Parse a predicate name.
predicate :: Parser (Name Predicate)
predicate = name <?> "predicate"

-- ** Sorts and types

-- | Parse a sort.
sort :: Parser (Name Sort)
sort = name <?> "sort"

-- | Parse a type.
type_ :: Parser Type
type_ =  Mapping <$> option [] (sorts <* op '>') <*> sort <?> "type"
  where
    sorts =  fmap (:[]) sort
         <|> parens (sort `sepBy1` op '*')

-- | Parse a number.
number :: Parser Number
number =  RationalConstant <$> signed integer <* char '/' <*> integer
      <|> IntegerConstant <$> signed integer
      <?> "number"

-- | Parse a term.
term :: Parser Term
term =  parens term
    <|> Function <$> function <*> option [] (parens (term `sepBy1` op ','))
    <|> Variable <$> var
    <|> Number   <$> number
    <|> DistinctTerm <$> distinctObject
    <?> "term"

-- | Parse the equality and unequality sign.
sign :: Parser Sign
sign = enum <?> "sign"

-- Parse a literal.
literal :: Parser Literal
literal =  parens literal
       <|> Equality  <$> term <*> sign <*> term
       <|> Predicate <$> predicate <*> option [] (parens (term `sepBy1` op ','))
       <|> token "$true"  $> Tautology
       <|> token "$false" $> Falsum
       <?> "literal"

-- | Parse a signed literal.
signedLiteral :: Parser (Sign, Literal)
signedLiteral =  (,) <$> option Positive (op '~' $> Negative) <*> literal
             <?> "signed literal"

-- | Parse a clause.
clause :: Parser Clause
clause =  parens clause
      <|> Clause . NEL.fromList <$> signedLiteral `sepBy1` op '|'
      <?> "clause"

-- | Parse a quantifier.
quantifier :: Parser Quantifier
quantifier = enum <?> "quantifier"

-- | Parse a logical connective.
connective :: Parser Connective
connective = enum <?> "connective"

-- | Given a parser for sort annotations, parse a formula in first-order logic.
firstOrder :: Parser s -> Parser (FirstOrder s)
firstOrder p = do
  f <- unitary
  option f (Connected f <$> connective <*> firstOrder p)
  where
    unitary =  parens (firstOrder p)
           <|> Atomic <$> literal
           <|> Quantified <$> quantifier <*> vs <* op ':' <*> unitary
           <|> Negated <$> (op '~' *> unitary)
           <?> "unitary first order"

    vs = brackets (NEL.fromList <$> v `sepBy1` op ',')
    v = (,) <$> var <*> p

unsorted :: Parser Unsorted
unsorted = pure (Unsorted ()) <?> "unsorted"

sorted :: Parser Sorted
sorted = Sorted <$> optional (op ':' *> sort) <?> "sorted"

-- ** Formula annotations

role :: Parser (Name Role)
role = name <?> "role"

lang :: Parser Language
lang = enum <?> "language"

formula :: Language -> Parser Formula
formula = \case
  CNF_ -> CNF <$> clause
  FOF_ -> FOF <$> firstOrder unsorted
  TFF_ -> TFF <$> firstOrder sorted

intro :: Parser Intro
intro = enum <?> "intro"

info :: Parser Info
info = Info <$> generalList <?> "info"

generalData :: Parser GeneralData
generalData =  string "bind" *> parens (GeneralBind <$> var <* op ',' <*> form)
           <|> GeneralFunction <$> atom <*> generalTerms
           <|> GeneralVariable <$> var
           <|> GeneralNumber   <$> number
           <|> GeneralFormula  <$> form
           <|> GeneralDistinct <$> distinctObject
           <?> "general data"
  where
    generalTerms = option [] (parens (generalTerm `sepBy1` op ','))
    form = char '$' *> do { l <- lang; parens (formula l) }

generalTerm :: Parser GeneralTerm
generalTerm =  GeneralData <$> generalData <*> optional (op ':' *> generalTerm)
           <|> GeneralList <$> generalList
           <?> "general term"

generalList :: Parser [GeneralTerm]
generalList = brackets (generalTerm `sepBy` op ',') <?> "general list"

parent :: Parser Parent
parent = Parent <$> source <*> option [] (op ':' *> generalList) <?> "parent"

source :: Parser Source
source =  app "file"       (File       <$> atom  <*> maybeP atom)
      <|> app "theory"     (Theory     <$> atom  <*> maybeP info)
      <|> app "creator"    (Creator    <$> atom  <*> maybeP info)
      <|> app "introduced" (Introduced <$> intro <*> maybeP info)
      <|> app "inference"  (Inference  <$> atom <* op ','
                                       <*> info <* op ',' <*> ps)
      <|> string "unknown" $> UnknownSource
      <|> Sources <$> brackets (NEL.fromList <$> source `sepBy` op ',')
      <|> Dag <$> atom
      <?> "source"
  where
    app f as = string f *> parens as
    ps = brackets (parent `sepBy` op ',')

-- ** Derivations

-- | Parse a TSTP unit.
unit :: Parser Unit
unit = do
  l <- lang
  let n = eitherP atom (signed integer)
  let ann = maybeP $ (,) <$> source <*> maybeP info
  let u = Unit <$> n <* op ',' <*> role <* op ',' <*> formula l <*> ann
  parens u <* op '.' <?> "unit"

-- | Parse a TSTP derivation.
derivation :: Parser Derivation
derivation = Derivation <$> manyTill unit endOfInput <?> "derivation"
