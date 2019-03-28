{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Data.TSTP.Parse.Combinators where

import Control.Applicative ((<|>), many, optional)

import Data.Attoparsec.Text as A
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

comment :: Parser ()
comment = char '%' *> skipWhile (not . isEndOfLine) *> endOfLine

whitespace :: Parser ()
whitespace = skipSpace *> skipMany (comment *> skipSpace)

-- | 'lexem' makes a given parser consume trailing whitespace. This function is
-- needed because off-the-shelf attoparsec parsers do not do it.
lexem :: Parser a -> Parser a
lexem p = p <* whitespace

numeral :: Parser Integer
numeral = lexem (signed decimal)

token :: Text -> Parser Text
token = lexem . string

op :: Char -> Parser Char
op = lexem . char

parens :: Parser a -> Parser a
parens p = op '(' *> p <* op ')'

brackets :: Parser a -> Parser a
brackets p = op '[' *> p <* op ']'

enum :: (Named a, Enum a, Bounded a) => Parser a
enum = lexem
     $ choice
     $ fmap (\(n, c) -> string n $> c)
     $ L.sortBy (\(a, _) (b, _) -> b `compare` a)
     $ fmap (\c -> (I.name c, c)) [minBound..]

name :: (Named a, Enum a, Bounded a) => Parser (Name a)
name =  Standard <$> enum
    <|> Defined  <$> atom

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlphaNum c || c == '_'

-- * Parser combinators

-- ** Names

atom :: Parser Atom
atom = Atom <$> lexem (singleQuoted <|> lowerWord)
  where
    singleQuoted = T.pack <$> (char '\'' *> many sq <* char '\'')
    sq = A.satisfy isSg <|> string "\\'" $> '\''
    isSg c = (c >= ' ' && c <= '&') || (c >= '(' && c <= '~')
    lowerWord = T.cons <$> A.satisfy isAsciiLower <*> A.takeWhile isAlphaNumeric

var :: Parser Var
var = Var <$> lexem upperWord
  where
    upperWord = T.cons <$> A.satisfy isAsciiUpper <*> A.takeWhile isAlphaNumeric

function :: Parser (Name Function)
function = name

predicate :: Parser (Name Predicate)
predicate = name

-- ** Sorts and types

sort :: Parser (Name Sort)
sort = name

type_ :: Parser Type
type_ = Mapping <$> option [] (sorts <* op '>') <*> sort
  where
    sorts =  fmap (:[]) sort
         <|> parens (sort `sepBy1` op '*')

term :: Parser Term
term =  parens term
    <|> Function <$> function <*> option [] (parens (term `sepBy1` op ','))
    <|> Variable <$> var
    <|> Constant <$> numeral

sign :: Parser Sign
sign = enum

literal :: Parser Literal
literal =  parens literal
       <|> Equality  <$> term <*> sign <*> term
       <|> Predicate <$> predicate <*> option [] (parens (term `sepBy1` op ','))
       <|> token "$true"  $> Tautology
       <|> token "$false" $> Falsum

signedLiteral :: Parser (Sign, Literal)
signedLiteral = (,) <$> option Positive (op '~' $> Negative) <*> literal

clause :: Parser Clause
clause =  parens clause
      <|> Clause . NEL.fromList <$> signedLiteral `sepBy1` op '|'

quantifier :: Parser Quantifier
quantifier = enum

connective :: Parser Connective
connective = enum

firstOrder :: Parser s -> Parser (FirstOrder s)
firstOrder p = do
  f <- unitary
  option f (Connected f <$> connective <*> firstOrder p)
  where
    unitary =  parens (firstOrder p)
           <|> Atomic <$> literal
           <|> Quantified <$> quantifier <*> vs <* op ':' <*> unitary
           <|> Negated <$> (op '~' *> unitary)

    vs = brackets (NEL.fromList <$> v `sepBy1` op ',')
    v = (,) <$> var <*> p

unsorted :: Parser Unsorted
unsorted = pure (Unsorted ())

sorted :: Parser Sorted
sorted = Sorted <$> optional (op ':' *> sort)

-- ** Formula annotations

role :: Parser (Name Role)
role = name

lang :: Parser Language
lang = enum

formula :: Language -> Parser Formula
formula = \case
  CNF_ -> CNF <$> clause
  FOF_ -> FOF <$> firstOrder unsorted
  TFF_ -> TFF <$> firstOrder sorted

intro :: Parser Intro
intro = enum

info :: Parser Info
info = Info <$> generalList

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = optional (op ',' *> p)

generalData :: Parser GeneralData
generalData =  string "bind" *> parens (GeneralBind <$> var <* op ',' <*> form)
           <|> GeneralFunction <$> atom <*> generalTerms
           <|> GeneralVariable <$> var
           <|> GeneralNumber   <$> numeral
           <|> GeneralFormula  <$> form
  where
    generalTerms = option [] (parens (generalTerm `sepBy1` op ','))
    form = char '$' *> do { l <- lang; parens (formula l) }

generalTerm :: Parser GeneralTerm
generalTerm =  GeneralData <$> generalData <*> optional (op ':' *> generalTerm)
           <|> GeneralList <$> generalList

generalList :: Parser [GeneralTerm]
generalList = brackets (generalTerm `sepBy` op ',')

parent :: Parser Parent
parent = Parent <$> source <*> option [] (op ':' *> generalList)

source :: Parser Source
source =  string "file"       *> parens (File       <$> atom  <*> maybeP atom)
      <|> string "theory"     *> parens (Theory     <$> atom  <*> maybeP info)
      <|> string "creator"    *> parens (Creator    <$> atom  <*> maybeP info)
      <|> string "introduced" *> parens (Introduced <$> intro <*> maybeP info)
      <|> string "inference"  *> parens (Inference  <$> atom <* op ','
                                                    <*> info <* op ',' <*> ps)
      <|> string "unknown"    $> UnknownSource
      <|> Sources <$> brackets (NEL.fromList <$> source `sepBy` op ',')
      <|> Dag <$> atom
  where
    ps = brackets (parent `sepBy` op ',')

-- ** Derivations

unit :: Parser Unit
unit = do
  l <- lang
  let n = eitherP atom numeral
  let ann = maybeP $ (,) <$> source <*> maybeP info
  let u = Unit <$> n <* op ',' <*> role <* op ',' <*> formula l <*> ann
  parens u <* op '.'

derivation :: Parser Derivation
derivation = Derivation <$> many unit
