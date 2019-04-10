-- |
-- Module       : Data.TPTP.Parse.Text
-- Description  : An attoparsec-based parser for the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TPTP.Parse.Text (
  -- * Runners for the parser for TPTP units.
  parseUnit,
  parseUnitOnly,
  parseUnitWith,

  -- * Runners for parsers for TPTP derivations.
  parseDerivation,
  parseDerivationOnly,
  parseDerivationWith
) where

import Data.Attoparsec.Text
import Data.Text (Text)

import Data.TPTP (Unit, Derivation)
import Data.TPTP.Parse.Combinators (whitespace, unit, derivation)

-- | Run a parser for a single TPTP unit on 'Text'.
parseUnit :: Text -> Result Unit
parseUnit = parse (whitespace *> unit <* endOfInput)

-- | Run a parser for a single TPTP unit that cannot be resupplied
-- via a 'Partial' result.
parseUnitOnly :: Text -> Either String Unit
parseUnitOnly = parseOnly (whitespace *> unit <* endOfInput)

-- | Run a parser for a single TPTP unit with an initial input string,
-- and a monadic action that can supply more input if needed.
parseUnitWith :: Monad m => m Text -> Text -> m (Result Unit)
parseUnitWith m = parseWith m (whitespace *> unit <* endOfInput)

-- | Run a parser for a TPTP derivation on 'Text'.
parseDerivation :: Text -> Result Derivation
parseDerivation = parse (whitespace *> derivation <* endOfInput)

-- | Run a parser for a TPTP derivation that cannot be resupplied
-- via a 'Partial' result.
parseDerivationOnly :: Text -> Either String Derivation
parseDerivationOnly = parseOnly (whitespace *> derivation <* endOfInput)

-- | Run a parser for a TPTP derivation with an initial input string,
-- and a monadic action that can supply more input if needed.
parseDerivationWith :: Monad m => m Text -> Text -> m (Result Derivation)
parseDerivationWith m = parseWith m (whitespace *> derivation <* endOfInput)
