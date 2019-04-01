-- |
-- Module       : Data.TSTP.Parse.Text
-- Description  : An attoparsec-based parser for the TSTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TSTP.Parse.Text (
  -- * Runners for the parser for TSTP units.
  parseUnit,
  parseUnitOnly,
  parseUnitWith,

  -- * Runners for parsers for TSTP derivations.
  parseDerivation,
  parseDerivationOnly,
  parseDerivationWith
) where

import Data.Attoparsec.Text
import Data.Text (Text)

import Data.TSTP
import Data.TSTP.Parse.Combinators

-- | Run a parser for a TSTP unit on 'Text'.
parseUnit :: Text -> Result Unit
parseUnit = parse (whitespace *> unit <* endOfInput)

-- | Run a parser for a TSTP unit that cannot be resupplied
-- via a 'Partial' result.
parseUnitOnly :: Text -> Either String Unit
parseUnitOnly = parseOnly (whitespace *> unit <* endOfInput)

-- | Run a parser for a TSTP unit with an initial input string,
-- and a monadic action that can supply more input if needed.
parseUnitWith :: Monad m => m Text -> Text -> m (Result Unit)
parseUnitWith m = parseWith m (whitespace *> unit <* endOfInput)

-- | Run a parser for a TSTP derivation on 'Text'.
parseDerivation :: Text -> Result Derivation
parseDerivation = parse (whitespace *> derivation <* endOfInput)

-- | Run a parser for a TSTP derivation that cannot be resupplied
-- via a 'Partial' result.
parseDerivationOnly :: Text -> Either String Derivation
parseDerivationOnly = parseOnly (whitespace *> derivation <* endOfInput)

-- | Run a parser for a TSTP derivation with an initial input string,
-- and a monadic action that can supply more input if needed.
parseDerivationWith :: Monad m => m Text -> Text -> m (Result Derivation)
parseDerivationWith m = parseWith m (whitespace *> derivation <* endOfInput)
