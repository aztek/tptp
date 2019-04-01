-- |
-- Module       : Data.TSTP.Parse.Text.Lazy
-- Description  : An attoparsec-based parser for the TSTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TSTP.Parse.Text.Lazy (
  -- * Parsers for TSTP units from different sources.
  parseUnit,

  -- * Parsers for TSTP derivations from different sources.
  parseDerivation
) where

import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy (Text)

import Data.TSTP
import Data.TSTP.Parse.Combinators

-- | Parse a TSTP unit from 'Lazy.Text'.
parseUnit :: Text -> Result Unit
parseUnit = parse (whitespace *> unit)

-- | Parse a TSTP derivation from 'Lazy.Text'.
parseDerivation :: Text -> Result Derivation
parseDerivation = parse (whitespace *> derivation)
