-- |
-- Module       : Data.TPTP.Parse.Text.Lazy
-- Description  : An attoparsec-based parser for the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TPTP.Parse.Text.Lazy (
  -- * Parsers for TPTP units from different sources.
  parseUnit,

  -- * Parsers for TPTP derivations from different sources.
  parseDerivation
) where

import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy (Text)

import Data.TPTP (Unit, Derivation)
import Data.TPTP.Parse.Combinators (whitespace, unit, derivation)

-- | Parse a single TPTP unit from 'Lazy.Text'.
parseUnit :: Text -> Result Unit
parseUnit = parse (whitespace *> unit)

-- | Parse a TPTP derivation from 'Lazy.Text'.
parseDerivation :: Text -> Result Derivation
parseDerivation = parse (whitespace *> derivation)
