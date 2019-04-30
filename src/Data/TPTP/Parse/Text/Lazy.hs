-- |
-- Module       : Data.TPTP.Parse.Text.Lazy
-- Description  : An attoparsec-based parser for the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TPTP.Parse.Text.Lazy (
  -- * Runners of parsers for TPTP units.
  parseUnit,

  -- * Runners of parsers for TPTP inputs.
  parseTPTP
) where

import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy (Text)

import Data.TPTP (Unit, TPTP)
import Data.TPTP.Parse.Combinators (whitespace, unit, tptp)

-- | Parse a single TPTP unit from 'Lazy.Text'.
parseUnit :: Text -> Result Unit
parseUnit = parse (whitespace *> unit <* endOfInput)

-- | Parse a TPTP input from 'Lazy.Text'.
parseTPTP :: Text -> Result TPTP
parseTPTP = parse (whitespace *> tptp <* endOfInput)
