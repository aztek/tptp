-- |
-- Module       : Data.TPTP.Parse.Text.Lazy
-- Description  : An attoparsec-based parser for the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TPTP.Parse.Text.Lazy (
  -- * Runners of parsers for TPTP units
  parseUnit,

  -- * Runners of parsers for TPTP inputs
  parseTPTP,

  -- * Runners of parsers for TSTP inputs
  parseTSTP
) where

import Data.Attoparsec.Text.Lazy (Result, parse)
import Data.Text.Lazy (Text)

import Data.TPTP (Unit, TPTP, TSTP)
import Data.TPTP.Parse.Combinators (input, unit, tptp, tstp)


-- | Parse a single TPTP unit from 'Data.Text.Lazy.Text'.
parseUnit :: Text -> Result Unit
parseUnit = parse (input unit)

-- | Parse a TPTP input from 'Data.Text.Lazy.Text'.
parseTPTP :: Text -> Result TPTP
parseTPTP = parse (input tptp)

-- | Parse a TSTP input from 'Data.Text.Lazy.Text'.
parseTSTP :: Text -> Result TSTP
parseTSTP = parse tstp
