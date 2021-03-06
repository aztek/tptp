-- |
-- Module       : Data.TPTP.Parse.Text
-- Description  : An attoparsec-based parser for the TPTP language.
-- Copyright    : (c) Evgenii Kotelnikov, 2019-2021
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--

module Data.TPTP.Parse.Text (
  -- * Runners of parsers for TPTP units
  parseUnit,
  parseUnitOnly,
  parseUnitWith,

  -- * Runners of parsers for TPTP inputs
  parseTPTP,
  parseTPTPOnly,
  parseTPTPWith,

  -- * Runners of parsers for TSTP inputs
  parseTSTP,
  parseTSTPOnly,
  parseTSTPWith,
) where

import Data.Attoparsec.Text (Result, parse, parseOnly, parseWith)
import Data.Text (Text)

import Data.TPTP (Unit, TPTP, TSTP)
import Data.TPTP.Parse.Combinators (input, unit, tptp, tstp)


-- | Run a parser for a single TPTP unit on 'Text'.
parseUnit :: Text -> Result Unit
parseUnit = parse (input unit)

-- | Run a parser for a single TPTP unit that cannot be resupplied
-- via a 'Data.Attoparsec.Text.Partial' result.
parseUnitOnly :: Text -> Either String Unit
parseUnitOnly = parseOnly (input unit)

-- | Run a parser for a single TPTP unit with an initial input string,
-- and a monadic action that can supply more input if needed.
parseUnitWith :: Monad m => m Text -> Text -> m (Result Unit)
parseUnitWith m = parseWith m (input unit)

-- | Run a parser for a TPTP input on 'Text'.
parseTPTP :: Text -> Result TPTP
parseTPTP = parse (input tptp)

-- | Run a parser for a TPTP input that cannot be resupplied
-- via a 'Data.Attoparsec.Text.Partial' result.
parseTPTPOnly :: Text -> Either String TPTP
parseTPTPOnly = parseOnly (input tptp)

-- | Run a parser for a TPTP input with an initial input string,
-- and a monadic action that can supply more input if needed.
parseTPTPWith :: Monad m => m Text -> Text -> m (Result TPTP)
parseTPTPWith m = parseWith m (input tptp)

-- | Run a parser for a TSTP input on 'Text'.
parseTSTP :: Text -> Result TSTP
parseTSTP = parse tstp

-- | Run a parser for a TSTP input that cannot be resupplied
-- via a 'Data.Attoparsec.Text.Partial' result.
parseTSTPOnly :: Text -> Either String TSTP
parseTSTPOnly = parseOnly tstp

-- | Run a parser for a TSTP input with an initial input string,
-- and a monadic action that can supply more input if needed.
parseTSTPWith :: Monad m => m Text -> Text -> m (Result TSTP)
parseTSTPWith m = parseWith m tstp
