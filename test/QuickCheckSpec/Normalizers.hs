{-# LANGUAGE LambdaCase #-}

-- |
-- Module       : Normalizers
-- Description  : Normalization of the Data.TPTP datatypes.
-- Copyright    : (c) Evgenii Kotelnikov, 2019
-- License      : GPL-3
-- Maintainer   : evgeny.kotelnikov@gmail.com
-- Stability    : experimental
--
-- Provides functions that make applications of associative logical connectives
-- left-associative. These functions are needed for testing in the 'Main' module
--

module Normalizers (
  reassociate,
  normalizeType,
  normalizeUnit,
  normalizeTPTP,
  normalizeSource,
  normalizeInfo,
  normalizeParent
) where

import Data.TPTP


-- * First-order logic

-- | 'reassociate' makes applications of associative connectives
-- left associative
reassociate :: FirstOrder s -> FirstOrder s
reassociate = \case
  Atomic          l -> Atomic l
  Negated         f -> Negated (reassociate f)
  Quantified q vs f -> Quantified q vs (reassociate f)
  Connected f c (Connected g c' h) | c == c' && isAssociative c ->
    reassociate (Connected (Connected f c g) c h)
  Connected   f c g -> Connected (reassociate f) c (reassociate g)


-- * Units

normalizeFormula :: Formula -> Formula
normalizeFormula = \case
  CNF   c -> CNF c
  FOF  uf -> FOF (reassociate uf)
  TFF0 sf -> TFF0 (reassociate sf)
  TFF1 sf -> case monomorphizeFirstOrder sf of
    Nothing  -> TFF1 (reassociate sf)
    Just sf' -> TFF0 (reassociate sf')

normalizeType :: Type -> Type
normalizeType = \case
  TFF1Type vs ss s -> tff1Type vs ss s
  t -> t

normalizeDeclaration :: Declaration -> Declaration
normalizeDeclaration = \case
  Formula r f -> Formula r (normalizeFormula f)
  Typing  a t -> Typing  a (normalizeType t)
  d -> d

normalizeUnit :: Unit -> Unit
normalizeUnit = \case
  Include f ns -> Include f ns
  Unit   n d a -> Unit n (normalizeDeclaration d) (normalizeAnn a)
    where
      normalizeAnn = fmap $ \(s, i) -> (normalizeSource s, fmap (fmap normalizeInfo) i)

normalizeTPTP :: TPTP -> TPTP
normalizeTPTP (TPTP us) = TPTP (fmap normalizeUnit us)


-- * Annotations

normalizeSource :: Source -> Source
normalizeSource = \case
  Theory       f i -> Theory     f (fmap (fmap normalizeInfo) i)
  Creator      f i -> Creator    f (fmap (fmap normalizeInfo) i)
  Introduced i inf -> Introduced i (fmap (fmap normalizeInfo) inf)
  Inference f i ps -> Inference  f (fmap normalizeInfo i) (fmap normalizeParent ps)
  s -> s

normalizeParent :: Parent -> Parent
normalizeParent (Parent s i) = Parent (normalizeSource s) (fmap normalizeInfo i)

normalizeExpression :: Expression -> Expression
normalizeExpression = \case
  Logical f -> Logical (normalizeFormula f)
  Term    t -> Term t

normalizeInfo :: Info -> Info
normalizeInfo = \case
  Expression     e -> Expression (normalizeExpression e)
  Bind         v e -> Bind v (normalizeExpression e)
  Application f is -> Application f (fmap normalizeInfo is)
  Infos         is -> Infos (fmap normalizeInfo is)
  i -> i
