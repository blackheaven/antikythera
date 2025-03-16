-- |
-- Module        : Control.Antikythera.Unit.Unit
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- Standalone definition 'Unit', for 'Periodicy' definition
module Control.Antikythera.Unit.Unit (Unit (..)) where

-- | Type use to define 'Periodicy' patterns (e.g. 'every', 'at') for time types
--
-- For example:
--
-- > instance HasMinute TimeOfDay where
-- >   minute =
-- >     Unit
-- >       { extract = todMin
-- >       , nextUnitWith = \n x ->
-- >           let m = n `mod` 60
-- >           in Just $ TimeOfDay ((x.todHour + (if m <= x.todMin then 1 else 0)) `mod` 24) m 0
-- >       }
data Unit i a = Unit
  { extract :: a -> i,
    -- | should apply modulo
    nextUnitWith :: i -> a -> Maybe a
  }
