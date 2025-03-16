-- |
-- Module        : Control.Antikythera.Periodicity
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- Defining a 'Periodicity', how often/when an event occurs
module Control.Antikythera.Periodicity
  ( Periodicity (..),

    -- * Base helpers
    never,
    always,

    -- * Combinators
    (.&&),
    (.||),
    allOf,
    anyOf,
    allOf',
    anyOf',

    -- * 'Unit'-based builders
    at,
    ats,
    every,
    inclusiveRange,

    -- * Reexports
    Max (..),
    Min (..),
  )
where

import Control.Antikythera.Unit.Unit
import qualified Data.List.NonEmpty as NE
import Data.Semigroup

-- | Event recurring period
--
-- Are we at 17:*?
--
-- > (at 17 hour).includes now
--
-- Next *:05
--
-- > (at 5 minute).nextPeriod now
data Periodicity a = Periodicity
  { includes :: a -> Bool,
    nextPeriod :: a -> Maybe a
  }

-- * Base helpers

-- | Never happen
never :: Periodicity a
never =
  Periodicity
    { includes = const False,
      nextPeriod = const Nothing
    }

-- | Always happen
always :: (a -> a) -> Periodicity a
always f =
  Periodicity
    { includes = const True,
      nextPeriod = Just . f
    }

-- * Combinators

-- | Intersection of two periods
--
-- Everyday at 15:15
--
-- > at 15 hour .&& at 15 minute
(.&&) :: (Ord a) => Periodicity a -> Periodicity a -> Periodicity a
x .&& y =
  Periodicity
    { includes = \c -> x.includes c && y.includes c,
      nextPeriod =
        let go c =
              case (x.nextPeriod c, y.nextPeriod c) of
                (Just n, Just m) ->
                  let c' = min m n
                   in if x.includes c' && y.includes c'
                        then Just c'
                        else go c'
                _ -> Nothing
         in go
    }

infixr 3 .&&

-- | Union of two periods
--
-- Everyday at 15:* or every hour at *:15
--
-- > at 15 hour .|| at 15 minute
(.||) :: (Ord a) => Periodicity a -> Periodicity a -> Periodicity a
x .|| y =
  Periodicity
    { includes = \c -> x.includes c || y.includes c,
      nextPeriod = \c ->
        case (x.nextPeriod c, y.nextPeriod c) of
          (Just n, Just m) -> Just $ min n m
          (Just n, _) -> Just n
          (_, o) -> o
    }

infixr 2 .||

-- | Intersections of all periods
--
-- Same as
--
-- > allOf = foldl1 (.&&)
allOf :: (Ord a) => NE.NonEmpty (Periodicity a) -> Periodicity a
allOf = foldl1 (.&&)

-- | Unions of all periods
--
-- Same as
--
-- > anyOf = foldl1 (.||)
anyOf :: (Ord a) => NE.NonEmpty (Periodicity a) -> Periodicity a
anyOf = foldl1 (.||)

-- | Intersections of all periods
--
-- Same as
--
-- > allOf' = foldl (.&&) . always
allOf' :: (Foldable f, Ord a) => (a -> a) -> f (Periodicity a) -> Periodicity a
allOf' = foldl (.&&) . always

-- | Unions of all periods
--
-- Same as
--
-- > anyOf' = foldl (.||) never
anyOf' :: (Foldable f, Ord a) => f (Periodicity a) -> Periodicity a
anyOf' = foldl (.||) never

-- * 'Unit'-based builders

-- | Happens when the 'Unit' has a value
--
-- Every hour at *:05
--
-- > at 5 minute
at :: (Eq i) => i -> Unit i a -> Periodicity a
at n u =
  Periodicity
    { includes = (== n) . u.extract,
      nextPeriod = u.nextUnitWith n
    }

-- | Happens when the 'Unit' has one of the values
--
-- Every hour at *:05 and *:35
--
-- > ats [5, 35] minute
--
-- Equivalent to
--
-- > at 5 minute .|| at 35 minute
ats :: (Ord i) => NE.NonEmpty i -> Unit i a -> Periodicity a
ats ns u =
  Periodicity
    { includes = (`elem` ns') . u.extract,
      nextPeriod = \x -> u.nextUnitWith (nextCandidate $ u.extract x) x
    }
  where
    ns' = NE.sort ns
    nextCandidate x =
      case NE.dropWhile (<= x) ns' of
        (c : _) -> c
        _ -> NE.head ns'

-- | Happens when the 'Unit' has a value with a modulo
--
-- Every hour at *:00, *:15, *:30, and *:45
--
-- > every 15 minute
every :: (Integral i) => i -> Unit i a -> Periodicity a
every n u =
  Periodicity
    { includes = ((== 0) . flip mod n) . u.extract,
      nextPeriod = \x -> u.nextUnitWith (nextCandidate $ u.extract x) x
    }
  where
    nextCandidate x = n * succ (x `div` n)

-- | Happens when the 'Unit' has a value in an inclusive range
--
-- Every hour at *:05, *:06, *:07, *:08, *:09, *:10
--
-- > inclusiveRange (Min 5) (Max 10) minute
inclusiveRange :: (Enum i, Ord i) => Min i -> Max i -> Unit i a -> Periodicity a
inclusiveRange (Min lowerBound) (Max upperBound) u =
  Periodicity
    { includes = (\n -> n >= lowerBound && n <= upperBound) . u.extract,
      nextPeriod = \x -> u.nextUnitWith (nextCandidate $ u.extract x) x
    }
  where
    nextCandidate x =
      if x < upperBound
        then succ x
        else lowerBound
