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
    nextPeriods,

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

    -- * Absolute builders
    sinceInclusive,
    untilInclusive,

    -- * Reexports
    Max (..),
    Min (..),
  )
where

import Control.Antikythera.Unit.Unit
import Control.Arrow ((&&&))
import Control.Monad (mfilter)
import Data.List (unfoldr)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup

-- | Event recurring period
--
-- Are we at @17:*@?
--
-- > (at 17 hour).includes now
--
-- Next @*:05@
--
-- > (at 5 minute).nextPeriod now
data Periodicity a = Periodicity
  { includes :: a -> Bool,
    nextPeriod :: a -> Maybe a
  }

-- | Get a poentially infinite list of upcoming event
--
-- __Warning:__ may loop infinitelly
nextPeriods :: Periodicity a -> a -> [a]
nextPeriods p = unfoldr (fmap (id &&& id) . p.nextPeriod)

-- * Base helpers

-- | Never happen
never :: Periodicity a
never =
  Periodicity
    { includes = const False,
      nextPeriod = const Nothing
    }

-- | Always happen
--
-- Going from minute to minute:
--
-- > always (addUTCTime $ secondsToNominalDiffTime 60)
always ::
  -- | Increment to next value
  (a -> a) ->
  Periodicity a
always f =
  Periodicity
    { includes = const True,
      nextPeriod = Just . f
    }

-- * Combinators

-- | Intersection of two periods
--
-- Everyday at @15:15@
--
-- > at 15 hour .&& at 15 minute
--
-- __Warning:__ may loop infinitelly when impossible constraints, e.g.
--
-- > at 15 minutes .&& at 15 minute
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
-- Everyday at @15:*@ or every hour at @*:15@
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
--
-- __Warning:__ may loop infinitelly when impossible constraints, see '(.&&)'
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
--
-- __Warning:__ may loop infinitelly when impossible constraints, see '(.&&)'
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
-- Every hour at @*:05@
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
-- Every hour at @*:05@ and @*:35@
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
-- Every hour at @*:00@, @*:15@, @*:30@, and @*:45@
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
-- Every hour at @*:05@, @*:06@, @*:07@, @*:08@, @*:09@, @*:10@
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

-- * Absolute builders

-- | An event only hapenning /since/
sinceInclusive ::
  (Ord a) =>
  -- | Increment to next value
  (a -> a) ->
  a ->
  Periodicity a
sinceInclusive f startingAt =
  Periodicity
    { includes = (>= startingAt),
      nextPeriod =
        \x ->
          Just $
            if x < startingAt
              then startingAt
              else f x
    }

-- | An event only hapenning /until/
untilInclusive ::
  (Ord a) =>
  -- | Increment to next value
  (a -> a) ->
  a ->
  Periodicity a
untilInclusive f endingAt =
  Periodicity
    { includes = (<= endingAt),
      nextPeriod = mfilter (<= endingAt) . Just . f
    }
