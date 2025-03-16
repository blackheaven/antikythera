{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module        : Control.Antikythera
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- Run an action given a 'Periodicity'
--
-- > import Control.Antikythera
-- >
-- > runPeriodicityZonedTime (inclusiveRange (Min 8) (Max 23) hour .&& every 30 minute) $
-- >   putStrLn "Don't forget to hydrate"
module Control.Antikythera.Scheduling
  ( -- * 'Periodicity' runners
    runPeriodicityUTCTime,
    runPeriodicityZonedTime,
    runPeriodicityZonedTime',
    runPeriodicity,
    runPeriodicityWithHooks,

    -- * Position in time
    PositionInTime (..),
    utcTime,
    zonedTime,
    zonedTime',
  )
where

import Control.Antikythera.Periodicity
import Control.Antikythera.Unit.Time
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forM_)
import Data.Time

-- * 'Periodicity' runners

-- | Run an action given a 'Periodicity' using system's 'UTCTime'
--
-- Note: the action is run in the loop, consider using a dedicated thread as any exception would break it
runPeriodicityUTCTime :: Periodicity UTCTime -> IO () -> IO ()
runPeriodicityUTCTime = runPeriodicity utcTime

-- | Run an action given a 'Periodicity' using system's 'ZonedTime' (wrapped to accomodate combination operators)
--
-- Note: the action is run in the loop, consider using a dedicated thread as any exception would break it
runPeriodicityZonedTime :: Periodicity ZonedTimeWrapped -> IO () -> IO ()
runPeriodicityZonedTime = runPeriodicity zonedTime

-- | Run an action given a 'Periodicity' using system's 'ZonedTime'
--
-- Note: the action is run in the loop, consider using a dedicated thread as any exception would break it
runPeriodicityZonedTime' :: Periodicity ZonedTime -> IO () -> IO ()
runPeriodicityZonedTime' = runPeriodicity zonedTime'

-- | Run an action given a 'Periodicity'
--
-- Note: the action is run in the loop, consider using a dedicated thread as any exception would break it
runPeriodicity ::
  -- | Fetch the time and compute the delay time
  PositionInTime t ->
  Periodicity t ->
  IO () ->
  IO ()
runPeriodicity =
  runPeriodicityWithHooks
    (const $ return ())
    (return ())
    (const $ return ())

-- | Run an action given a 'Periodicity' with hooks
--
-- Note: the action is run in the loop, consider using a dedicated thread as any exception would break it
runPeriodicityWithHooks ::
  -- | Hooks planned
  (t -> IO ()) ->
  -- | Hooks at time (before running the action)
  IO () ->
  -- | Hooks done
  (a -> IO ()) ->
  -- | Fetch the time and compute the delay time
  PositionInTime t ->
  Periodicity t ->
  IO a ->
  IO ()
runPeriodicityWithHooks hookPlanned hookBefore hookAfter pit p f = do
  now <- pit.getTime
  forM_ (p.nextPeriod now) $ \next -> do
    hookPlanned next
    delay $ pit.delayMicroSeconds now next
    hookBefore
    f >>= hookAfter
    runPeriodicityWithHooks hookPlanned hookBefore hookAfter pit p f

-- * Position in time

-- | Fetch the time and compute the delay time
data PositionInTime t = PositionInTime
  { getTime :: IO t,
    -- | now -> nextPeriod -> µs
    delayMicroSeconds :: t -> t -> Integer
  }

-- | System's 'UTCTime'
utcTime :: PositionInTime UTCTime
utcTime =
  PositionInTime
    { getTime = getCurrentTime,
      delayMicroSeconds = \now next ->
        ceiling $ 1_000_000 * nominalDiffTimeToSeconds (diffUTCTime next now)
    }

-- | System's 'ZonedTime' (wrapped to accomodate combination operators)
zonedTime :: PositionInTime ZonedTimeWrapped
zonedTime =
  PositionInTime
    { getTime = ZonedTimeWrapped <$> getZonedTime,
      delayMicroSeconds = \(ZonedTimeWrapped now) (ZonedTimeWrapped next) ->
        ceiling $ 1_000_000 * nominalDiffTimeToSeconds (diffUTCTime (zonedTimeToUTC next) (zonedTimeToUTC now))
    }

-- | System's 'ZonedTime'
zonedTime' :: PositionInTime ZonedTime
zonedTime' =
  PositionInTime
    { getTime = getZonedTime,
      delayMicroSeconds = \now next ->
        ceiling $ 1_000_000 * nominalDiffTimeToSeconds (diffUTCTime (zonedTimeToUTC next) (zonedTimeToUTC now))
    }
