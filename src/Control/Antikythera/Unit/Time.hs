-- |
-- Module        : Control.Antikythera.Unit.Time
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- Every time-'Unit' classes/instances
--
-- > every 5 minute
module Control.Antikythera.Unit.Time
  ( -- * Units
    HasMinute (..),
    HasHour (..),
    HasWeekDay (..),
    HasMonthDay (..),
    HasMonth (..),
    HasYear (..),

    -- * Helpers
    ZonedTimeWrapped (..),
  )
where

import Control.Antikythera.Unit.Unit
import Data.Function (on)
import Data.List (find)
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Format.ISO8601 (ISO8601)

-- * Units

-- | Every type with minutes
--
-- > every 5 minute
class HasMinute a where
  minute :: Unit Int a

instance HasMinute TimeOfDay where
  minute =
    Unit
      { extract = todMin,
        nextUnitWith = \n x ->
          let m = n `mod` 60
           in Just $ TimeOfDay ((x.todHour + (if m <= x.todMin then 1 else 0)) `mod` 24) m 0
      }

instance HasMinute LocalTime where
  minute =
    Unit
      { extract = minute.extract . localTimeOfDay,
        nextUnitWith = \n x ->
          flip fmap (minute.nextUnitWith n x.localTimeOfDay) $ \tod ->
            LocalTime ((if tod <= x.localTimeOfDay then succ else id) x.localDay) tod
      }

instance HasMinute UTCTime where
  minute =
    Unit
      { extract = minute.extract . timeToTimeOfDay . utctDayTime,
        nextUnitWith = \n x ->
          flip fmap (minute.nextUnitWith n $ timeToTimeOfDay x.utctDayTime) $ \tod ->
            UTCTime ((if timeOfDayToTime tod <= x.utctDayTime then succ else id) x.utctDay) (timeOfDayToTime tod)
      }

instance HasMinute ZonedTime where
  minute =
    Unit
      { extract = minute.extract . zonedTimeToLocalTime,
        nextUnitWith = \n x ->
          flip ZonedTime (zonedTimeZone x) <$> minute.nextUnitWith n (zonedTimeToLocalTime x)
      }

instance HasMinute UniversalTime where
  minute =
    Unit
      { extract = minute.extract . ut1ToLocalTime 0,
        nextUnitWith = \n x ->
          localTimeToUT1 0 <$> minute.nextUnitWith n (ut1ToLocalTime 0 x)
      }

-- | Every type with hours
--
-- > every 5 hour
class HasHour a where
  hour :: Unit Int a

instance HasHour TimeOfDay where
  hour =
    Unit
      { extract = todHour,
        nextUnitWith = \n _ ->
          Just $ TimeOfDay (n `mod` 24) 0 0
      }

instance HasHour LocalTime where
  hour =
    Unit
      { extract = hour.extract . localTimeOfDay,
        nextUnitWith = \n x ->
          flip fmap (hour.nextUnitWith n x.localTimeOfDay) $ \tod ->
            LocalTime ((if tod <= x.localTimeOfDay then succ else id) x.localDay) tod
      }

instance HasHour UTCTime where
  hour =
    Unit
      { extract = hour.extract . timeToTimeOfDay . utctDayTime,
        nextUnitWith = \n x ->
          flip fmap (hour.nextUnitWith n $ timeToTimeOfDay x.utctDayTime) $ \tod ->
            UTCTime ((if timeOfDayToTime tod <= x.utctDayTime then succ else id) x.utctDay) (timeOfDayToTime tod)
      }

instance HasHour ZonedTime where
  hour =
    Unit
      { extract = hour.extract . zonedTimeToLocalTime,
        nextUnitWith = \n x ->
          flip ZonedTime (zonedTimeZone x) <$> hour.nextUnitWith n (zonedTimeToLocalTime x)
      }

instance HasHour UniversalTime where
  hour =
    Unit
      { extract = hour.extract . ut1ToLocalTime 0,
        nextUnitWith = \n x ->
          localTimeToUT1 0 <$> hour.nextUnitWith n (ut1ToLocalTime 0 x)
      }

-- | Every type with week days (Mon - Sun)
--
-- > every 5 weekDay
class HasWeekDay a where
  weekDay :: Unit DayOfWeek a

instance HasWeekDay DayOfWeek where
  weekDay =
    Unit
      { extract = id,
        nextUnitWith = const . Just
      }

instance HasWeekDay Day where
  weekDay =
    Unit
      { extract = weekDay.extract . dayOfWeek,
        nextUnitWith = \n x ->
          let wd = dayOfWeek $ succ x
           in (\wd' -> addDays (fromIntegral (dayOfWeekDiff wd' wd) + 1) x) <$> weekDay.nextUnitWith n wd
      }

instance HasWeekDay LocalTime where
  weekDay =
    Unit
      { extract = weekDay.extract . localDay,
        nextUnitWith = \n x ->
          (`LocalTime` midnight) <$> weekDay.nextUnitWith n (localDay x)
      }

instance HasWeekDay UTCTime where
  weekDay =
    Unit
      { extract = weekDay.extract . utctDay,
        nextUnitWith = \n x ->
          (`UTCTime` 0) <$> weekDay.nextUnitWith n (utctDay x)
      }

instance HasWeekDay ZonedTime where
  weekDay =
    Unit
      { extract = weekDay.extract . zonedTimeToLocalTime,
        nextUnitWith = \n x ->
          flip ZonedTime (zonedTimeZone x) <$> weekDay.nextUnitWith n (zonedTimeToLocalTime x)
      }

instance HasWeekDay UniversalTime where
  weekDay =
    Unit
      { extract = weekDay.extract . ut1ToLocalTime 0,
        nextUnitWith = \n x ->
          localTimeToUT1 0 <$> weekDay.nextUnitWith n (ut1ToLocalTime 0 x)
      }

-- | Every type with month days (1-31)
--
-- > every 5 monthDay
class HasMonthDay a where
  -- | 0 == Monday
  monthDay :: Unit Int a

instance HasMonthDay Day where
  monthDay =
    Unit
      { extract = \(MonthDay _ d) -> d,
        nextUnitWith = \n x@(MonthDay m _) ->
          let td = if n > 31 then 1 + (n `mod` 32) else n
           in find (\x'@(MonthDay _ d') -> d' == td && x' > x) $ map (`MonthDay` td) [m ..]
      }

instance HasMonthDay LocalTime where
  monthDay =
    Unit
      { extract = monthDay.extract . localDay,
        nextUnitWith = \n x ->
          (`LocalTime` midnight) <$> monthDay.nextUnitWith n (localDay x)
      }

instance HasMonthDay UTCTime where
  monthDay =
    Unit
      { extract = monthDay.extract . utctDay,
        nextUnitWith = \n x ->
          (`UTCTime` 0) <$> monthDay.nextUnitWith n (utctDay x)
      }

instance HasMonthDay ZonedTime where
  monthDay =
    Unit
      { extract = monthDay.extract . zonedTimeToLocalTime,
        nextUnitWith = \n x ->
          flip ZonedTime (zonedTimeZone x) <$> monthDay.nextUnitWith n (zonedTimeToLocalTime x)
      }

instance HasMonthDay UniversalTime where
  monthDay =
    Unit
      { extract = monthDay.extract . ut1ToLocalTime 0,
        nextUnitWith = \n x ->
          localTimeToUT1 0 <$> monthDay.nextUnitWith n (ut1ToLocalTime 0 x)
      }

-- | Every type with months
--
-- > every 5 month
class HasMonth a where
  month :: Unit Int a

instance HasMonth Day where
  month =
    Unit
      { extract = \(MonthDay (YearMonth _ m) _) -> m,
        nextUnitWith = \n (MonthDay (YearMonth y m) _) ->
          let (ty, tm)
                | n > 12 && mm <= m = (y + 1, mm)
                | n > 12 = (y, mm)
                | n <= m = (y + 1, n)
                | otherwise = (y, n)
              mm = 1 + (n `mod` 13)
           in Just $ MonthDay (YearMonth ty tm) 1
      }

instance HasMonth LocalTime where
  month =
    Unit
      { extract = month.extract . localDay,
        nextUnitWith = \n x ->
          (`LocalTime` midnight) <$> month.nextUnitWith n (localDay x)
      }

instance HasMonth UTCTime where
  month =
    Unit
      { extract = month.extract . utctDay,
        nextUnitWith = \n x ->
          (`UTCTime` 0) <$> month.nextUnitWith n (utctDay x)
      }

instance HasMonth ZonedTime where
  month =
    Unit
      { extract = month.extract . zonedTimeToLocalTime,
        nextUnitWith = \n x ->
          flip ZonedTime (zonedTimeZone x) <$> month.nextUnitWith n (zonedTimeToLocalTime x)
      }

instance HasMonth UniversalTime where
  month =
    Unit
      { extract = month.extract . ut1ToLocalTime 0,
        nextUnitWith = \n x ->
          localTimeToUT1 0 <$> month.nextUnitWith n (ut1ToLocalTime 0 x)
      }

-- | Every type with years
--
-- > every 5 year
class HasYear a where
  year :: Unit Integer a

instance HasYear Day where
  year =
    Unit
      { extract = \(MonthDay (YearMonth y _) _) -> y,
        nextUnitWith = \n _ -> Just $ MonthDay (YearMonth n 1) 1
      }

instance HasYear LocalTime where
  year =
    Unit
      { extract = year.extract . localDay,
        nextUnitWith = \n x ->
          (`LocalTime` midnight) <$> year.nextUnitWith n (localDay x)
      }

instance HasYear UTCTime where
  year =
    Unit
      { extract = year.extract . utctDay,
        nextUnitWith = \n x ->
          (`UTCTime` 0) <$> year.nextUnitWith n (utctDay x)
      }

instance HasYear ZonedTime where
  year =
    Unit
      { extract = year.extract . zonedTimeToLocalTime,
        nextUnitWith = \n x ->
          flip ZonedTime (zonedTimeZone x) <$> year.nextUnitWith n (zonedTimeToLocalTime x)
      }

instance HasYear UniversalTime where
  year =
    Unit
      { extract = year.extract . ut1ToLocalTime 0,
        nextUnitWith = \n x ->
          localTimeToUT1 0 <$> year.nextUnitWith n (ut1ToLocalTime 0 x)
      }

-- * Helpers

-- | 'ZonedTime' with 'Eq' and `Ord` instances via 'UTCTime'
newtype ZonedTimeWrapped = ZonedTimeWrapped {unwrapZonedTime :: ZonedTime}
  deriving newtype (Show, Read, HasMinute, HasHour, HasWeekDay, HasMonthDay, HasMonth, HasYear, ISO8601)

instance Eq ZonedTimeWrapped where
  (==) = (==) `on` zonedTimeToUTC . unwrapZonedTime

instance Ord ZonedTimeWrapped where
  compare = compare `on` zonedTimeToUTC . unwrapZonedTime
