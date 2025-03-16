-- |
-- Module        : Control.Antikythera
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- Standalone module including everything needed to build a scheduler
--
-- > import Control.Antikythera
-- >
-- > runPeriodicityZonedTime (inclusiveRange (Min 8) (Max 23) hour .&& every 30 minute) $
-- >   putStrLn "Don't forget to hydrate"
module Control.Antikythera (module X) where

import Control.Antikythera.Periodicity as X
import Control.Antikythera.Scheduling as X
import Control.Antikythera.Unit as X
