-- |
-- Module:     FRP.Netwire.Noise
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module FRP.Netwire.Noise
    ( -- * Noise generators
      noise,
      noiseR,
      wackelkontakt,

      -- * Convenience
      stdNoise,
      stdNoiseR,
      stdWackelkontakt
    )
    where

import Control.Wire
import Prelude hiding ((.), id)
import System.Random


-- | Noise events with the given distance between events.  Use 'hold' or
-- 'holdFor' to generate a staircase.

noise ::
    (HasTime t s, Random b, RandomGen g)
    => t  -- ^ Time period.
    -> g  -- ^ Random number generator.
    -> Wire s e m a (Event b)
noise int | int <= 0 = error "noise: Non-positive interval"
noise int = periodicList int . randoms


-- | Noise events with the given distance between events.  Noise will be
-- in the given range.  Use 'hold' or 'holdFor' to generate a staircase.

noiseR ::
    (HasTime t s, Random b, RandomGen g)
    => t       -- ^ Step duration.
    -> (b, b)  -- ^ Noise range.
    -> g       -- ^ Random number generator.
    -> Wire s e m a (Event b)
noiseR int _ | int <= 0 = error "noiseR: Non-positive interval"
noiseR int r = periodicList int . randomRs r


-- | Convenience interface to 'noise' for 'StdGen'.

stdNoise ::
    (HasTime t s, Random b)
    => t    -- ^ Step duration.
    -> Int  -- ^ 'StdGen' seed.
    -> Wire s e m a (Event b)
stdNoise int = noise int . mkStdGen


-- | Convenience interface to 'noiseR' for 'StdGen'.

stdNoiseR ::
    (HasTime t s, Monad m, Random b)
    => t       -- ^ Step duration.
    -> (b, b)  -- ^ Noise range.
    -> Int     -- ^ 'StdGen' seed.
    -> Wire s e m a (Event b)
stdNoiseR int r = noiseR int r . mkStdGen


-- | Convenience interface to 'wackelkontakt' for 'StdGen'.

stdWackelkontakt ::
    (HasTime t s, Monad m, Monoid e)
    => t    -- ^ Step duration.
    -> Double    -- ^ Probability to produce.
    -> Int  -- ^ 'StdGen' seed.
    -> Wire s e m a a
stdWackelkontakt int p = wackelkontakt int p . mkStdGen


-- | Randomly produce or inhibit with the given probability, each time
-- for the given duration.
--
-- The name /Wackelkontakt/ (German for /slack joint/) is a Netwire
-- running gag.  It makes sure that you revisit the documentation from
-- time to time. =)
--
-- * Depends: now.

wackelkontakt ::
    (HasTime t s, Monad m, Monoid e, RandomGen g)
    => t  -- ^ Duration.
    -> Double  -- ^ Probability to produce.
    -> g  -- ^ Random number generator.
    -> Wire s e m a a
wackelkontakt int _ _ | int <= 0 = error "wackelkontakt: Non-positive duration"
wackelkontakt int p g = fmap snd $ when (< p) . hold . noise int g &&& id
