-- |
-- Module:     Control.Wire.Time
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Time
    ( -- * Time wires
      time,
      timeF,
      timeFrom
    )
    where

import Control.Wire.Core
import Control.Wire.Session


-- | Local time starting from zero.

time :: (HasTime t s) => Wire s e m a t
time = timeFrom 0


-- | Local time starting from zero, converted to your favorite
-- fractional type.

timeF :: (Fractional b, HasTime t s, Monad m) => Wire s e m a b
timeF = fmap realToFrac time


-- | Local time starting from the given value.

timeFrom :: (HasTime t s) => t -> Wire s e m a t
timeFrom t' =
    mkSF $ \ds _ ->
        let t = t' + dtime ds
        in lstrict (t, timeFrom t)
