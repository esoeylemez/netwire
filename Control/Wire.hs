-- |
-- Module:     Control.Wire
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire
    ( -- * Reexports
      module Control.Wire.Core,
      module Control.Wire.Event,
      module Control.Wire.Interval,
      module Control.Wire.Run,
      module Control.Wire.Session,
      module Control.Wire.Switch,
      module Control.Wire.Time,

      -- * Convenient type aliases
      WireP,
      SimpleWire,

      -- * External
      module Control.Applicative,
      module Control.Arrow,
      module Control.Category,
      module Data.Semigroup,
      Identity(..),
      NominalDiffTime
    )
    where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Wire.Core
import Control.Wire.Event
import Control.Wire.Interval
import Control.Wire.Run
import Control.Wire.Session
import Control.Wire.Switch
import Control.Wire.Time
import Data.Functor.Identity
import Data.Semigroup
import Data.Time.Clock


-- | Pure wires.

type WireP s e = Wire s e Identity


-- | Simple wires with time.

type SimpleWire = Wire (Timed NominalDiffTime ()) () Identity
