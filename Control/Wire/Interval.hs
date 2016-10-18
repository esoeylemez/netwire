-- |
-- Module:     Control.Wire.Interval
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Interval
    ( -- * Basic intervals
      inhibit,

      -- * Time intervals
      after,
      for,

      -- * Signal analysis
      unless,
      when,

      -- * Event-based intervals
      asSoonAs,
      between,
      hold,
      holdFor,
      until
    )
    where

import Control.Arrow
import Control.Wire.Core
import Control.Wire.Event
import Control.Wire.Session
import Control.Wire.Unsafe.Event
import Data.Monoid
import Prelude hiding (until)


-- | After the given time period.
--
-- * Depends: now after the given time period.
--
-- * Inhibits: for the given time period.

after :: (HasTime t s, Monoid e) => t -> Wire s e m a a
after t' =
    mkPure $ \ds x ->
        let t = t' - dtime ds in
        if t <= 0
          then (Right x, mkId)
          else (Left mempty, after t)


-- | Alias for 'hold'.

asSoonAs :: (Monoid e) => Wire s e m (Event a) a
asSoonAs = hold


-- | Start each time the left event occurs, stop each time the right
-- event occurs.
--
-- * Depends: now when active.
--
-- * Inhibits: after the right event occurred, before the left event
-- occurs.

between :: (Monoid e) => Wire s e m (a, Event b, Event c) a
between =
    mkPureN $ \(x, onEv, _) ->
        event (Left mempty, between)
              (const (Right x, active))
              onEv

    where
    active =
        mkPureN $ \(x, _, offEv) ->
            event (Right x, active)
                  (const (Left mempty, between))
                  offEv


-- | For the given time period.
--
-- * Depends: now for the given time period.
--
-- * Inhibits: after the given time period.

for :: (HasTime t s, Monoid e) => t -> Wire s e m a a
for t' =
    mkPure $ \ds x ->
        let t = t' - dtime ds in
        if t <= 0
          then (Left mempty, mkEmpty)
          else (Right x, for t)


-- | Start when the event occurs for the first time reflecting its
-- latest value.
--
-- * Depends: now.
--
-- * Inhibits: until the event occurs for the first time.

hold :: (Monoid e) => Wire s e m (Event a) a
hold =
    mkPureN $
        event (Left mempty, hold)
              (Right &&& holdWith)

    where
    holdWith x =
        mkPureN $
            event (Right x, holdWith x)
                  (Right &&& holdWith)


-- | Hold each event occurrence for the given time period.  Inhibits
-- when no event occurred for the given amount of time.  New occurrences
-- override old occurrences, even when they are still held.
--
-- * Depends: now.
--
-- * Inhibits: when no event occurred for the given amount of time.

holdFor :: (HasTime t s, Monoid e) => t -> Wire s e m (Event a) a
holdFor int | int <= 0 = error "holdFor: Non-positive interval."
holdFor int = off
    where
    off =
        mkPure $ \_ ->
            event (Left mempty, off)
                  (Right &&& on int)

    on t' x' =
        mkPure $ \ds ->
            let t = t' - dtime ds in
            event (if t <= 0
                     then (Left mempty, off)
                     else (Right x', on t x'))
                  (Right &&& on int)


-- | Inhibit forever with the given value.
--
-- * Inhibits: always.

inhibit :: e -> Wire s e m a b
inhibit = mkConst . Left


-- | When the given predicate is false for the input signal.
--
-- * Depends: now.
--
-- * Inhibits: unless the predicate is false.

unless :: (Monoid e) => (a -> Bool) -> Wire s e m a a
unless p =
    mkPure_ $ \x ->
        if p x then Left mempty else Right x


-- | Produce until the given event occurs.  When it occurs, inhibit with
-- its value forever.
--
-- * Depends: now until event occurs.
--
-- * Inhibits: forever after event occurs.

until :: (Monoid e) => Wire s e m (a, Event b) a
until =
    mkPureN . uncurry $ \x ->
        event (Right x, until) (const (Left mempty, mkEmpty))


-- | When the given predicate is true for the input signal.
--
-- * Depends: now.
--
-- * Inhibits: when the predicate is false.

when :: (Monoid e) => (a -> Bool) -> Wire s e m a a
when p =
    mkPure_ $ \x ->
        if p x then Right x else Left mempty
