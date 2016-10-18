-- |
-- Module:     Control.Wire.Event
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Event
    ( -- * Events
      Event,

      -- * Time-based
      at,
      never,
      now,
      periodic,
      periodicList,

      -- * Signal analysis
      became,
      noLonger,
      edge,

      -- * Modifiers
      (<&),
      (&>),
      dropE,
      dropWhileE,
      filterE,
      merge,
      mergeL,
      mergeR,
      notYet,
      once,
      takeE,
      takeWhileE,

      -- * Scans
      accumE,
      accum1E,
      iterateE,
      -- ** Special scans
      maximumE,
      minimumE,
      productE,
      sumE
    )
    where

import Control.Applicative
import Control.Arrow
import Control.Monad.Fix
import Control.Wire.Core
import Control.Wire.Session
import Control.Wire.Unsafe.Event
import Data.Fixed


-- | Merge events with the leftmost event taking precedence.  Equivalent
-- to using the monoid interface with 'First'.  Infixl 5.
--
-- * Depends: now on both.
--
-- * Inhibits: when any of the two wires inhibit.

(<&) :: (Monad m) => Wire s e m a (Event b) -> Wire s e m a (Event b) -> Wire s e m a (Event b)
(<&) = liftA2 (merge const)

infixl 5 <&


-- | Merge events with the rightmost event taking precedence.
-- Equivalent to using the monoid interface with 'Last'.  Infixl 5.
--
-- * Depends: now on both.
--
-- * Inhibits: when any of the two wires inhibit.

(&>) :: (Monad m) => Wire s e m a (Event b) -> Wire s e m a (Event b) -> Wire s e m a (Event b)
(&>) = liftA2 (merge (const id))

infixl 5 &>


-- | Left scan for events.  Each time an event occurs, apply the given
-- function.
--
-- * Depends: now.

accumE ::
    (b -> a -> b)  -- ^ Fold function
    -> b           -- ^ Initial value.
    -> Wire s e m (Event a) (Event b)
accumE f = loop
    where
    loop x' =
        mkSFN $
            event (NoEvent, loop x')
                  (\y -> let x = f x' y in (Event x, loop x))


-- | Left scan for events with no initial value.  Each time an event
-- occurs, apply the given function.  The first event is produced
-- unchanged.
--
-- * Depends: now.

accum1E ::
    (a -> a -> a)  -- ^ Fold function
    -> Wire s e m (Event a) (Event a)
accum1E f = initial
    where
    initial =
        mkSFN $ event (NoEvent, initial) (Event &&& accumE f)


-- | At the given point in time.
--
-- * Depends: now when occurring.

at ::
    (HasTime t s)
    => t  -- ^ Time of occurrence.
    -> Wire s e m a (Event a)
at t' =
    mkSF $ \ds x ->
        let t = t' - dtime ds
        in if t <= 0
             then (Event x, never)
             else (NoEvent, at t)


-- | Occurs each time the predicate becomes true for the input signal,
-- for example each time a given threshold is reached.
--
-- * Depends: now.

became :: (a -> Bool) -> Wire s e m a (Event a)
became p = off
    where
    off = mkSFN $ \x -> if p x then (Event x, on) else (NoEvent, off)
    on = mkSFN $ \x -> (NoEvent, if p x then on else off)


-- | Forget the first given number of occurrences.
--
-- * Depends: now.

dropE :: Int -> Wire s e m (Event a) (Event a)
dropE n | n <= 0 = mkId
dropE n =
    fix $ \again ->
    mkSFN $ \mev ->
        (NoEvent, if occurred mev then dropE (pred n) else again)


-- | Forget all initial occurrences until the given predicate becomes
-- false.
--
-- * Depends: now.

dropWhileE :: (a -> Bool) -> Wire s e m (Event a) (Event a)
dropWhileE p =
    fix $ \again ->
    mkSFN $ \mev ->
        case mev of
          Event x | not (p x) -> (mev, mkId)
          _ -> (NoEvent, again)


-- | Forget all occurrences for which the given predicate is false.
--
-- * Depends: now.

filterE :: (a -> Bool) -> Wire s e m (Event a) (Event a)
filterE p =
    mkSF_ $ \mev ->
        case mev of
          Event x | p x -> mev
          _ -> NoEvent


-- | On each occurrence, apply the function the event carries.
--
-- * Depends: now.

iterateE :: a -> Wire s e m (Event (a -> a)) (Event a)
iterateE = accumE (\x f -> f x)


-- | Maximum of all events.
--
-- * Depends: now.

maximumE :: (Ord a) => Wire s e m (Event a) (Event a)
maximumE = accum1E max


-- | Minimum of all events.
--
-- * Depends: now.

minimumE :: (Ord a) => Wire s e m (Event a) (Event a)
minimumE = accum1E min


-- | Left-biased event merge.

mergeL :: Event a -> Event a -> Event a
mergeL = merge const


-- | Right-biased event merge.

mergeR :: Event a -> Event a -> Event a
mergeR = merge (const id)


-- | Never occurs.

never :: Wire s e m a (Event b)
never = mkConst (Right NoEvent)


-- | Occurs each time the predicate becomes false for the input signal,
-- for example each time a given threshold is no longer exceeded.
--
-- * Depends: now.

noLonger :: (a -> Bool) -> Wire s e m a (Event a)
noLonger p = off
    where
    off = mkSFN $ \x -> if p x then (NoEvent, off) else (Event x, on)
    on = mkSFN $ \x -> (NoEvent, if p x then off else on)


-- | Events occur first when the predicate is false then when it is
-- true, and then this pattern repeats.
--
-- * Depends: now.

edge :: (a -> Bool) -> Wire s e m a (Event a)
edge p = off
    where
    off = mkSFN $ \x -> if p x then (Event x, on) else (NoEvent, off)
    on = mkSFN $ \x -> if p x then (NoEvent, on) else (Event x, off)


-- | Forget the first occurrence.
--
-- * Depends: now.

notYet :: Wire s e m (Event a) (Event a)
notYet =
    mkSFN $ event (NoEvent, notYet) (const (NoEvent, mkId))


-- | Occurs once immediately.
--
-- * Depends: now when occurring.

now :: Wire s e m a (Event a)
now = mkSFN $ \x -> (Event x, never)


-- | Forget all occurrences except the first.
--
-- * Depends: now when occurring.

once :: Wire s e m (Event a) (Event a)
once =
    mkSFN $ \mev ->
        (mev, if occurred mev then never else once)


-- | Periodic occurrence with the given time period.  First occurrence
-- is now.
--
-- * Depends: now when occurring.

periodic :: (HasTime t s) => t -> Wire s e m a (Event a)
periodic int | int <= 0 = error "periodic: Non-positive interval"
periodic int = mkSFN $ \x -> (Event x, loop int)
    where
    loop 0 = loop int
    loop t' =
        mkSF $ \ds x ->
            let t = t' - dtime ds
            in if t <= 0
                 then (Event x, loop (mod' t int))
                 else (NoEvent, loop t)


-- | Periodic occurrence with the given time period.  First occurrence
-- is now.  The event values are picked one by one from the given list.
-- When the list is exhausted, the event does not occur again.

periodicList :: (HasTime t s) => t -> [b] -> Wire s e m a (Event b)
periodicList int _ | int <= 0 = error "periodic: Non-positive interval"
periodicList _ [] = never
periodicList int (x:xs) = mkSFN $ \_ -> (Event x, loop int xs)
    where
    loop _ [] = never
    loop 0 xs = loop int xs
    loop t' xs0@(x:xs) =
        mkSF $ \ds _ ->
            let t = t' - dtime ds
            in if t <= 0
                 then (Event x, loop (mod' t int) xs)
                 else (NoEvent, loop t xs0)


-- | Product of all events.
--
-- * Depends: now.

productE :: (Num a) => Wire s e m (Event a) (Event a)
productE = accumE (*) 1


-- | Sum of all events.
--
-- * Depends: now.

sumE :: (Num a) => Wire s e m (Event a) (Event a)
sumE = accumE (+) 0


-- | Forget all but the first given number of occurrences.
--
-- * Depends: now.

takeE :: Int -> Wire s e m (Event a) (Event a)
takeE n | n <= 0 = never
takeE n =
    fix $ \again ->
    mkSFN $ \mev ->
        (mev, if occurred mev then takeE (pred n) else again)


-- | Forget all but the initial occurrences for which the given
-- predicate is true.
--
-- * Depends: now.

takeWhileE :: (a -> Bool) -> Wire s e m (Event a) (Event a)
takeWhileE p =
    fix $ \again ->
    mkSFN $ \mev ->
        case mev of
          Event x | not (p x) -> (NoEvent, never)
          _ -> (mev, again)
