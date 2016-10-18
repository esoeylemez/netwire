-- |
-- Module:     FRP.Netwire.Utils.Timeline
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module FRP.Netwire.Utils.Timeline
    ( -- * Time lines for statistics wires
      Timeline,

      -- * Constructing time lines
      insert,
      singleton,
      union,

      -- * Linear sampling
      linAvg,
      linCutL,
      linCutR,
      linLookup,

      -- * Staircase sampling
      scAvg,
      scCutL,
      scCutR,
      scLookup
    )
    where

import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Data
import Data.Map.Strict (Map)


-- | A time line is a non-empty set of samples together with time
-- information.

newtype Timeline t a =
    Timeline {
      timeline :: Map t a
    }
    deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Functor (Timeline t) where
    fmap f (Timeline m) = Timeline (M.map f m)


-- | Insert the given data point.

insert :: (Ord t) => t -> a -> Timeline t a -> Timeline t a
insert t x (Timeline m) = Timeline (M.insert t x m)


-- | Linearly interpolate the points in the time line, integrate the
-- given time interval of the graph, divide by the interval length.

linAvg ::
    (Fractional a, Fractional t, Real t)
    => t -> t -> Timeline t a -> a
linAvg t0 t1
    | t0 > t1 = const (error "linAvg: Invalid interval")
    | t0 == t1 = linLookup t0
linAvg t0 t1 = avg 0 . M.assocs . timeline . linCutR t1 . linCutL t0
    where
    avg a' ((t', y1) : xs@((t, y2) : _)) =
        let dt = realToFrac (t - t')
            a  = a' + dt*(y1 + y2)/2
        in a `seq` avg a xs
    avg a' _ = a' / realToFrac (t1 - t0)


-- | Cut the timeline at the given point in time @t@, such that all
-- samples up to but not including @t@ are forgotten.  The most recent
-- sample before @t@ is moved and interpolated accordingly.

linCutL ::
    (Fractional a, Fractional t, Real t)
    => t -> Timeline t a -> Timeline t a
linCutL t tl@(Timeline m) =
    Timeline $
    case M.splitLookup t m of
      (_, Just x, mr) -> M.insert t x mr
      (_, _, mr)      -> M.insert t (linLookup t tl) mr


-- | Cut the timeline at the given point in time @t@, such that all
-- samples later than @t@ are forgotten.  The most recent sample after
-- @t@ is moved and interpolated accordingly.

linCutR ::
    (Fractional a, Fractional t, Real t)
    => t -> Timeline t a -> Timeline t a
linCutR t tl@(Timeline m) =
    Timeline $
    case M.splitLookup t m of
      (ml, Just x, _) -> M.insert t x ml
      (ml, _, _)      -> M.insert t (linLookup t tl) ml


-- | Look up with linear sampling.

linLookup :: (Fractional a, Fractional t, Real t) => t -> Timeline t a -> a
linLookup t (Timeline m) =
    case M.splitLookup t m of
      (_, Just x, _) -> x
      (ml, _, mr)    ->
          case (fst <$> M.maxViewWithKey ml, fst <$> M.minViewWithKey mr) of
            (Just (t1, x1), Just (t2, x2)) ->
                let f = realToFrac ((t - t1) / (t2 - t1))
                in x1*(1 - f) + x2*f
            (Just (_, x), _) -> x
            (_, Just (_, x)) -> x
            _                -> error "linLookup: BUG: querying empty Timeline"


-- | Integrate the given time interval of the staircase, divide by the
-- interval length.

scAvg :: (Fractional a, Real t) => t -> t -> Timeline t a -> a
scAvg t0 t1
    | t0 > t1 = const (error "scAvg: Invalid interval")
    | t0 == t1 = scLookup t0
scAvg t0 t1 = avg 0 . M.assocs . timeline . scCutR t1 . scCutL t0
    where
    avg a' ((t', y) : xs@((t, _) : _)) =
        let dt = realToFrac (t - t')
            a  = a' + dt*y
        in a `seq` avg a xs
    avg a' _ = a' / realToFrac (t1 - t0)


-- | Cut the timeline at the given point in time @t@, such that all
-- samples up to but not including @t@ are forgotten.  The most recent
-- sample before @t@ is moved accordingly.

scCutL :: (Ord t) => t -> Timeline t a -> Timeline t a
scCutL t tl@(Timeline m) =
    Timeline $
    case M.splitLookup t m of
      (_, Just x, mr) -> M.insert t x mr
      (_, _, mr)      -> M.insert t (scLookup t tl) mr


-- | Cut the timeline at the given point in time @t@, such that all
-- samples later than @t@ are forgotten.  The earliest sample after @t@
-- is moved accordingly.

scCutR :: (Ord t) => t -> Timeline t a -> Timeline t a
scCutR t tl@(Timeline m) =
    Timeline $
    case M.splitLookup t m of
      (ml, Just x, _) -> M.insert t x ml
      (ml, _, _)      -> M.insert t (scLookup t tl) ml


-- | Look up on staircase.

scLookup :: (Ord t) => t -> Timeline t a -> a
scLookup t (Timeline m) =
    case (M.lookupLE t m, M.lookupGE t m) of
      (Just (_, x), _) -> x
      (_, Just (_, x)) -> x
      _                -> error "linLookup: BUG: querying empty Timeline"


-- | Singleton timeline with the given point.

singleton :: t -> a -> Timeline t a
singleton t = Timeline . M.singleton t


-- | Union of two time lines.  Right-biased.

union :: (Ord t) => Timeline t a -> Timeline t a -> Timeline t a
union (Timeline m1) (Timeline m2) = Timeline (M.union m2 m1)
