-- |
-- Module:     FRP.Netwire.Analyze
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module FRP.Netwire.Analyze
    ( -- * Linear graphs
      lAvg,
      lGraph,
      lGraphN,

      -- * Staircase graphs
      sAvg,
      sGraph,
      sGraphN,

      -- * Peaks
      highPeak,
      highPeakBy,
      lowPeak,
      lowPeakBy,

      -- * Debug
      avgFps,
      framerate
    )
    where

import qualified FRP.Netwire.Utils.Timeline as Tl
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Control.Wire
import Prelude hiding ((.), id)


-- | Average framerate over the last given number of samples.  One
-- important thing to note is that the value of this wire will generally
-- disagree with 'sAvg' composed with 'framerate'.  This is expected,
-- because this wire simply calculates the arithmetic mean, whereas
-- 'sAvg' will actually integrate the framerate graph.
--
-- Note:  This wire is for debugging purposes only, because it exposes
-- discrete time.  Do not taint your application with discrete time.
--
-- * Complexity: O(n) time and space wrt number of samples.

avgFps ::
    (RealFloat b, HasTime t s)
    => Int  -- ^ Number of samples.
    -> Wire s e m a b
avgFps int | int < 1 = error "avgFps: Non-positive number of samples"
avgFps int = loop Seq.empty
    where
    intf = fromIntegral int
    afps = (/ intf) . F.foldl' (+) 0

    loop ss' =
        mkSF $ \ds _ ->
            let fps = recip . realToFrac . dtime $ ds
                ss  = Seq.take int (fps Seq.<| ss')
            in if isInfinite fps
                 then (afps ss', loop ss')
                 else ss `seq` (afps ss, loop ss)


-- | Current framerate.
--
-- Note:  This wire is for debugging purposes only, because it exposes
-- discrete time.  Do not taint your application with discrete time.
--
-- * Inhibits: when the clock stopped ticking.

framerate ::
    (Eq b, Fractional b, HasTime t s, Monoid e)
    => Wire s e m a b
framerate =
    mkPure $ \ds _ ->
        let dt = realToFrac (dtime ds)
        in (if dt == 0 then Left mempty else Right (recip dt), framerate)


-- | High peak.
--
-- * Depends: now.

highPeak :: (Ord a) => Wire s e m a a
highPeak = highPeakBy compare


-- | High peak with respect to the given comparison function.
--
-- * Depends: now.

highPeakBy :: (a -> a -> Ordering) -> Wire s e m a a
highPeakBy = peakBy GT


-- | Calculate the average of the signal over the given interval (from
-- now).  This is done by calculating the integral of the corresponding
-- linearly interpolated graph and dividing it by the interval length.
-- See 'Tl.linAvg' for details.
--
-- Linear interpolation can be slow.  If you don't need it, you can use
-- the staircase variant 'sAvg'.
--
-- Example: @lAvg 2@
--
-- * Complexity: O(s) space, O(s) time wrt number of samples in the
--   interval.
--
-- * Depends: now.

lAvg ::
    (Fractional a, Fractional t, HasTime t s)
    => t    -- ^ Interval size.
    -> Wire s e m a a
lAvg int =
    mkSF $ \ds x ->
        let t = dtime ds in
        (x, loop t (Tl.singleton t x))

    where
    loop t' tl' =
        mkSF $ \ds x ->
            let t  = t' + dtime ds
                t0 = t - int
                tl = Tl.linCutL t0 (Tl.insert t x tl')
                a  = Tl.linAvg t0 t tl
            in (a, loop t tl)


-- | Produce a linearly interpolated graph for the given points in time,
-- where the magnitudes of the points are distances from /now/.
--
-- Linear interpolation can be slow.  If you don't need it, you can use
-- the faster staircase variant 'sGraph'.
--
-- Example: @lGraph [0, 1, 2]@ will output the interpolated inputs at
-- /now/, one second before now and two seconds before now.
--
-- * Complexity: O(s) space, O(n * log s) time, where s = number of
--   samples in the interval, n = number of requested data points.
--
-- * Depends: now.

lGraph ::
    (Fractional a, Fractional t, HasTime t s)
    => [t]  -- ^ Data points to produce.
    -> Wire s e m a [a]
lGraph qts =
    mkSF $ \ds x ->
        let t = dtime ds in
        (x <$ qts, loop t (Tl.singleton t x))

    where
    earliest = maximum (map abs qts)

    loop t' tl' =
        mkSF $ \ds x ->
            let t  = t' + dtime ds
                tl = Tl.linCutL (t - earliest) (Tl.insert t x tl')
                ps = map (\qt -> Tl.linLookup (t - abs qt) tl) qts
            in (ps, loop t tl)


-- | Graph the given interval from now with the given number of evenly
-- distributed points in time.  Convenience interface to 'lGraph'.
--
-- Linear interpolation can be slow.  If you don't need it, you can use
-- the faster staircase variant 'sGraphN'.
--
-- * Complexity: O(s) space, O(n * log s) time, where s = number of
--   samples in the interval, n = number of requested data points.
--
-- * Depends: now.

lGraphN ::
    (Fractional a, Fractional t, HasTime t s)
    => t    -- ^ Interval to graph from now.
    -> Int  -- ^ Number of data points to produce.
    -> Wire s e m a [a]
lGraphN int n
    | int <= 0 = error "lGraphN: Non-positive interval"
    | n <= 0   = error "lGraphN: Non-positive number of data points"
lGraphN int n =
    let n1   = n - 1
        f qt = realToFrac int * fromIntegral qt / fromIntegral n1
    in lGraph (map f [0..n1])


-- | Low peak.
--
-- * Depends: now.

lowPeak :: (Ord a) => Wire s e m a a
lowPeak = lowPeakBy compare


-- | Low peak with respect to the given comparison function.
--
-- * Depends: now.

lowPeakBy :: (a -> a -> Ordering) -> Wire s e m a a
lowPeakBy = peakBy LT


-- | Given peak with respect to the given comparison function.

peakBy ::
    (Eq o)
    => o  -- ^ This ordering means the first argument is larger.
    -> (a -> a -> o)  -- ^ Compare two elements.
    -> Wire s e m a a
peakBy o comp = mkSFN $ \x -> (x, loop x)
    where
    loop x' =
        mkSFN $ \x ->
            id &&& loop $
            if comp x x' == o then x else x'


-- | Calculate the average of the signal over the given interval (from
-- now).  This is done by calculating the integral of the corresponding
-- staircase graph and dividing it by the interval length.  See
-- 'Tl.scAvg' for details.
--
-- See also 'lAvg'.
--
-- Example: @sAvg 2@
--
-- * Complexity: O(s) space, O(s) time wrt number of samples in the
--   interval.
--
-- * Depends: now.

sAvg ::
    (Fractional a, Fractional t, HasTime t s)
    => t    -- ^ Interval size.
    -> Wire s e m a a
sAvg int =
    mkSF $ \ds x ->
        let t = dtime ds in
        (x, loop t (Tl.singleton t x))

    where
    loop t' tl' =
        mkSF $ \ds x ->
            let t  = t' + dtime ds
                t0 = t - int
                tl = Tl.scCutL t0 (Tl.insert t x tl')
                a  = Tl.scAvg t0 t tl
            in (a, loop t tl)


-- | Produce a staircase graph for the given points in time, where the
-- magnitudes of the points are distances from /now/.
--
-- See also 'lGraph'.
--
-- Example: @sGraph [0, 1, 2]@ will output the inputs at /now/, one
-- second before now and two seconds before now.
--
-- * Complexity: O(s) space, O(n * log s) time, where s = number of
--   samples in the interval, n = number of requested data points.
--
-- * Depends: now.

sGraph ::
    (Fractional t, HasTime t s)
    => [t]  -- ^ Data points to produce.
    -> Wire s e m a [a]
sGraph qts =
    mkSF $ \ds x ->
        let t = dtime ds in
        (x <$ qts, loop t (Tl.singleton t x))

    where
    earliest = maximum (map abs qts)

    loop t' tl' =
        mkSF $ \ds x ->
            let t  = t' + dtime ds
                tl = Tl.scCutL (t - earliest) (Tl.insert t x tl')
                ps = map (\qt -> Tl.scLookup (t - abs qt) tl) qts
            in (ps, loop t tl)


-- | Graph the given interval from now with the given number of evenly
-- distributed points in time.  Convenience interface to 'sGraph'.
--
-- See also 'lGraphN'.
--
-- * Complexity: O(s) space, O(n * log s) time, where s = number of
--   samples in the interval, n = number of requested data points.
--
-- * Depends: now.

sGraphN ::
    (Fractional t, HasTime t s)
    => t    -- ^ Interval to graph from now.
    -> Int  -- ^ Number of data points to produce.
    -> Wire s e m a [a]
sGraphN int n
    | int <= 0 = error "sGraphN: Non-positive interval"
    | n <= 0   = error "sGraphN: Non-positive number of data points"
sGraphN int n =
    let n1   = n - 1
        f qt = realToFrac int * fromIntegral qt / fromIntegral n1
    in sGraph (map f [0..n1])
