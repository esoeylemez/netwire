-- |
-- Module:     FRP.Netwire.Move
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module FRP.Netwire.Move
    ( -- * Calculus
      derivative,
      integral,
      integralWith
    )
    where

import Control.Wire


-- | Time derivative of the input signal.
--
-- * Depends: now.
--
-- * Inhibits: at singularities.

derivative ::
    (RealFloat a, HasTime t s, Monoid e)
    => Wire s e m a a
derivative = mkPure $ \_ x -> (Left mempty, loop x)
    where
    loop x' =
        mkPure $ \ds x ->
            let dt  = realToFrac (dtime ds)
                dx  = (x - x') / dt
                mdx | isNaN dx      = Right 0
                    | isInfinite dx = Left mempty
                    | otherwise     = Right dx
            in mdx `seq` (mdx, loop x)


-- | Integrate the input signal over time.
--
-- * Depends: before now.

integral ::
    (Fractional a, HasTime t s)
    => a  -- ^ Integration constant (aka start value).
    -> Wire s e m a a
integral x' =
    mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
        in x' `seq` (Right x', integral (x' + dt*dx))


-- | Integrate the left input signal over time, but apply the given
-- correction function to it.  This can be used to implement collision
-- detection/reaction.
--
-- The right signal of type @w@ is the /world value/.  It is just passed
-- to the correction function for reference and is not used otherwise.
--
-- The correction function must be idempotent with respect to the world
-- value: @f w (f w x) = f w x@.  This is necessary and sufficient to
-- protect time continuity.
--
-- * Depends: before now.

integralWith ::
    (Fractional a, HasTime t s)
    => (w -> a -> a)  -- ^ Correction function.
    -> a              -- ^ Integration constant (aka start value).
    -> Wire s e m (a, w) a
integralWith correct = loop
    where
    loop x' =
        mkPure $ \ds (dx, w) ->
            let dt = realToFrac (dtime ds)
                x  = correct w (x' + dt*dx)
            in x' `seq` (Right x', loop x)
