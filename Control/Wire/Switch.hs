-- |
-- Module:     Control.Wire.Switch
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Switch
    ( -- * Simple switching
      (-->),
      (>--),
      -- * Context switching
      modes,

      -- * Event-based switching
      -- ** Intrinsic
      switch,
      dSwitch,
      -- ** Intrinsic continuable
      kSwitch,
      dkSwitch,
      -- ** Extrinsic
      rSwitch,
      drSwitch,
      alternate,
      -- ** Extrinsic continuable
      krSwitch,
      dkrSwitch
    )
    where

import qualified Data.Map as M
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Wire.Core
import Control.Wire.Event
import Control.Wire.Unsafe.Event
import Data.Monoid


-- | Acts like the first wire until it inhibits, then switches to the
-- second wire.  Infixr 1.
--
-- * Depends: like current wire.
--
-- * Inhibits: after switching like the second wire.
--
-- * Switch: now.

(-->) :: (Monad m) => Wire s e m a b -> Wire s e m a b -> Wire s e m a b
w1' --> w2' =
    WGen $ \ds mx' -> do
        (mx, w1) <- stepWire w1' ds mx'
        case mx of
          Left _ | Right _ <- mx' -> stepWire w2' ds mx'
          _                       -> mx `seq` return (mx, w1 --> w2')

infixr 1 -->

-- | Acts like the first wire until the second starts producing, at which point
-- it switches to the second wire.  Infixr 1.
--
-- * Depends: like current wire.
--
-- * Inhibits: after switching like the second wire.
--
-- * Switch: now.

(>--) :: (Monad m) => Wire s e m a b -> Wire s e m a b -> Wire s e m a b
w1' >-- w2' =
    WGen $ \ds mx' -> do
        (m2, w2) <- stepWire w2' ds mx'
        case m2 of
          Right _ -> m2 `seq` return (m2, w2)
          _       -> do (m1, w1) <- stepWire w1' ds mx'
                        m1 `seq` return (m1, w1 >-- w2)

infixr 1 >--


-- | Intrinsic continuable switch:  Delayed version of 'kSwitch'.
--
-- * Inhibits: like the first argument wire, like the new wire after
--   switch.  Inhibition of the second argument wire is ignored.
--
-- * Switch: once, after now, restart state.

dkSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, b) (Event (Wire s e m a b -> Wire s e m a b))
    -> Wire s e m a b
dkSwitch w1' w2' =
    WGen $ \ds mx' -> do
        (mx,  w1) <- stepWire w1' ds mx'
        (mev, w2) <- stepWire w2' ds (liftA2 (,) mx' mx)
        let w | Right (Event sw) <- mev = sw w1
              | otherwise = dkSwitch w1 w2
        return (mx, w)


-- | Extrinsic switch:  Delayed version of 'rSwitch'.
--
-- * Inhibits: like the current wire.
--
-- * Switch: recurrent, after now, restart state.

drSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b)) b
drSwitch w' =
    WGen $ \ds mx' ->
        let nw w | Right (_, Event w1) <- mx' = w1
                 | otherwise = w
        in liftM (second (drSwitch . nw)) (stepWire w' ds (fmap fst mx'))


-- | Acts like the first wire until an event occurs then switches
-- to the second wire. Behaves like this wire until the event occurs
-- at which point a *new* instance of the first wire is switched to.
--
-- * Depends: like current wire.
--
-- * Inhibits: like the argument wires.
--
-- * Switch: once, now, restart state.

alternate ::
  (Monad m)
  => Wire s e m a b
  -> Wire s e m a b
  -> Wire s e m (a, Event x) b
alternate w1 w2 = go w1 w2 w1
    where
    go w1' w2' w' =
        WGen $ \ds mx' ->
            let (w1, w2, w) | Right (_, Event _) <- mx' = (w2', w1', w2')
                            | otherwise  = (w1', w2', w')
            in liftM (second (go w1 w2)) (stepWire w ds (fmap fst mx'))


-- | Intrinsic switch:  Delayed version of 'switch'.
--
-- * Inhibits: like argument wire until switch, then like the new wire.
--
-- * Switch: once, after now, restart state.

dSwitch ::
    (Monad m)
    => Wire s e m a (b, Event (Wire s e m a b))
    -> Wire s e m a b
dSwitch w' =
    WGen $ \ds mx' -> do
        (mx, w) <- stepWire w' ds mx'
        let nw | Right (_, Event w1) <- mx = w1
               | otherwise = dSwitch w
        return (fmap fst mx, nw)


-- | Extrinsic continuable switch.  Delayed version of 'krSwitch'.
--
-- * Inhibits: like the current wire.
--
-- * Switch: recurrent, after now, restart state.

dkrSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b -> Wire s e m a b)) b
dkrSwitch w' =
    WGen $ \ds mx' ->
        let nw w | Right (_, Event f) <- mx' = f w
                 | otherwise = w
        in liftM (second (dkrSwitch . nw)) (stepWire w' ds (fmap fst mx'))


-- | Intrinsic continuable switch:  @kSwitch w1 w2@ starts with @w1@.
-- Its signal is received by @w2@, which may choose to switch to a new
-- wire.  Passes the wire we are switching away from to the new wire,
-- such that it may be reused in it.
--
-- * Inhibits: like the first argument wire, like the new wire after
--   switch.  Inhibition of the second argument wire is ignored.
--
-- * Switch: once, now, restart state.

kSwitch ::
    (Monad m, Monoid s)
    => Wire s e m a b
    -> Wire s e m (a, b) (Event (Wire s e m a b -> Wire s e m a b))
    -> Wire s e m a b
kSwitch w1' w2' =
    WGen $ \ds mx' -> do
        (mx,  w1) <- stepWire w1' ds mx'
        (mev, w2) <- stepWire w2' ds (liftA2 (,) mx' mx)
        case mev of
          Right (Event sw) -> stepWire (sw w1) mempty mx'
          _                -> return (mx, kSwitch w1 w2)


-- | Extrinsic continuable switch.  This switch works like 'rSwitch',
-- except that it passes the wire we are switching away from to the new
-- wire.
--
-- * Inhibits: like the current wire.
--
-- * Switch: recurrent, now, restart state.

krSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b -> Wire s e m a b)) b
krSwitch w'' =
    WGen $ \ds mx' ->
        let w' | Right (_, Event f) <- mx' = f w''
               | otherwise = w''
        in liftM (second krSwitch) (stepWire w' ds (fmap fst mx'))


-- | Route the left input signal based on the current mode.  The right
-- input signal can be used to change the current mode.  When switching
-- away from a mode and then switching back to it, it will be resumed.
-- Freezes time during inactivity.
--
-- * Complexity: O(n * log n) space, O(log n) lookup time on switch wrt
--   number of started, inactive modes.
--
-- * Depends: like currently active wire (left), now (right).
--
-- * Inhibits: when active wire inhibits.
--
-- * Switch: now on mode change.

modes ::
    (Monad m, Ord k)
    => k  -- ^ Initial mode.
    -> (k -> Wire s e m a b)  -- ^ Select wire for given mode.
    -> Wire s e m (a, Event k) b
modes m0 select = loop M.empty m0 (select m0)
    where
    loop ms' m' w'' =
        WGen $ \ds mxev' ->
            case mxev' of
              Left _ -> do
                  (mx, w) <- stepWire w'' ds (fmap fst mxev')
                  return (mx, loop ms' m' w)
              Right (x', ev) -> do
                  let (ms, m, w') = switch ms' m' w'' ev
                  (mx, w) <- stepWire w' ds (Right x')
                  return (mx, loop ms m w)

    switch ms' m' w' NoEvent = (ms', m', w')
    switch ms' m' w' (Event m) =
        let ms = M.insert m' w' ms' in
        case M.lookup m ms of
          Nothing -> (ms, m, select m)
          Just w  -> (M.delete m ms, m, w)


-- | Extrinsic switch:  Start with the given wire.  Each time the input
-- event occurs, switch to the wire it carries.
--
-- * Inhibits: like the current wire.
--
-- * Switch: recurrent, now, restart state.

rSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b)) b
rSwitch w'' =
    WGen $ \ds mx' ->
        let w' | Right (_, Event w1) <- mx' = w1
               | otherwise = w''
        in liftM (second rSwitch) (stepWire w' ds (fmap fst mx'))


-- | Intrinsic switch:  Start with the given wire.  As soon as its event
-- occurs, switch to the wire in the event's value.
--
-- * Inhibits: like argument wire until switch, then like the new wire.
--
-- * Switch: once, now, restart state.

switch ::
    (Monad m, Monoid s)
    => Wire s e m a (b, Event (Wire s e m a b))
    -> Wire s e m a b
switch w' =
    WGen $ \ds mx' -> do
        (mx, w) <- stepWire w' ds mx'
        case mx of
          Right (_, Event w1) -> stepWire w1 mempty mx'
          _                   -> return (fmap fst mx, switch w)
