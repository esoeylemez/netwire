-- |
-- Module:     Control.Wire.Unsafe.Event
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Unsafe.Event
    ( -- * Events
      Event(..),

      -- * Helper functions
      event,
      merge,
      occurred,
      onEventM
    )
    where

import Control.DeepSeq
import Control.Monad
import Control.Wire.Core
import Data.Semigroup
import Data.Typeable


-- | Denotes a stream of values, each together with time of occurrence.
-- Since 'Event' is commonly used for functional reactive programming it
-- does not define most of the usual instances to protect continuous
-- time and discrete event occurrence semantics.

data Event a = Event a | NoEvent  deriving (Typeable)

instance Functor Event where
    fmap f = event NoEvent (Event . f)

instance (Semigroup a) => Monoid (Event a) where
    mempty = NoEvent
    mappend = (<>)

instance (NFData a) => NFData (Event a) where
    rnf (Event x) = rnf x
    rnf NoEvent   = ()

instance (Semigroup a) => Semigroup (Event a) where
    (<>) = merge (<>)


-- | Fold the given event.

event :: b -> (a -> b) -> Event a -> b
event _ j (Event x) = j x
event n _ NoEvent   = n


-- | Merge two events using the given function when both occur at the
-- same time.

merge :: (a -> a -> a) -> Event a -> Event a -> Event a
merge _ NoEvent NoEvent     = NoEvent
merge _ (Event x) NoEvent   = Event x
merge _ NoEvent (Event y)   = Event y
merge f (Event x) (Event y) = Event (f x y)


-- | Did the given event occur?

occurred :: Event a -> Bool
occurred = event False (const True)


-- | Each time the given event occurs, perform the given action with the
-- value the event carries.  The resulting event carries the result of
-- the action.
--
-- * Depends: now.

onEventM :: (Monad m) => (a -> m b) -> Wire s e m (Event a) (Event b)
onEventM c = mkGen_ $ liftM Right . event (return NoEvent) (liftM Event . c)
