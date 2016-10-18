-- |
-- Module:     Control.Wire.Run
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Run
    ( -- * Testing wires
      testWire,
      testWireM
    )
    where

import Control.Monad.IO.Class
import Control.Wire.Core
import Control.Wire.Session
import Data.Functor.Identity
import System.IO


-- | This function runs the given wire using the given state delta
-- generator.  It constantly shows the output of the wire on one line on
-- stdout.  Press Ctrl-C to abort.

testWire ::
    (MonadIO m, Show b, Show e)
    => Session m s
    -> (forall a. Wire s e Identity a b)
    -> m c
testWire s0 w0 = loop s0 w0
    where
    loop s' w' = do
        (ds, s) <- stepSession s'
        let Identity (mx, w) = stepWire w' ds (Right ())
        liftIO $ do
            putChar '\r'
            putStr (either (\ex -> "I: " ++ show ex) show mx)
            putStr "\027[K"
            hFlush stdout
        loop s w


-- | This function runs the given wire using the given state delta
-- generator.  It constantly shows the output of the wire on one line on
-- stdout.  Press Ctrl-C to abort.

testWireM ::
    (Monad m', MonadIO m, Show b, Show e)
    => (forall a. m' a -> m a)
    -> Session m s
    -> (forall a. Wire s e m' a b)
    -> m c
testWireM run s0 w0 = loop s0 w0
    where
    loop s' w' = do
        (ds, s) <- stepSession s'
        (mx, w) <- run (stepWire w' ds (Right ()))
        liftIO $ do
            putChar '\r'
            putStr (either (\ex -> "I: " ++ show ex) show mx)
            putStr "\027[K"
            hFlush stdout
        loop s w
