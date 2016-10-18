-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Monad.Fix
import Control.Wire
import Prelude hiding ((.), id)


wire :: SimpleWire a String
wire =
    holdFor 0.5 . periodicList 1 (cycle ["a", "b", "c"]) <|> "---"


main :: IO ()
main = testWire clockSession_ wire
