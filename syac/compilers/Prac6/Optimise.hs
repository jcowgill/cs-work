-- Haskell Practical 5 - Optimiser
--  By James Cowgill

module Prac6.Optimise where

import Prac5.Compile
import Prac5.Turtle

-- Run function until a fixed point is reached
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f as = continue (f as) as
  where
    continue as prevAs
      | as == prevAs = as
      | otherwise    = continue (f as) as

-- Peephole optimization using rewriting
rewriteOne :: [Command] -> [Command]
rewriteOne (Rotate:Rotate:Rotate:Rotate:cs) = cs
rewriteOne (PenDown:PenDown:cs) = PenDown:cs
rewriteOne (PenUp  :PenUp  :cs) = PenUp:cs
rewriteOne (PenUp  :PenDown:cs) = PenDown:cs
rewriteOne (PenDown:Rotate :cs) = Rotate:PenDown:cs
rewriteOne (Rotate :PenUp  :cs) = PenUp:Rotate:cs
rewriteOne (c:cs) = c:rewriteOne cs
rewriteOne [] = []

-- Test code
testOpt :: [Command]
testOpt =
    [Rotate, Forward, Forward, Forward, Forward, Rotate, Rotate, Rotate, PenDown, Forward,
    Forward, Forward, Forward, Rotate, Rotate, Rotate, PenUp, Rotate, Rotate, Rotate,
    Forward, Forward, Forward, Forward, Rotate, Rotate, Rotate, Rotate, Rotate, Forward,
    Forward, Forward, Forward, Rotate, Rotate, Rotate, Rotate, Rotate, Forward, Rotate,
    Forward, Forward, Forward, Rotate, Rotate, Rotate, PenUp, PenDown, Forward, Forward,
    PenUp, Rotate, Rotate, Forward, Forward, Forward, Rotate, Rotate, Rotate, Rotate,
    Rotate, Forward, Forward, Forward, Rotate, Rotate, Rotate, Rotate, Rotate, Forward,
    Forward, Rotate, Forward, Forward, PenDown, Rotate, Rotate, Rotate, PenDown]
