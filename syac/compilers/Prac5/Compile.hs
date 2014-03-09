-- Haskell Practical 5 - Compiler (Picture to Turtle Commands)
--  By James Cowgill

module Prac5.Compile(compile) where

import Prac5.Picture
import Prac5.Turtle

-- Generates commands to move turtle forwards given number of steps
--  Direction is preserved
advance :: Int -> [Command]
advance n | n >= 0    = replicate n Forward
advance n | otherwise = [Rotate, Rotate] ++
                        advance (-n) ++
                        [Rotate, Rotate]

-- Moves the turtle to a position relative to the current position
--  Assumes turtle is facing east
--  Direction is preserved
move :: Int -> Int -> [Command]
move x y = advance x ++
           [Rotate] ++
           advance y ++
           [Rotate, Rotate, Rotate]

-- Draws a horizontal or vertical line (see "horizontal")
--  Lists are pre-draw and post-draw commands
line :: [Command] -> [Command] -> Int -> Int -> Int -> [Command]
line pre post _ _ 0 = []
line pre post x y n = move x y ++
                      pre ++
                      [PenDown] ++
                      advance nDec ++
                      [PenUp] ++
                      advance (-nDec) ++
                      post ++
                      move (-x) (-y)
  where
    nDec = n - signum n     -- nDec = 1 unit closer to 0

-- Draw horizontal line with relative coords and length
--  Assumes turtle is facing east and pen is raised
--  Direction, pen and position are preserved
horizontal :: Int -> Int -> Int -> [Command]
horizontal  = line [] []

-- As "horizontal" but draws a vertical line
vertical :: Int -> Int -> Int -> [Command]
vertical = line [Rotate] [Rotate, Rotate, Rotate]

-- Compiles a picture to a series of turtle commands
--  Assumes initial state is facing east, pen raised, at (0, 0)
compile :: Picture -> [Command]
compile []     = []
compile (c:cs) = compileCmd c ++ compile cs
  where
    compileCmd (CmdHor (x, y) n) = horizontal x y n
    compileCmd (CmdVer (x, y) n) = vertical   x y n
