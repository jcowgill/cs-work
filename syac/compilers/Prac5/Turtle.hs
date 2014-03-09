-- Haskell Practical 5 - Turtle
--  By James Cowgill

module Prac5.Turtle(Command(..), box, example2, follow)  where

import Prac5.Canvas

-- Turtle commands (rotate = 90° clockwise)
data Command = Rotate | Forward | PenUp | PenDown
    deriving (Eq, Show)

-- Box in turtle
box :: [Command]
box = [PenDown, Forward, Forward,
       Rotate,  Forward, Forward,
       Rotate,  Forward, Forward,
       Rotate,  Forward,
       PenUp]

-- Turtle direction
data Direction = North | South | East | West

rotate :: Direction -> Direction
rotate North = East
rotate East  = South
rotate South = West
rotate West  = North

forward :: Direction -> (Int, Int) -> (Int, Int)
forward North (x, y) = (x, y - 1)
forward East  (x, y) = (x + 1, y)
forward South (x, y) = (x, y + 1)
forward West  (x, y) = (x - 1, y)

-- Turtle state
--  commands to execute, location, direction, is pen down?
type State = ([Command], (Int, Int), Direction, Bool)

-- Execute one turtle command
step :: State -> (State, Canvas)
step s@([], _, _, _)          = (s, [])
step ((c:cs), pos, dir, pen)  = ((cs, newPos, newDir, newPen), genC)
  where
    newPen  | c == PenUp    = False
            | c == PenDown  = True
            | otherwise     = pen
    newDir  | c == Rotate   = rotate dir
            | otherwise     = dir
    newPos  | c == Forward  = forward dir pos
            | otherwise     = pos
    genC    | newPen        = [newPos]
            | otherwise     = []

-- Executes a turtle program with the given initial state
followState :: State -> Canvas
followState = exec []
  where
    exec c ([], _, _, _) = c
    exec c s             = let (newS, genC) = step s
                           in exec (overlay c genC) newS

-- Execute turtle code with default initial state
follow :: [Command] -> Canvas
follow cs = followState (cs, (0, 0), East, False)

-- Example 2
example2 :: [Command]
example2 =
    [Rotate, PenDown, Forward, Forward, Forward, Forward, PenUp,
    Rotate, Rotate, Forward, Forward, Rotate, PenDown, Forward,
    Forward, PenUp, Rotate, Forward, Forward, Rotate, Rotate,
    PenDown, Forward, Forward, Forward, Forward, PenUp, Rotate,
    Forward, Forward, Rotate, PenDown, Forward, Forward, Forward,
    Forward, PenUp, Rotate, Rotate, Rotate, Forward, Forward,
    Rotate, Rotate, Rotate, PenDown, PenUp, Forward, Forward,
    PenDown, Forward, Forward]
