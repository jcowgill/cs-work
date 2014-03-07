-- Haskell Practical 5 - Turtle
--  By James Cowgill

module Prac5.Turtle where

import Prac5.Canvas
import Prac5.Picture

-- Turtle commands (rotate = 90° clockwise)
data TurtleCmd = Rotate | Forward | PenUp | PenDown
    deriving (Eq, Show)

-- Box in turtle
turtleBox :: [TurtleCmd]
turtleBox = [PenDown, Forward, Forward,
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

forward :: Direction -> Coords -> Coords
forward North (x, y) = (x, y - 1)
forward East  (x, y) = (x + 1, y)
forward South (x, y) = (x, y + 1)
forward West  (x, y) = (x - 1, y)

-- Turtle state
--  commands to execute, location, direction, is pen down?
type TurtleState = ([TurtleCmd], Coords, Direction, Bool)

-- Execute one turtle command
turtleStep :: TurtleState -> (TurtleState, Canvas)
turtleStep s@([], _, _, _)          = (s, [])
turtleStep ((c:cs), pos, dir, pen)  = ((cs, newPos, newDir, newPen), genC)
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
turtleExec :: TurtleState -> Canvas
turtleExec = exec []
  where
    exec c ([], _, _, _) = c
    exec c s             = let (newS, genC) = turtleStep s
                           in exec (overlay c genC) newS

-- Execute turtle code with default initial state
follow :: [TurtleCmd] -> Canvas
follow cs = turtleExec (cs, (0, 0), East, False)

-- Example 2
example2 :: [TurtleCmd]
example2 =
    [Rotate, PenDown, Forward, Forward, Forward, Forward, PenUp,
    Rotate, Rotate, Forward, Forward, Rotate, PenDown, Forward,
    Forward, PenUp, Rotate, Forward, Forward, Rotate, Rotate,
    PenDown, Forward, Forward, Forward, Forward, PenUp, Rotate,
    Forward, Forward, Rotate, PenDown, Forward, Forward, Forward,
    Forward, PenUp, Rotate, Rotate, Rotate, Forward, Forward,
    Rotate, Rotate, Rotate, PenDown, PenUp, Forward, Forward,
    PenDown, Forward, Forward]
