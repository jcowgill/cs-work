-- Haskell Practical 5 - Canvas
--  By James Cowgill

module Prac5.Canvas(Canvas, plus, hor, ver, overlay, render) where

import Data.List

-- A canvas is a grid where each point can be black or white
--  Stored as a list of black points
--  They do not have fixed sizes, they can expand in any direction
type Canvas = [(Int, Int)]

-- Plus symbol
plus :: Canvas
plus = [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]

-- Range function
--  Generates a list of values from n to 0
--  Returned list does not include the value n itself
range :: Int -> [Int]
range n | n < 0     = [n+1..0]
        | otherwise = [0..n-1]

-- Horizontal lines
hor :: (Int, Int) -> Int -> Canvas
hor (x, y) len = [(x + n, y) | n <- range len]

-- Vertical lines
ver :: (Int, Int) -> Int -> Canvas
ver (x, y) len = [(x, y + n) | n <- range len]

-- Overlay two canvases
overlay :: Canvas -> Canvas -> Canvas
overlay = union

-- Canvas boundaries
--  Returns left most and right most x coordinates
xbounds :: Canvas -> (Int, Int)
xbounds [] = (0, 0)
xbounds c = (minimum xs, maximum xs)
  where
    xs = map fst c

--  Returns top most and bottom most y coordinates
ybounds :: Canvas -> (Int, Int)
ybounds [] = (0, 0)
ybounds c = (minimum ys, maximum ys)
  where
    ys = map snd c

-- Returns the character to print for a cell
cell :: Canvas -> (Int, Int) -> Char
cell c coords | elem coords c = '#'
              | otherwise     = ' '

-- Renders a canvas
render :: Canvas -> String
render c = canvas top
  where
    canvas y | y > bottom = []
             | otherwise  = row y left ++ canvas (y + 1)
    row y x  | x > right  = "\n"
             | otherwise  = cell c (x, y) : row y (x + 1)
    (left, right) = xbounds c
    (top, bottom) = ybounds c
