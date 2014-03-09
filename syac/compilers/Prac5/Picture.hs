-- Haskell Practical 5 - Picture
--  By James Cowgill

module Prac5.Picture(Picture, PicCommand(..), box, count, readPicture, drawPicture) where

import Data.List
import Prac5.Canvas
import Prac5.Lexer
import Prac5.Parser

-- 3 by 3 box
box :: Picture
box = [CmdHor (0, 0) 3,
       CmdHor (2, 2) (-3),
       CmdVer (0, 0) 3,
       CmdVer (2, 0) 3]

-- Count lines in picture
count :: Picture -> Int
count = length

-- Read picture from string
readPicture :: String -> Picture
readPicture = doParse . alexScanTokens

-- Draw picture to canvas
drawPicture :: Picture -> Canvas
drawPicture p = foldl' overlay [] (map drawCmd p)
  where
    drawCmd (CmdHor c l) = hor c l
    drawCmd (CmdVer c l) = ver c l
