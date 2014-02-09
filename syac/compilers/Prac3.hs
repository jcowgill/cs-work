-- Haskell Practical 3 Code - Parsing
--  By James Cowgill

import Data.Char
import Prac3ParserCommon

-- Removes all whitespace from a string
removeSpace :: String -> String
removeSpace = filter (not.isSpace)

-- Main parser rules
parserP :: Parser Char
parserP =  terminal 'T'
        <> terminal 'F'
        <> parserV
        <> terminal '-' +> parserP
        <> terminal '(' +> parserP +> terminal '*' +> parserP +> terminal ')'

-- Variable names parser
parserV :: Parser Char
parserV (x:xs) | isAsciiLower x = (True, xs)
parserV xs                      = (False, xs)

-- Tests for valid prepositional calculus strings
parser :: String -> Bool
parser str = rFst && (null rSnd)
    where
        (rFst, rSnd) = parserP (removeSpace str)
