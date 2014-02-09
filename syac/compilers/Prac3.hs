-- Haskell Practical 3 Code - Parsing
--  By James Cowgill

import Data.Char
import Prac3ParserCommon

-- Lexer tokens
data Token =
    TokTrue |
    TokFalse |
    TokNegate |
    TokAnd |
    TokLeft |
    TokRight |
    TokVar Char
    deriving (Eq, Show)

-- Converts single characters into a token
lexerSingle :: Char -> Token
lexerSingle 'T'                = TokTrue
lexerSingle 'F'                = TokFalse
lexerSingle '-'                = TokNegate
lexerSingle '*'                = TokAnd
lexerSingle '('                = TokLeft
lexerSingle ')'                = TokRight
lexerSingle c | isAsciiLower c = TokVar c
lexerSingle c                  = error ("lexical error: " ++ [c])

-- Simple lexer for prepositional calculus
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isSpace x = lexer xs
    | otherwise = (lexerSingle x) : lexer xs

-- Main parser rules
parserP :: Parser Token
parserP =  terminal TokTrue
        <> terminal TokFalse
        <> parserV
        <> terminal TokNegate +> parserP
        <> terminal TokLeft +> parserP +> terminal TokAnd +> parserP +> terminal TokRight

-- Variable names parser
parserV :: Parser Token
parserV (TokVar _:xs) = (True, xs)
parserV xs            = (False, xs)

-- Tests for valid prepositional calculus strings
parser :: [Token] -> Bool
parser xs = rFst && (null rSnd)
    where
        (rFst, rSnd) = parserP xs
