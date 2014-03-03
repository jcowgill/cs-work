-- Haskell Practical 3 Lexer for fuller prepositional calculus
--  By James Cowgill

module Prac3.Lexer where

import Data.Char

-- Lexer tokens
data Token =
    TokTrue |
    TokFalse |
    TokNegate |
    TokAnd |
    TokOr |
    TokImplies |
    TokEqual |
    TokLeft |
    TokRight |
    TokVar String
    deriving (Eq, Show)

-- Converts single characters into a token
lexerSingle :: Char -> Token
lexerSingle 'T'                = TokTrue
lexerSingle 'F'                = TokFalse
lexerSingle '-'                = TokNegate
lexerSingle '*'                = TokAnd
lexerSingle '+'                = TokOr
lexerSingle '>'                = TokImplies
lexerSingle '='                = TokEqual
lexerSingle '('                = TokLeft
lexerSingle ')'                = TokRight
lexerSingle c                  = error ("lexical error: " ++ [c])

-- Simple lexer for prepositional calculus
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isSpace x     = lexer xs
    | isVariable    = TokVar vFst : lexer vSnd
    | otherwise     = lexerSingle x : lexer xs
    where
        (vFst, vSnd)    = span (isAsciiLower) (x:xs)
        isVariable      = not (null vFst)
