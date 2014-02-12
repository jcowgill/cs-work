{
-- Haskell Practical 2 Alex Code - N0 Lexer
--  By James Cowgill

module Prac2Alex(Token(..), alexScanTokens) where

import Data.Char
}

%wrapper "basic"

:-
    -- Whitespace and Comments
    $white+             ;
    "--".*              ;

    -- Keywords
    "true"              { \s -> TokBool True }
    "false"             { \s -> TokBool False }
    "skip"              { \s -> TokSkip }
    "if"                { \s -> TokIf }
    "then"              { \s -> TokThen }
    "else"              { \s -> TokElse }
    "while"             { \s -> TokWhile }

    -- Symbols
    [\=\<\+\-\*\;\(\)]  { \s -> TokSym (head s) }
    ":="                { \s -> TokAssign }

    -- Identifiers and Numbers
    [a-z]+              { \s -> TokVar s }
    0b[01]+             { \s -> TokNum (readBinary (drop 2 s)) }
    [0-9]+              { \s -> TokNum (read s) }

{
-- N0 Tokens
data Token =
    TokAssign |
    TokSkip | TokIf | TokThen | TokElse | TokWhile |
    TokBool Bool |
    TokSym Char |
    TokVar String |
    TokNum Int
    deriving (Eq, Show)

-- Converts binary numbers to integers
readBinary :: String -> Int
readBinary = (foldl1 f).(map digitToInt)
        where f a b = a * 2 + b
}