{
-- Haskell Practical 2 Alex Code - N0 Lexer
--  By James Cowgill

module Prac2Alex(Token(..), alexScanTokens) where
}

%wrapper "basic"

:-

    -- Whitespace
    $white+     ;

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

    -- Identifiers and numbers
    [a-z]+              { \s -> TokVar s }
    [0-9]+              { \s -> TokNum (read s) }

{
-- N0 Tokens
data Token =
    TokAssign |
    TokSkip | TokIf | TokThen | TokElse | TokWhile |
    TokBool Bool |
    TokSym Char |
    TokVar String |
    TokNum Integer
    deriving (Eq, Show)
}
