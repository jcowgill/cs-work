{
-- Haskell Practical 5 - Drawing Code Lexer
--  By James Cowgill

module Prac5.Lexer(Token(..), alexScanTokens) where
}

%wrapper "basic"

:-
    -- Whitespace and Comments
    $white+             ;
    "//".*              ;
    "/*"[^]*"*/"        ;

    -- Keywords and symbols
    "hor"       { \s -> TokHor }
    "ver"       { \s -> TokVer }
    ","         { \s -> TokComma }
    ";"|"+"     { \s -> TokPlus }

    -- Numbers
    "-"?[0-9]+  { \s -> TokNum (read s) }

{
-- Tokens
data Token =
    TokHor | TokVer |
    TokComma | TokPlus |
    TokNum Int
    deriving (Eq, Show)
}
