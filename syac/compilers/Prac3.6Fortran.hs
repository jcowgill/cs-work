-- Haskell Practical 3 Code - Fortran like variable names
--  By James Cowgill

import Data.Char
import Prac2Lexer
import Prac3Combinators

{-
Original Grammar
    a   = a + b | ^N
    b   = b * a | $N

Remove left-recursion
    a   = ^N a'
    a'  = + b a'
        | λ
    b   = $N b'
    b'  = + a b'
        | λ
-}

-- Lexer
data Token = TokPlus | TokTimes | TokUp | TokDown | TokVar String
    deriving (Eq, Show)

lexerRegexp :: RegExp Char
lexerRegexp = (itrPlus (range isAsciiLower)) `alt` (range (`elem` "+*^$"))

lexerWhitespace :: RegExp Char
lexerWhitespace = itr (range isSpace)

lexerConvert :: String -> Token
lexerConvert "+" = TokPlus
lexerConvert "*" = TokTimes
lexerConvert "^" = TokUp
lexerConvert "$" = TokDown
lexerConvert v   = TokVar v

lexer :: String -> [Token]
lexer = scanner lexerRegexp lexerWhitespace lexerConvert

-- Parser
n :: Parser Token
n (TokVar v:es) = (True, es)
n es            = (False, es)

a   =  terminal TokUp +> n +> a'
a'  =  terminal TokPlus +> b +> a'
    <> empty
b   =  terminal TokDown +> n +> b'
b'  =  terminal TokTimes +> a +> b'
    <> empty

-- Final function
isFortranLang :: String -> Bool
isFortranLang str = rFst && null rSnd
    where
        (rFst, rSnd) = a (lexer str)
