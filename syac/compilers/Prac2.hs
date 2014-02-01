-- Haskell Practical 2 Code - N0 Lexer
--  By James Cowgill

import Data.Char
import Data.Maybe
import Prac2Lexer

-- N0 Tokens
data Token =
    TokTrue | TokFalse |
    TokEquals | TokLess |
    TokPlus | TokMinus | TokTimes |
    TokAssign |
    TokSkip |
    TokSeq | TokIf | TokThen | TokElse | TokWhile |
    TokLeft | TokRight |
    TokVar String |
    TokNum Integer
    deriving (Eq, Show)

-- Constant tokens
constTokens :: [(String, Token)]
constTokens =
       [("true",    TokTrue),
        ("false",   TokFalse),
        ("=",       TokEquals),
        ("<",       TokLess),
        ("+",       TokPlus),
        ("-",       TokMinus),
        ("*",       TokTimes),
        (":=",      TokAssign),
        ("skip",    TokSkip),
        (";",       TokSeq),
        ("if",      TokIf),
        ("then",    TokThen),
        ("else",    TokElse),
        ("while",   TokWhile),
        ("(",       TokLeft),
        (")",       TokRight)]

-- Regexes for N0 terminals
nWord :: RegExp Char
nWord = itrPlus (range isAsciiLower)

nNumeral :: RegExp Char
nNumeral = itrPlus (range isDigit)

nWhitespace :: RegExp Char
nWhitespace = itr (range isSpace)

-- Converts lexemes to tokens
toToken :: String -> Token
toToken str | isJust keywordToken    = fromJust keywordToken
            | matched (nWord str)    = TokVar str
            | matched (nNumeral str) = TokNum (read str)
            where
                keywordToken = lookup str constTokens

-- Regex to match all lexemes
nRegExpKeywords :: RegExp Char
nRegExpKeywords = foldl1 alt (map sqns (fst (unzip constTokens)))

nRegExp :: RegExp Char
nRegExp =  nWord `alt` nNumeral `alt` nRegExpKeywords

-- Scanner for N0
nScanner :: String -> [Token]
nScanner = scanner nRegExp nWhitespace toToken
