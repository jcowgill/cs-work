-- Haskell Practical 2 Code - Lexing
--  By James Cowgill

module Prac2Lexer where

import Data.Maybe

-- Type of regular expression parsers
--  Input = input string
--  fst Output = the string witch matches this regex
--               or Nothing if the regex doesn't match
--  snd Output = rest of string to process
type RegExp a = [a] -> (Maybe [a], [a])

-- True if the regex worked
matched :: (Maybe [a], [a]) -> Bool
matched (f, s) = isJust f

-- True if matched and the match is not empty
matchedNonEmpty :: (Maybe [a], [a]) -> Bool
matchedNonEmpty (f, s) = isJust f && (not (null (fromJust f)))

-- Matches empty regex
nil :: RegExp a
nil xs = (Just [], xs)

-- Matches character set which matches a given function
range :: (a -> Bool) -> RegExp a
range f (x:xs) | f x = (Just [x], xs)
range _ xs           = (Nothing, xs)

-- Matches any one character
arb :: RegExp a
arb = range (const True)

-- Matches exactly the character given
one :: Eq a => a -> RegExp a
one c = range (==c)

-- Matches union of two regexs
alt :: RegExp a -> RegExp a -> RegExp a
alt l _ xs | matched (lMatch) = lMatch
    where lMatch = l xs
alt _ r xs = r xs

-- Matches sequence of two regexs
sqn :: RegExp a -> RegExp a -> RegExp a
sqn l r xs | isJust lFst && isJust rFst =
    (Just ((fromJust lFst) ++ (fromJust rFst)), rSnd)
    where
        (lFst, lSnd) = l xs
        (rFst, rSnd) = r lSnd
sqn _ _ xs = (Nothing, xs)

-- Matches a squence of characters
sqns :: Eq a => [a] -> RegExp a
sqns xs = foldl sqn nil (map one xs)

-- Matches kleene star of regex
itr :: RegExp a -> RegExp a
itr l xs | isJust lFst =
    (Just ((fromJust lFst) ++ (fromJust rFst)), rSnd)
    where
        (lFst, lSnd) = l xs
        (rFst, rSnd) = itr l lSnd
itr _ xs = nil xs

-- Matches kleene plus of regex
itrPlus :: RegExp a -> RegExp a
itrPlus l = l `sqn` (itr l)

-- Identify lexemes in string
--  arg1 = regex of valid regexes in language
--  arg2 = regex of whitespace
lexemes :: Show a => RegExp a -> RegExp a -> [a] -> [[a]]
lexemes r w xs | matchedNonEmpty wMatch = lexemes r w (snd wMatch)
    where wMatch = w xs
lexemes r w xs | isJust rFst = (fromJust rFst):lexemes r w rSnd
    where (rFst, rSnd) = r xs
lexemes _ _ [] = []
lexemes _ _ xs = error ("string could not be parsed: " ++ show xs)

-- The main lexical scanner
--  arg1 = regex of valid regexes in language
--  arg2 = regex of whitespace
--  arg3 = string -> token function
scanner :: Show a => RegExp a -> RegExp a -> ([a] -> b) -> [a] -> [b]
scanner r w t cs = map t (lexemes r w cs)
