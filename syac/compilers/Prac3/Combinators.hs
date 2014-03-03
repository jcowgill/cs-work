-- Haskell Practical 3 Common Parsing Code
--  By James Cowgill

module Prac3.Combinators where

type Parse t = (Bool, [t])
type Parser t = [t] -> Parse t

-- Terminal symbol
terminal :: Eq t => t -> Parser t
terminal t (e:es) | t == e = (True, es)
terminal _ es              = (False, es)

-- Empty string
empty :: Parser t
empty ts = (True, ts)

-- Sequence
infixr 5 +>
(+>) :: Eq t => Parser t -> Parser t -> Parser t
(f +> g) ts
    | qf        = g rf
    | otherwise = (False, ts)
    where
        (qf, rf) = f ts

-- Choice
infixl 4 <>
(<>) :: Eq t => Parser t -> Parser t -> Parser t
(f <> g) ts
    | b         = fts
    | otherwise = g ts
    where
        fts@(b,_) = f ts
