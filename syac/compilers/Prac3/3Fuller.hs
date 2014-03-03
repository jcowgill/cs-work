-- Haskell Practical 3 Code - Parsing Fuller
--  By James Cowgill

import Prac3.Combinators
import Prac3.Lexer

{-
(all symbols and uppercase letters are terminals)

Original Grammar
    p = T | F | V | -p | p*p | p+p | p>p | p=p | (p)

    Precendence (decending order)

    Op | Associativity
    ---+--------------
     - | Right
     * | Left
     + | Left
     > | Right
     = | Left

Disambiguated
    p   = p = p1
        | p1
    p1  = p2 > p1
        | p2
    p2  = p2 + p3
        | p3
    p3  = p3 * p4
        | p4
    p4  = -p4
        | p5
    p5  = T
        | F
        | V
        | (p)

Left factored
    p1  = p2 p1'
    p1' = > p1
        | λ

Remove left-recursion
    p   = p1 p'
    p'  = = p1 p'
        | λ
    p1  = p2 p1'
    p1' = > p1
        | λ
    p2  = p3 p2'
    p2' = + p3 p2'
        | λ
    p3  = p4 p3'
    p3' = * p4 p3'
        | λ
    p4  = -p4
        | p5
    p5  = T
        | F
        | V
        | (p)

Tokens are:
    T F V ( ) = > + * -
    All singles except for V
-}

-- Main parser rules
parserP :: Parser Token
parserP   =  parserP1 +> parserP'
parserP'  =  terminal TokEqual +> parserP1 +> parserP'
          <> empty
parserP1  =  parserP2 +> parserP1'
parserP1' =  terminal TokImplies +> parserP1
          <> empty
parserP2  =  parserP3 +> parserP2'
parserP2' =  terminal TokOr +> parserP3 +> parserP2'
          <> empty
parserP3  =  parserP4 +> parserP3'
parserP3' =  terminal TokAnd +> parserP4 +> parserP3'
          <> empty
parserP4  =  terminal TokNegate +> parserP4
          <> parserP5
parserP5  =  terminal TokTrue
          <> terminal TokFalse
          <> parserV
          <> terminal TokLeft +> parserP +> terminal TokRight

-- Variable names parser
parserV :: Parser Token
parserV (TokVar _:xs) = (True, xs)
parserV xs            = (False, xs)

-- Tests for valid prepositional calculus strings
parser :: [Token] -> Bool
parser xs = rFst && (null rSnd)
    where
        (rFst, rSnd) = parserP xs
