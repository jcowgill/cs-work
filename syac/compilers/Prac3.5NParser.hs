-- Haskell Practical 3 Code - N1 Parser
--  By James Cowgill

-- The practicals say to use the combinators OR the llparser
--  but I'm not a fan of either so I'm just doing a handwritten
--  recursive decent parser

import Prac2Alex

{-
Original Grammar
    e   = e + e
        | e - e
        | e * e
        | N
        | V
        | (e)

    b   = not b     (logical)
        | b and b   (logical)
        | b or b    (logical)
        | e = e
        | e < e
        | e > e
        | (b)

    s   = skip
        | s; s
        | v := e
        | if b then s else s
        | while b s
        | (s)

Precedence + Associativity
    not     right
    *       left
    + -     left
    < >     left
    =       left
    and     left
    or      left

Disambiguated
    e   = e + e1
        | e - e1
        | e1
    e1  = e1 * e2
        | e2
    e2  = N
        | V
        | (e)

    b   = b or b1
        | b1
    b1  = b1 and b2
        | b2
    b2  = not b2
        | b3
    b3  = e = e
        | e < e
        | e > e
        | (b)

Left factored
    e   = e eOp e1
        | e1
    eOp = + | -

    b3  = e b3Op e
        | (b)
    b3Op= = | < | >

Remove left recursion
    e   = e1 e'
    e'  = + e1 e'
        | - e1 e'
        | λ
    e1  = e2 e1'
    e1' = * e2 e1'
        | λ
    e2  = N
        | V
        | (e)

    b   = b1 b'
    b'  = or b1 b'
        | λ
    b1  = b2 b1'
    b1' = and b2 b1'
        | λ
    b2  = not b2
        | b3
    b3  = e b3Op e
        | (b)
    b3Op= = | < | >

    s   = s1 s'
    s'  = ; s1 s'
        | λ
    s1  = skip
        | v := e
        | if b then s else s
        | while b s
        | (s)
-}

data NonTerminal = E | E' | E1 | E1' | E2 |
                    B | B' | B1 | B1' | B2 | B3 | B3Op |
                    S | S' | S1

-- Main parse table
parseTable :: NonTerminal -> Maybe Token -> [Symbol NonTerminal Token]
parseTable E    _                   = [Nont E1, Nont E']
parseTable E'   (Just TokSym '+')   = [Term '+', Nont E1, Nont E']
parseTable E'   (Just TokSym '-')   = [Term '-', Nont E1, Nont E']
parseTable E'   _                   = []
parseTable E1   _                   = [Nont E2, Nont E1']
parseTable E1'  (Just TokSym '*')   = [Term '*', Nont E2, Nont E1']
parseTable E1'  _                   = []

{-

Remove left recursion
    e   = e1 e'
    e'  = + e1 e'
        | - e1 e'
        | λ
    e1  = e2 e1'
    e1' = * e2 e1'
        | λ
    e2  = N
        | V
        | (e)

    b   = b1 b'
    b'  = or b1 b'
        | λ
    b1  = b2 b1'
    b1' = and b2 b1'
        | λ
    b2  = not b2
        | b3
    b3  = e b3Op e
        | (b)
    b3Op= = | < | >

    s   = s1 s'
    s'  = ; s1 s'
        | λ
    s1  = skip
        | v := e
        | if b then s else s
        | while b s
        | (s)
-}
