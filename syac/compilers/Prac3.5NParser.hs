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
        | not e     (logical)
        | e and e   (logical)
        | e or e    (logical)
        | e = e
        | e < e
        | true
        | false
        | N
        | V
        | (e)

    s   = skip
        | s; s
        | v := e
        | if e then s else s
        | while e s
        | (s)

Precedence + Associativity
    not     right
    *       left
    + -     left
    <       left
    =       left
    and     left
-}
