-- Haskell Practical 3 Code - N1 Parser
--  By James Cowgill

import Prac2Alex
import Prac3LLParser

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
    and     right
    or      right
    ;       left

Disambiguated
    e   = e + e1
        | e - e1
        | e1
    e1  = e1 * e2
        | e2
    e2  = N
        | V
        | (e)

    b   = b1 or b
        | b1
    b1  = b2 and b1
        | b2
    b2  = not b2
        | e = e
        | e < e
        | e > e
        | true
        | false
        | (b)

Remove left recursion
    e   = e1 e'
    e'  = + e
        | - e
        | λ
    e1  = e2 e1'
    e1' = * e1
        | λ
    e2  = N
        | V
        | (e)

    b   = b1 b'
    b'  = or b
        | λ
    b1  = b2 b1'
    b1' = and b1
        | λ
    b2  = e b2Op e
        | not b2
        | true
        | false
        | (b)
    b2Op= = | < | >

    s   = s1 s'
    s'  = ; s
        | λ
    s1  = skip
        | v := e
        | if b then s1 else s1
        | while b s1
        | (s)
-}

data NonTerminal = E | E' | E1 | E1' | E2 |
                    B | B' | B1 | B1' | B2 | B2Op |
                    S | S' | S1 deriving (Show)

-- Main parse table
parseTable :: NonTerminal -> Maybe Token -> [Symbol NonTerminal Token]
parseTable E    _                   = [SymNont E1, SymNont E']
parseTable E'   (Just TokPlus)      = [SymTerm TokPlus, SymNont E]
parseTable E'   (Just TokMinus)     = [SymTerm TokMinus, SymNont E]
parseTable E'   _                   = []
parseTable E1   _                   = [SymNont E2, SymNont E1']
parseTable E1'  (Just TokTimes)     = [SymTerm TokTimes, SymNont E1]
parseTable E1'  _                   = []
parseTable E2   (Just (TokNum n))   = [SymTerm (TokNum n)]
parseTable E2   (Just (TokVar v))   = [SymTerm (TokVar v)]
parseTable E2   (Just TokLeft)      = [SymTerm TokLeft, SymNont E, SymTerm TokRight]
parseTable E2   _                   = [SymError]

parseTable B    _                   = [SymNont B1, SymNont B']
parseTable B'   (Just TokOr)        = [SymTerm TokOr, SymNont B]
parseTable B'   _                   = []
parseTable B1   _                   = [SymNont B2, SymNont B1']
parseTable B1'  (Just TokAnd)       = [SymTerm TokAnd, SymNont B1]
parseTable B1'  _                   = []
parseTable B2   (Just TokLeft)      = [SymTerm TokLeft, SymNont B, SymTerm TokRight]
parseTable B2   (Just TokNot)       = [SymTerm TokNot, SymNont B2]
parseTable B2   (Just (TokBool b))  = [SymTerm (TokBool b)]
parseTable B2   _                   = [SymNont E, SymNont B2Op, SymNont E]
parseTable B2Op (Just TokEquals)    = [SymTerm TokEquals]
parseTable B2Op (Just TokLess)      = [SymTerm TokLess]
parseTable B2Op (Just TokGreater)   = [SymTerm TokGreater]
parseTable B2Op _                   = [SymError]

parseTable S    _                   = [SymNont S1, SymNont S']
parseTable S'   (Just TokSeq)       = [SymTerm TokSeq, SymNont S]
parseTable S'   _                   = []
parseTable S1   (Just TokSkip)      = [SymTerm TokSkip]
parseTable S1   (Just (TokVar v))   = [SymTerm (TokVar v), SymTerm TokAssign, SymNont E]
parseTable S1   (Just TokIf)        = [SymTerm TokIf, SymNont B, SymTerm TokThen, SymNont S1, SymTerm TokElse, SymNont S1]
parseTable S1   (Just TokWhile)     = [SymTerm TokWhile, SymNont B, SymNont S1]
parseTable S1   (Just TokLeft)      = [SymTerm TokLeft, SymNont S, SymTerm TokRight]
parseTable S1   _                   = [SymError]

-- Returns true if an N1 program is syntactically correct
isN1String :: String -> Bool
isN1String str = llparse parseTable S (alexScanTokens str)
