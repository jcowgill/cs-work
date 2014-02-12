-- Haskell Practical 3 Code - Predictive Parser
--  By James Cowgill

import Prac3Lexer

-- Generic LL(1) Parser
--  Given a parsing table, starting symbol, list of tokens
--  Outputs true iff the list of tokens is in the language
data Symbol n t = SymNont n | SymTerm t | SymError
type Table n t = n -> Maybe t -> [Symbol n t]

llparse :: Eq t => Table n t -> n -> [t] -> Bool
llparse tb start = parseHelper [SymNont start]
 where
  parseHelper []             []     = True
  parseHelper (SymNont n:ss) []     = parseHelper ((tb n Nothing) ++ ss) []
  parseHelper (SymNont n:ss) (c:cs) = parseHelper ((tb n (Just c)) ++ ss) (c:cs)
  parseHelper (SymTerm t:ss) (c:cs) | t == c = parseHelper ss cs
  parseHelper _              _      = False

{-
First and follow sets
 first p   = [-, T, F, V, (]
 first p'  = [=, λ]
 first p1  = [-, T, F, V, (]
 first p1' = [>, λ]
 first p2  = [-, T, F, V, (]
 first p2' = [+, λ]
 first p3  = [-, T, F, V, (]
 first p3' = [*, λ]
 first p4  = [-, T, F, V, (]
 first p5  = [T, F, V, (]

 follow p   = [), $]
 follow p'  = [), $]
 follow p1  = [=, ), $]
 follow p1' = [=, ), $]
 follow p2  = [>, =, ), $]
 follow p2' = [>, =, ), $]
 follow p3  = [+, >, =, ), $]
 follow p3' = [+, >, =, ), $]
 follow p4  = [*, +, >, =, ), $]
 follow p5  = [*, +, >, =, ), $]
-}

data NonTerminal = P | P' | P1 | P1' | P2 | P2' | P3 | P3' | P4 | P5

-- Tests if a token is a "value" (true, false, variable)
isValue :: Token -> Bool
isValue TokTrue     = True
isValue TokFalse    = True
isValue (TokVar _)  = True
isValue _           = False

-- Main parse table
--  I cheated a bit here (and completely ignored the above tables :)
--  This made it much easier and I think it has the same result
parseTable :: NonTerminal -> Maybe Token -> [Symbol NonTerminal Token]
parseTable P   _                 = [SymNont P1, SymNont P']
parseTable P'  (Just TokEqual)   = [SymTerm TokEqual, SymNont P1, SymNont P']
parseTable P'  _                 = []
parseTable P1  _                 = [SymNont P2, SymNont P1']
parseTable P1' (Just TokImplies) = [SymTerm TokImplies, SymNont P1]
parseTable P1' _                 = []
parseTable P2  _                 = [SymNont P3, SymNont P2']
parseTable P2' (Just TokOr)      = [SymTerm TokOr, SymNont P3, SymNont P2']
parseTable P2' _                 = []
parseTable P3  _                 = [SymNont P4, SymNont P3']
parseTable P3' (Just TokAnd)     = [SymTerm TokAnd, SymNont P4, SymNont P3']
parseTable P3' _                 = []
parseTable P4  (Just TokNegate)  = [SymTerm TokNegate, SymNont P4]
parseTable P4  _                 = [SymNont P5]
parseTable P5  (Just t)          | isValue t = [SymTerm t]
parseTable P5  (Just TokLeft)    = [SymTerm TokLeft, SymNont P, SymTerm TokRight]
parseTable _   _                 = [SymError]

-- Determines if a given string is a valid prepositonal calculus string
--  (still raises exception on lexical error)
isCalculusString :: String -> Bool
isCalculusString str = llparse parseTable P (lexer str)
