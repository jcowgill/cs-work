-- Haskell Practical 3 Code - Generic LL(1) Parser
--  By James Cowgill

module Prac3LLParser where

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
