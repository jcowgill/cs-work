-- Haskell Practical 4 Code - Naïve type checking
--  By James Cowgill

import Data.List

-- Type checking for AST
--  OPlus A B   = A
--  OTimes B A  = B
data C = OPlus C C | OTimes C C | Var String
    deriving (Eq, Show)

-- Result of asserts (Type A vars, Type B vars, Error vars)
--  All lists are disjoint
type AssertResult = ([String], [String], [String])

-- Merge results of two assertions
mergeResults :: AssertResult -> AssertResult -> AssertResult
mergeResults (lTypeA, lTypeB, lBad) (rTypeA, rTypeB, rBad) =
    (finalTypeA, finalTypeB, finalBad)
    where
        newBad      = (lTypeA `intersect` rTypeB) `union`
                      (lTypeB `intersect` rTypeA)
        finalBad    = lBad `union` rBad `union` newBad
        finalTypeA  = (lTypeA `union` rTypeA) \\ finalBad
        finalTypeB  = (lTypeB `union` rTypeB) \\ finalBad

-- Asserts that the given expression is of a type
assertTypeA :: C -> AssertResult
assertTypeA (Var v)         = ([v], [], [])
assertTypeA (OTimes _ _)    = ([], [], ["*"])
assertTypeA (OPlus l r)     = mergeResults (assertTypeA l) (assertTypeB r)

assertTypeB :: C -> AssertResult
assertTypeB (Var v)         = ([], [v], [])
assertTypeB (OPlus _ _)     = ([], [], ["+"])
assertTypeB (OTimes l r)    = mergeResults (assertTypeB l) (assertTypeA r)

-- Initial entry point
typeCheck :: C -> AssertResult
typeCheck c@(Var _)         = assertTypeA c
typeCheck c@(OPlus _ _)     = assertTypeA c
typeCheck c@(OTimes _ _)    = assertTypeB c

-- Is an expression type correct?
isTypeCorrect :: C -> Bool
isTypeCorrect c = null bad
    where
        (_, _, bad) = typeCheck c
