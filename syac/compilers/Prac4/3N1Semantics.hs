-- Haskell Practical 4 Code - Semantics for N1
--  By James Cowgill

import Prac4.DomFunc

-- N1 Abstract Syntax Tree
data E  = Plus E E
        | Minus E E
        | Times E E
        | Number Int
        | Variable String

data B  = Not B
        | And B B
        | Or B B
        | Equal E E
        | Less E E
        | Greater E E
        | BTrue
        | BFalse

data S  = Skip
        | Print E
        | Seq S S
        | Assign String E
        | If B S S
        | While B S

-- Evaluation
type Env = DomFunc String Int
type Output = [Int]

-- Evaluate numeric expression
evalExpr :: Env -> E -> Int
evalExpr env (Plus e1 e2)   = (evalExpr env e1) + (evalExpr env e2)
evalExpr env (Minus e1 e2)  = (evalExpr env e1) - (evalExpr env e2)
evalExpr env (Times e1 e2)  = (evalExpr env e1) * (evalExpr env e2)
evalExpr _   (Number n)     = n
evalExpr env (Variable v)   = fetch v env

-- Evaluate boolean expression
evalBExpr :: Env -> B -> Bool
evalBExpr env (Not b)           = not (evalBExpr env b)
evalBExpr env (And b1 b2)       = (evalBExpr env b1) && (evalBExpr env b2)
evalBExpr env (Or b1 b2)        = (evalBExpr env b1) || (evalBExpr env b2)
evalBExpr env (Equal e1 e2)     = (evalExpr env e1) == (evalExpr env e2)
evalBExpr env (Less e1 e2)      = (evalExpr env e1) < (evalExpr env e2)
evalBExpr env (Greater e1 e2)   = (evalExpr env e1) > (evalExpr env e2)
evalBExpr _   BTrue             = True
evalBExpr _   BFalse            = False

-- Evaluate statement
evalStatement :: Env -> S -> (Env, Output)
evalStatement env Skip          = (env, [])
evalStatement env (Print e)     = (env, [evalExpr env e])
evalStatement env (Seq s1 s2)   = (rEnv, lOut ++ rOut)
    where
        (lEnv, lOut) = evalStatement env s1
        (rEnv, rOut) = evalStatement lEnv s2
evalStatement env (Assign v e)  = (update v (evalExpr env e) env, [])
evalStatement env (If b s1 s2)
    | evalBExpr env b           = evalStatement env s1
    | otherwise                 = evalStatement env s2
evalStatement env w@(While b s) = evalStatement env (If b (Seq s w) Skip)

-- Run program with blank initial environment
runProgram :: S -> Output
runProgram s = snd (evalStatement empty s)

-- Test program
--  Calculates smallest power of 2 greater than 10000
n1TestProgram :: S
n1TestProgram = (Seq (Seq
                    (Assign "x" (Number 1))
                    (While (Less (Variable "x") (Number 10000))
                        (Assign "x" (Times (Variable "x") (Number 2)))))
                    (Print (Variable "x")))
