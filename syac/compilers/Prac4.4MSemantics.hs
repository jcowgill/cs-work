-- Haskell Practical 4 Code - Semantics for M
--  By James Cowgill

import Data.Char;
import Data.List;
import Data.Maybe;
import Text.Printf;

-- M Abstract Syntax Tree
type Reg = Int
type Label = String

data Op = Add | Sub | Mul
    deriving (Eq, Show)
data CmpOp = Eq | NEq | Lt | Gt | LtEq | GtEq
    deriving (Eq, Show)

data Instruction =
    Hlt |                       -- Halt execution
    Prn  Reg |                  -- Print contents of register
    Lim  Reg Int |              -- Load register with value
    Mov  Reg Reg |              -- Load register with another register
    Opn  Reg Reg Op Reg |       -- Register operation
    Opni Reg Reg Op Int |       -- Immediate register operation
    Jmp  Label |                -- Unconditional jump
    Br   Reg CmpOp Reg Label |  -- Conditional jump
    MarkLabel Label             -- Mark label
    deriving (Eq, Show)

type Code = [Instruction]

-- State of execution
--  Program counter (Nothing = Halted)
--  Registers + their values
--  Output so far
type RegStore = [Int]
type Output = [Int]
type State = (Maybe Int, RegStore, Output)

-- Returns the value of a register
getReg :: RegStore -> Reg -> Int
getReg rs r | r < length rs = rs!!r
            | otherwise     = 0

-- Updates the list of registers with a new value
setReg :: RegStore -> Reg -> Int -> RegStore
setReg rs r i | r < len   = left ++ [i] ++ (tail right)
              | otherwise = rs ++ (replicate (r - length rs) 0) ++ [i]
  where
    len = length rs
    (left, right) = splitAt r rs

-- Evaluates an operator
evalOp :: Op -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)

-- Evaluates a comparison operator
evalCmpOp :: CmpOp -> Int -> Int -> Bool
evalCmpOp Eq    = (==)
evalCmpOp NEq   = (/=)
evalCmpOp Lt    = (<)
evalCmpOp Gt    = (>)
evalCmpOp LtEq  = (<=)
evalCmpOp GtEq  = (>=)

-- Evaluates a register command
--  Returns new register store
evalRegCmd :: RegStore -> Instruction -> RegStore
evalRegCmd rs instr = setReg rs left right
  where
    (left, right) = regCmd instr
    regCmd (Lim r i)            = (r, i)
    regCmd (Mov rD rS)          = (rD, getReg rs rS)
    regCmd (Opn rD rS1 op rS2)  = (rD, evalOp op (getReg rs rS1) (getReg rs rS2))
    regCmd (Opni rD rS1 op iS2) = (rD, evalOp op (getReg rs rS1) iS2)

-- Finds the program counter value for a label
findLabel :: Code -> Label -> Maybe Int
findLabel c l | isJust index = index
  where
    index = elemIndex (MarkLabel l) c

-- Wrapper for evalInstruction to handle extracting from code + state
oneStep :: State -> Code -> State
oneStep s@(Nothing, _, _) _  = s
oneStep (Just pc, rs, out) c = cmd (c!!pc)
  where
    nextPc              = Just (pc + 1)
    cmd (Hlt)           = (Nothing, rs, out)
    cmd (Prn r)         = (nextPc, rs, out ++ [getReg rs r])
    cmd (Jmp l)         = (findLabel c l, rs, out)
    cmd b@(Br _ _ _ _)  = (brDest b, rs, out)
    cmd (MarkLabel _)   = (nextPc, rs, out)
    cmd instr           = (nextPc, evalRegCmd rs instr, out)

    brDest (Br r1 op r2 l)
        | evalCmpOp op (getReg rs r1) (getReg rs r2) = findLabel c l
        | otherwise                                  = nextPc

-- Evaluates a block of code using a state as input
evalWithState :: State -> Code -> Output
evalWithState s c | isNothing pc = out
                  | otherwise    = evalWithState newS c
  where
    newS@(pc, rs, out) = oneStep s c

-- Evaluates a block of code
eval :: Code -> Output
eval = evalWithState (Just 0, [], [])

-- Pretty prints a block of code
showCode :: Code -> String
showCode [] = ""
showCode (c:cs) = showCmd c ++ "\n" ++ showCode cs
  where
    showCmd (Hlt)               = "  hlt"
    showCmd (Prn r)             = printf "  prn r%d" r
    showCmd (Lim r i)           = printf "  lim r%d, %d" r i
    showCmd (Mov r1 r2)         = printf "  mov r%d, r%d" r1 r2
    showCmd (Opn r1 r2 op r3)   = printf "  %s r%d, r%d, r%d" (showOp op) r1 r2 r3
    showCmd (Opni r1 r2 op r3)  = printf "  %si r%d, r%d, r%d" (showOp op) r1 r2 r3
    showCmd (Jmp l)             = printf "  jmp %s" l
    showCmd (Br r1 op r2 l)     = printf "  %s r%d, r%d, %s" (showCmpOp op) r1 r2 l
    showCmd (MarkLabel l)       = l ++ ":"
    showOp op       = map toLower (show op)
    showCmpOp Eq    = "beq"
    showCmpOp NEq   = "bneq"
    showCmpOp Lt    = "blt"
    showCmpOp Gt    = "bgt"
    showCmpOp LtEq  = "bgeq"
    showCmpOp GtEq  = "bleq"

-- Prints block of code to stdout
putStrCode :: Code -> IO ()
putStrCode c = putStr (showCode c)

-- Test program
--  Calculates smallest power of 2 greater than 10000
mTestProgram :: Code
mTestProgram =
    [Lim 0 1,
     Lim 1 10000,
     MarkLabel "top",
     Br 0 GtEq 1 "end",
     Opni 0 0 Mul 2,
     Jmp "top",
     MarkLabel "end",
     Prn 0,
     Hlt]
