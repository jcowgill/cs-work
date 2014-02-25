-- Haskell Practical 4 Code - Semantics for M
--  By James Cowgill

-- M Abstract Syntax Tree
type Reg = Int
type Label = Int

data Op = Add | Sub | Mul
data CmpOp = Eq | Neq | Lt | Gr | LtEq | GrEq

data Instruction =
    Hlt |                       -- Halt execution
    Prn  Reg                    -- Print contents of register
    Lim  Reg Int |              -- Load register with value
    Mov  Reg Reg |              -- Load register with another register
    Opn  Reg Reg Op Reg |       -- Register operation
    Opni Reg Reg Op Int |       -- Immediate register operation
    Jmp  Label |                -- Unconditional jump
    Br   Reg CmpOp Reg Label |  -- Conditional jump
    MarkLabel Label             -- Mark label

type Code = [Instruction]

-- State of execution
--  Program counter (Nothing = Halted)
--  Registers + their values
type State = (Maybe Int, [Int])
type Output = [Int]

-- Returns the value of a register
getReg :: Int -> [Int] -> Int
getReg r rs | length rs < r = rs!!r
            | otherwise     = 0

-- Wrapper for evalInstruction to handle extracting from code + state
oneStep :: Code -> State -> (State, Output)
oneStep _ s@(Nothing, _) = (s, [])
oneStep c (Just p, rs)   = cmd c!!p
  where
    cmd (Hlt)       = ((Nothing, rs), [])
    cmd (Prn r)     = (
