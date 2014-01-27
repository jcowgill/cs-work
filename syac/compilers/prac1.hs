-- Haskell Practical 1 Code
--  James Cowgill

import Data.Char

-- Convert string to integer
s2n :: String -> Int
s2n = (foldl1 f).(map digitToInt)
    where f a b = a * 10 + b

-- Drop leading whitespace from a string
dropLeadingWhitespace :: String -> String
dropLeadingWhitespace = dropWhile isSpace

-- Expression trees
type V = String
type N = Int

data E = Var V
        | Val N
        | Plus E E
        | Mult E E

instance Show E where
    show (Var s)    = s
    show (Val n)    = show n
    show (Plus a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

-- Evaluate expression tree
evaluate :: E -> Int
evaluate (Var a)    = error ("Unknown variable `" ++ a ++ "`")
evaluate (Val n)    = n
evaluate (Plus a b) = evaluate a + evaluate b
evaluate (Mult a b) = evaluate a * evaluate b

-- Simplify expressions (very basic)
--  reduce will apply the simplifying rules without recursing
reduce :: E -> E
reduce (Plus e (Val 0)) = e
reduce (Plus (Val 0) e) = e
reduce (Mult e (Val 1)) = e
reduce (Mult (Val 1) e) = e
reduce e                = e

--  simplify iterates over the tree calling reduce
simplify :: E -> E
simplify (Mult a b) = reduce (Mult (simplify a) (simplify b))
simplify (Plus a b) = reduce (Plus (simplify a) (simplify b))
simplify e          = reduce e

-- Association lists (maps)
type Assoc t = [(String, t)]

-- Return list of names in the list
names :: Assoc t -> [String]
names = map fst

-- Return true if first argument is in the list
inAssoc :: String -> Assoc a -> Bool
inAssoc n l = elem n (names l)

-- Fetch item from list or raise an error if it doesn't exist
fetch :: String -> Assoc a -> a
fetch n [] = error ("key `" ++ n ++ "` does not exist")
fetch n (x:xs) | n == fst x = snd x
               | otherwise  = fetch n xs

-- Updates / adds an item in the list
update :: String -> a -> Assoc a -> Assoc a
update k v [] = [(k, v)]
update k v (x:xs) | k == fst x = (k, v):xs
                  | otherwise  = x:update k v xs

-- Evaluation trees reprise
evaluate2 :: Assoc Int -> E -> Int
evaluate2 v (Var a)    = fetch a v
evaluate2 _ (Val n)    = n
evaluate2 v (Plus a b) = evaluate2 v a + evaluate2 v b
evaluate2 v (Mult a b) = evaluate2 v a * evaluate2 v b

-- Alternate representation
data Op = Add | Mul
        deriving Show
data ET = Vr V | Vl N | Ap ET Op ET
        deriving Show

-- Get function which evaluates an operator
opFunc :: Op -> (Int -> Int -> Int)
opFunc Add = (+)
opFunc Mul = (*)

-- Main evaluate function
evaluate3 :: Assoc Int -> ET -> Int
evaluate3 v (Vr a)      = fetch a v
evaluate3 _ (Vl n)      = n
evaluate3 v (Ap a op b) = (opFunc op) (evaluate3 v a) (evaluate3 v b)

-- Conversion between representations
treeToNew :: E -> ET
treeToNew (Var v)     = Vr v
treeToNew (Val n)     = Vl n
treeToNew (Plus a b)  = Ap (treeToNew a) Add (treeToNew b)
treeToNew (Mult a b)  = Ap (treeToNew a) Mul (treeToNew b)

treeToOld :: ET -> E
treeToOld (Vr v) = Var v
treeToOld (Vl n) = Val n
treeToOld (Ap a Add b) = Plus (treeToOld a) (treeToOld b)
treeToOld (Ap a Mul b) = Mult (treeToOld a) (treeToOld b)

-- Maybe types
headT :: [a] -> Maybe a
headT [] = Nothing
headT xs = Just (head xs)

tailT :: [a] -> Maybe [a]
tailT [] = Nothing
tailT xs = Just (tail xs)

lastT :: [a] -> Maybe a
lastT [] = Nothing
lastT xs = Just (last xs)
