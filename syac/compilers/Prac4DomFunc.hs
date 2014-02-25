-- Haskell Practical 4 Code - Domain Functions
--  By James Cowgill

module Prac4DomFunc where

-- Domain function lists
--  Contain a list with the domain and a function to map the domain to values
type DomFunc n v = ([n], n -> v)

-- Return list of names in the list
names :: DomFunc n v -> [n]
names = fst

-- Return true if first argument is in the list
inAssoc :: Eq n => n -> DomFunc n v -> Bool
inAssoc val df = elem val (names df)

-- Fetch item from list or raise an error if it doesn't exist
fetch :: (Eq n, Show n) => n -> DomFunc n v -> v
fetch n df@(d, f)   | inAssoc n df = f n
                    | otherwise = error ("key `" ++ (show n) ++ "` does not exist")

-- Updates / adds an item in the list
update :: Eq n => n -> v -> DomFunc n v -> DomFunc n v
update k v df@(d, f)    = (newD, newF)
    where
        newD    | inAssoc k df  = d
                | otherwise     = [k] ++ d
        newF n  | n == k    = v
                | otherwise = f n

-- Conversion to association lists
toAssocList :: DomFunc n v -> [(n, v)]
toAssocList (d, f) = [(n, f n) | n <- d]
