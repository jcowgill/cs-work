 {
-- Haskell Practical 4 Happy Code - Unlabelled binary trees
--  By James Cowgill

module Prac4.ParserUBinTree(UBToken(..), UBTree(..), doParse) where
}

%name doParse
%error { parseError }
%tokentype { UBToken }
%token
    l   { Leaf }
    '(' { Lt }
    ')' { Rt }
    ',' { Comma }

%%

Tree : '(' Tree ',' Tree ')'    { TreeNode $2 $4 }
     | l                        { TreeLeaf }

{
-- Token type
data UBToken = Lt | Rt | Leaf | Comma
    deriving(Eq, Show)

-- Tree type
data UBTree = TreeLeaf | TreeNode UBTree UBTree
    deriving(Eq, Show)

-- Error function
parseError :: [UBToken] -> a
parseError _ = error "parse error"
}
