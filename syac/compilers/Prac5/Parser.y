{
-- Haskell Practical 5 - Drawing Code Parser
--  By James Cowgill

module Prac5.Parser(Picture, PicCommand(..), doParse) where

import Prac5.Lexer
}

%name doParse
%error { parseError }
%tokentype { Token }
%token
    hor { TokHor }
    ver { TokVer }
    ',' { TokComma }
    '+' { TokPlus }
    num { TokNum $$ }

%%

-- This fixes the order of the list of commands
--  (since Picture returns a reversed list)
PictureRev : Picture            { reverse $1 }

Picture : Picture '+' Command   { $3 : $1 }
        | Picture '+'           { $1 }
        | Command               { [$1] }
        | {- empty -}           { [] }

Command : hor num ',' num ',' num   { CmdHor ($2, $4) $6 }
        | ver num ',' num ',' num   { CmdVer ($2, $4) $6 }

{
-- Picture and Commands
type Picture = [PicCommand]

data PicCommand = CmdHor (Int, Int) Int
                | CmdVer (Int, Int) Int
                deriving (Eq, Show)

-- Error function
parseError :: [Token] -> a
parseError _ = error "parse error"
}
