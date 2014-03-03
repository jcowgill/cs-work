{
-- Haskell Practical 4 Happy Code - N1 Parser
--  By James Cowgill

module Prac4.ParserN1(AstStmt(..), AstExpr(..), AstBExpr(..), doParse) where

import Prac2.Alex
}

%name doParse
%error { parseError }
%tokentype { Token }
%token
    skip    { TokSkip }
    print   { TokPrint }
    if      { TokIf }
    then    { TokThen }
    else    { TokElse }
    while   { TokWhile }
    or      { TokOr }
    and     { TokAnd }
    not     { TokNot }
    '='     { TokEquals }
    '<'     { TokLess }
    '>'     { TokGreater }
    '+'     { TokPlus }
    '-'     { TokMinus }
    '*'     { TokTimes }
    ';'     { TokSeq }
    '('     { TokLeft }
    ')'     { TokRight }
    ':='    { TokAssign }
    bool    { TokBool $$ }
    var     { TokVar $$ }
    num     { TokNum $$ }

-- Operator precedence
%left  ';'
%right or
%right and
%left  '='
%left  '<' '>'
%left  '+' '-'
%left  '*'
%right not

%%

Stmt  : Stmt ';' Stmt   { AstSeq $1 $3 }
      | Stmt2           { $1 }

Stmt2 : if BExpr then Stmt2 else Stmt2  { AstIf $2 $4 $6 }
      | while BExpr Stmt2               { AstWhile $2 $3 }
      | var ':=' Expr                   { AstAssign $1 $3 }
      | skip                            { AstSkip }
      | print Expr                      { AstPrint $2 }
      | '(' Stmt ')'                    { $2 }

BExpr : not BExpr       { AstNot    $2 }
      | BExpr and BExpr { AstAnd    $1 $3 }
      | BExpr or BExpr  { AstOr     $1 $3 }
      | Expr '=' Expr   { AstEqual  $1 $3 }
      | Expr '<' Expr   { AstLess   $1 $3 }
      | Expr '>' Expr   { AstGreater $1 $3 }
      | bool            { AstBool   $1 }
      | '(' BExpr ')'   { $2 }

Expr  : Expr '+' Expr   { AstPlus   $1 $3 }
      | Expr '-' Expr   { AstMinus  $1 $3 }
      | Expr '*' Expr   { AstTimes  $1 $3 }
      | num             { AstNumber $1 }
      | var             { AstVariable $1 }
      | '(' Expr ')'    { $2 }

{
-- AST Type
data AstExpr =
    AstPlus     AstExpr AstExpr |
    AstMinus    AstExpr AstExpr |
    AstTimes    AstExpr AstExpr |
    AstNumber   Int |
    AstVariable String
    deriving(Eq, Show)

data AstBExpr =
    AstNot      AstBExpr |
    AstAnd      AstBExpr AstBExpr |
    AstOr       AstBExpr AstBExpr |
    AstEqual    AstExpr AstExpr |
    AstLess     AstExpr AstExpr |
    AstGreater  AstExpr AstExpr |
    AstBool     Bool
    deriving(Eq, Show)

data AstStmt =
    AstPrint    AstExpr |
    AstSeq      AstStmt AstStmt |
    AstAssign   String AstExpr |
    AstIf       AstBExpr AstStmt AstStmt |
    AstWhile    AstBExpr AstStmt |
    AstSkip
    deriving(Eq, Show)

-- Error function
parseError :: [Token] -> a
parseError _ = error "parse error"
}
