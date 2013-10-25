#lang eopl

; Lexical specification
(define lexer-spec '(
  ; Skip all whitespace
  (white (whitespace) skip)

  ; Integers
  (number (digit (arbno digit)) number)
))

; Grammar specification
(define grammar-spec '(
  ; Primitive number expression
  (expression (number) expr-number)

  ; Subtraction
  (expression ("-" "(" expression "," expression ")") expr-sub)

  ; Zero test
  (expression ("zero?" "(" expression ")") expr-zero)

  ; Conditional
  (expression ("if" expression "then" expression "else" expression) expr-if)
))

; Define datatypes from the grammar
(sllgen:make-define-datatypes lexer-spec grammar-spec)

; Create parsers
(define core-scan  (sllgen:make-string-scanner lexer-spec grammar-spec))
(define core-parse (sllgen:make-string-parser  lexer-spec grammar-spec))