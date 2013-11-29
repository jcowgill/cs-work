#lang racket

(require eopl)

(provide lang-name lang-scan lang-parse lang-eval-expr)

; The language name
(define lang-name "PROC")

; Initialize primitive functions
(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

; Definitions for extra primitive functions
(define less? <)
(define greater? >)
(define (minus x) (- 0 x))
(define (print val) (begin (display val) val))

; Lexical specification
(define lexer-spec '(
  ; Skip all whitespace
  (white (whitespace) skip)

  ; Integers
  (number (digit (arbno digit)) number)

  ; Identifiers
  (identifier (letter (arbno (or letter digit "-" "_"))) symbol)

  ; Primitives
  ;  I admit this is a hack, but it's needed since the language grammar treats
  ;  primitives and other functions fundamentally differently
  ;  (unlike scheme for example).
  ;  All these must be defined as functions somewhere in this file (or imported)
  (primitive ((or
      "zero?" "minus" "+" "-" "*" "/" "equal?" "greater?" "less?"
      "cons" "car" "cdr" "null?" "print"
    )) string)
))

; Grammar specification
(define grammar-spec '(
  ; Primitive numbers and identifiers
  (expression (number) expr-number)
  (expression (identifier) expr-ident)

  ; List definition
  (expression ("[" (separated-list expression ",") "]") expr-list)

  ; Primitive function call
  (expression (primitive "(" (separated-list expression ",") ")") expr-primitive)

  ; Procedures and procedure calls
  (expression ("proc" "(" (arbno identifier) ")" expression) expr-proc)
  (expression ("(" expression (arbno expression) ")") expr-call)

  ; Let expressions
  (expression ("let"  (arbno identifier "=" expression) "in" expression) expr-let)
  (expression ("let*" (arbno identifier "=" expression) "in" expression) expr-let*)

  ; Conditional
  (expression ("if" expression "then" expression "else" expression) expr-if)

  ; Multi-way conditional
  (expression ("cond" (arbno expression "==>" expression) "end") expr-cond)
))

; Define datatypes from the grammar
(sllgen:make-define-datatypes lexer-spec grammar-spec)

; Create parsers
(define lang-scan  (sllgen:make-string-scanner lexer-spec grammar-spec))
(define lang-parse (sllgen:make-string-parser  lexer-spec grammar-spec))

; Evaluates a list with the given bindings
(define (eval-expr-list exprs bindings)
  (map
   (lambda (expr) (eval-expr expr bindings))
   exprs
  )
)

; Evaluate primitive expression
(define (eval-primitive ident exprs bindings)
  ; Get primitive function
  (let ((func (eval (string->symbol ident) eval-ns)))
    ; Accepting the correct number of args?
    (if (procedure-arity-includes? func (length exprs))
      ; OK, execute function over evaluated expressions
      (apply func (eval-expr-list exprs bindings))

      ; Bad argument count
      (eopl:error 'lang-arg-count "wrong number of arguments for primitive ~s" ident)
    )
  )
)

; Create procedure
(define (eval-make-proc arg-names expr bindings)
  (lambda arg-values
    ; Args must be correct length
    (if (= (length arg-names) (length arg-values))
      ; Bind names + evaluate function
      (eval-expr expr (append (make-bindings-list arg-names arg-values) bindings))

      ; Bad argument count
      (eopl:error 'lang-arg-count "wrong number of arguments for function")
    )
  )
)

; Call procedure
(define (eval-call-proc func args bindings)
  (apply (eval-expr func bindings) (eval-expr-list args bindings))
)

; Evaluates a "read name" expression
(define (eval-read-name name bindings)
  ; Search for binding
  (let ((value (assoc name bindings)))
    (if value
      ; Return it's value
      (cdr value)

      ; Raise unbound error
      (eopl:error 'lang-unbound-name "unbound identifier ~s" name)
    )
  )
)

; Evaluates a multi-way conditional
(define (eval-cond conds exprs bindings)
  ; Error on no conditionals left
  (if (null? conds)
    (eopl:error 'lang-no-cond "no matching conditionals")

    ; Try each conditional in turn
    (if (eval-expr (car conds) bindings)
      (eval-expr (car exprs) bindings)
      (eval-cond (cdr conds) (cdr exprs) bindings)
    )
  )
)

; Constructs a list of bindings from a list of names and values
;  Does not evaluate the values list
(define (make-bindings-list names values [bindings '()])
  ; Stop if at the end of the list
  (if (null? names)
    bindings

    ; Check if the name exists
    (if (assoc (car names) bindings)
      (eopl:error 'lang-dup-name "duplicate binding name ~s" (car names))

      ; Append to list of bindings
      (make-bindings-list
        (cdr names)
        (cdr values)
        (cons (cons (car names) (car values)) bindings)
      )
    )
  )
)

; Creates bindings list for let expression
(define (make-let-bindings names values bindings)
  (append (make-bindings-list names (eval-expr-list values bindings)) bindings)
)

; Creates bindings list for let* expression
(define (make-let*-bindings names values bindings)
  ; Stop if at the end of the list
  (if (null? names)
    bindings

    ; Create new bindings list and recurse
    (make-let*-bindings
      (cdr names)
      (cdr values)
      (cons (cons (car names) (eval-expr (car values) bindings)) bindings)
    )
  )
)

; Evaulates an expression tree generated by (lang-parse)
;  bindings = list of binding -> value pairs which can be used in the expression
(define (eval-expr tree bindings)
  (cases expression tree
    ; Numeric + variable lookup expressions
    (expr-number (num) num)
    (expr-ident (ident) (eval-read-name ident bindings))

    ; List definition
    (expr-list (lst) (eval-expr-list lst bindings))

    ; Primitive function call
    (expr-primitive (ident exprs) (eval-primitive ident exprs bindings))

    ; Procedure definition
    (expr-proc (args expr) (eval-make-proc args expr bindings))

    ; Procedure call
    (expr-call (func exprs) (eval-call-proc func exprs bindings))

    ; Let expressions
    (expr-let  (names values expr) (eval-expr expr (make-let-bindings  names values bindings)))
    (expr-let* (names values expr) (eval-expr expr (make-let*-bindings names values bindings)))

    ; Conditional
    (expr-if (expr true false)
             (if (eval-expr expr bindings) (eval-expr true bindings) (eval-expr false bindings)))

    ; Multi-way conditional
    (expr-cond (conds exprs) (eval-cond conds exprs bindings))
))

; Main expression evaluator
(define (lang-eval-expr tree) (eval-expr tree '()))