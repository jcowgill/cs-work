#lang racket

(require eopl)

(provide lang-name lang-scan lang-parse lang-eval-expr)

; The language name
(define lang-name "LET")

; Primitive functions and their evaluators
(define primitives (list
  (cons "zero?" zero?)
  (cons "minus" (lambda (x) (- 0 x)))

  (cons "+" +)
  (cons "-" -)
  (cons "*" *)
  (cons "/" /)
  (cons "equal?"   equal?)
  (cons "greater?" >)
  (cons "less?"    <)

  (cons "cons"  cons)
  (cons "car"   car)
  (cons "cdr"   cdr)
  (cons "null?" null?)
))

; Lexical specification
(define lexer-spec '(
  ; Skip all whitespace
  (white (whitespace) skip)

  ; Integers
  (number (digit (arbno digit)) number)

  ; Identifiers
  (identifier ((or letter "+" "-" "*" "/") (arbno (or letter digit "?"))) string)
))

; Grammar specification
(define grammar-spec '(
  ; Primitive number expression
  (expression (number) expr-number)

  ; Expression beginning with an identifier
  (expression (identifier ident-tail) expr-ident-start)

  ; Let expression
  (expression ("let" identifier "=" expression "in" expression) expr-let)

  ; Conditional
  (expression ("if" expression "then" expression "else" expression) expr-if)

  ; Multi-way conditional
  (expression ("cond" (arbno "{" expression "==>" expression "}") "end") expr-cond)

  ; List definition
  (expression ("[" (separated-list expression ",") "]") expr-list)

  ; Tail of identifier expressions (primitive and compound expressions)
  (ident-tail () ident-tail-empty)
  (ident-tail ("(" (separated-list expression ",") ")") ident-tail-compound)
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

; Evaluate compound expression
(define (eval-compound type exprs bindings)
  ; Get operator
  (let ((oper-pair (assoc type primitives)))
    (if oper-pair
      ; Accepting the correct number of args?
      (if (procedure-arity-includes? (cdr oper-pair) (length exprs))
        ; OK, execute function over evaluated expressions
        (apply (cdr oper-pair) (eval-expr-list exprs bindings))

        ; Bad argument count
        (eopl:error 'lang-arg-count "wrong number of arguments for operator ~s" type)
      )

      ; Bad operator
      (eopl:error 'lang-undefined "undefined operator ~s" type)
    )
  )
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

; Evaulates an expression tree generated by (lang-parse)
;  bindings = list of binding -> value pairs which can be used in the expression
(define (eval-expr tree bindings)
  (cases expression tree
    ; Numeric expression
    (expr-number (num) num)

    ; Ident start expression
    (expr-ident-start (ident tail)
      (cases ident-tail tail
        ; Primitive identifier
        (ident-tail-empty () (eval-read-name ident bindings))

        ; Compound expression
        (ident-tail-compound (exprs) (eval-compound ident exprs bindings))
      )
    )

    ; Let expression
    (expr-let (name value expr)
              (eval-expr expr (cons (cons name (eval-expr value bindings)) bindings)))

    ; Conditional
    (expr-if (expr true false)
             (if (eval-expr expr bindings) (eval-expr true bindings) (eval-expr false bindings)))

    ; Multi-way conditional
    (expr-cond (conds exprs) (eval-cond conds exprs bindings))

    ; List definition
    (expr-list (lst) (eval-expr-list lst bindings))
))

; Main expression evaluator
(define (lang-eval-expr tree) (eval-expr tree '()))