#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "lang.scm")

; Ensures that the evaluation of expr equals value
(define-binary-check (check-eval-equal? expr value)
  (equal? (lang-eval-expr (lang-parse expr)) value)
)

; Ensures the evaluation of expr fails
(define (check-eval-fail? expr)
  (check-exn exn:fail?
     (lambda ()
       (lang-eval-expr (lang-parse expr))
     )
  )
)

; The main test suite to run
;  Use the check-eval-equal? and check-eval-fail? predicates to create the tests
(define lang-tests
  (test-suite
   (string-append "Tests for the " lang-name " language")

   ; Basic tests
   (check-eval-equal? "10" 10)
   (check-eval-equal? "zero?(0)" #t)
   (check-eval-equal? "zero?(10)" #f)
   (check-eval-fail?  "zero?()")
   (check-eval-fail?  "zero?(zero?(0))")
   (check-eval-fail?  "-( 2, zero?(2))")
   (check-eval-equal? "if zero?( -( 2, 3) ) then 4 else -( 4, -(2,1))" 3)
   (check-eval-equal? "if zero?(1) then 10 else if zero?(1) then 1 else 2" 2)
   (check-eval-equal? "if zero?(1) then 10 else -( 400, if zero?(1) then 1 else 2)" 398)

   ; Extended operator tests
   (check-eval-equal? "+(1,2,3)" 6)
   (check-eval-equal? "+()" 0)
   (check-eval-equal? "minus(0)" 0)
   (check-eval-equal? "minus(100)" -100)
   (check-eval-fail?  "minus()")
   (check-eval-fail?  "minus(1,2)")

   ; Comparison tests
   (check-eval-equal? "equal?(89, -(90, 1))" #t)
   (check-eval-fail?  "greater?(equal?(9, 9), 8)")
   (check-eval-equal? "less?(89, -(90, 1))" #f)

   ; Let tests
   (check-eval-equal? "let x = +(1,2,3) in *(x,x)" 36)
   (check-eval-equal? "let x = let y = 1 in +(y,5) in *(x,2)" 12)
   (check-eval-fail?  "let x = let y = x in 0 in 0")
   (check-eval-equal? "let x = 5 in let y = 12 in *(x,y)" 60)

   ; Cond tests
   (check-eval-equal? "cond { zero?(1) ==> 1 } { zero?(0) ==> 42 } end" 42)
   (check-eval-fail?  "cond { zero?(1) ==> 1 } { zero?(42) ==> 42 } end")
   (check-eval-fail?  "cond end")
))

; Run the tests
(run-tests lang-tests)