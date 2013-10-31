#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "core.scm")

; Ensures that the evaluation of expr equals value
(define-binary-check (check-eval-equal? expr value)
  (equal? (core-eval-expr (core-parse expr)) value)
)

; Ensures the evaluation of expr fails
(define-simple-check (check-eval-fail? expr)
  (check-exn exn:fail?
     (lambda ()
       (core-eval-expr (core-parse expr))
     )
  )
)

; The main test suite to run
;  Use the check-eval-equal? and check-eval-fail? predicates to create the tests
(define core-tests
  (test-suite
   "Tests for the CORE language"

   (check-eval-equal? "10" 10)
   (check-eval-equal? "zero?(0)" #t)
   (check-eval-equal? "zero?(10)" #f)
   (check-eval-fail?  "zero?(zero?(0))")
   (check-eval-fail?  "-( 2, zero?(2))")
   (check-eval-equal? "if zero?( -( 2, 3) ) then 4 else -( 4, -(2,1))" 3)
   (check-eval-equal? "if zero?(1) then 10 else if zero?(1) then 1 else 2" 2)
   (check-eval-equal? "if zero?(1) then 10 else -( 400, if zero?(1) then 1 else 2)" 398)
))

; Run the tests
(run-tests core-tests)