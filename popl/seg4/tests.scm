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
   (check-eval-equal? "let x = 5 y = 12 in *(x,y)" 60)
   (check-eval-equal? "let in 60" 60)
   (check-eval-fail?  "let x = 1 y = 3 x = 2 in 60")
   (check-eval-equal? "let* x = +(1,2,3) in *(x,x)" 36)
   (check-eval-equal? "let* x = 1 y = 3 x = 2 in *(x,y)" 6)
   (check-eval-equal? "let* x = 1 y = +(x,1) x = 2 in *(x,y)" 4)

   ; Cond tests
   (check-eval-equal? "cond zero?(1) ==> 1 zero?(0) ==> 42 end" 42)
   (check-eval-fail?  "cond zero?(1) ==> 1 zero?(42) ==> 42 end")
   (check-eval-fail?  "cond end")

   ; List tests
   (check-eval-equal? "[]" '())
   (check-eval-equal? "[1,2,3]" '(1 2 3))
   (check-eval-equal? "cons(+(4,2), [1,2,3])" '(6 1 2 3))
   (check-eval-equal? "car(cdr(cons(+(4,2), [1,2,3])))" 1)
   (check-eval-equal? "null?([])" #t)
   (check-eval-equal? "null?([1])" #f)

   ; Procedure tests
   (check-eval-fail?  "(death 7)")
   (check-eval-equal? "let inc = proc (x) +(x,1) in (inc 7)" 8)
   (check-eval-equal? "(proc (x) +(x,1) 7)" 8)
   (check-eval-equal? "let square = proc (x) *(x,x) in (square +(4,1))" 25)

   ; PROC tests from EOPL
   (check-eval-equal? "let f = proc (x) -(x,11) in (f (f 77))" 55)
   (check-eval-equal? "(proc (f) (f (f 77)) proc (x) -(x,11))" 55)
   (check-eval-equal? "let x = 200
                       in let f = proc (z) -(z,x)
                         in let x = 100
                           in let g = proc (z) -(z,x)
                             in -((f 1), (g 1))" -100)

   (check-eval-equal? "let makerec = proc (f)
                         let d = proc (x)
                           proc (z) ((f (x x)) z)
                         in proc (n) ((f (d d)) n)
                       in let maketimes4 = proc (f)
                         proc (x)
                           if zero?(x)
                           then 0
                           else -((f -(x,1)), minus(4))
                         in let times4 = (makerec maketimes4)
                         in (times4 3)" 12)
))

; Run the tests
(run-tests lang-tests)