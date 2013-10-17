#lang racket

; Doctor entry point
(define (visit-doctor name)
  (begin
    (print (list 'hello name))
    (print '(what seems to be the trouble?))
    (doctor-driver-loop name)))

; Main loop where commands are received and stuff is printed
(define (doctor-driver-loop name)
  (begin
    (newline)
    (display '**)
    (let ((user-response (read)))
      (if (equal? user-response '(goodbye))
                (begin
                   (print (list 'goodbye name))
                   (print '(see you next week))
                )
                (begin
                    (print (reply user-response))
                    (doctor-driver-loop name)
                )))))

; Calculates the reply to a user's request
(define (reply user-response)
  (if (fifty-fifty)
        (append (qualifier) (change-person user-response))
        (hedge)))

; Returns true / false randomly
(define (fifty-fifty) (= (random 2) 0))

; Returns a random sentence qualifier
(define (qualifier)
  (pick-random '((you seem to think)
                 (you feel that)
                 (why do you believe)
                 (why do you say))))

; Returns a random hedging statement
(define (hedge)
  (pick-random
   '((please go on)
     (many people have the same sorts of feelings)
     (many of my patients have told me the same thing)
     (please continue))))

; Replaces all occurences of pattern with replacement in th given list
(define (replace pattern replacement lst)
  (cond ((null? lst) '())
        ((equal? (car lst) pattern)
                 (cons replacement
                       (replace pattern replacement (cdr lst)))
        )
        (else (cons (car lst)
                    (replace pattern replacement (cdr lst)))
        )))

; Performs many replacements within a list
;  replacement-pairs = a list of pairs (pattern replacement)
;   containing everything to replace and what to replace with
(define (many-replace replacement-pairs lst)
  (if (null? replacement-pairs)
         lst
         (let ((pat-rep (car replacement-pairs)))
                (replace (car pat-rep)
                         (cadr pat-rep)
                         (many-replace (cdr replacement-pairs)
                                       lst)))))

; Changes the person in a phrase
(define (change-person phrase)
  (many-replace '((i you) (me you) (am are) (my your))
                phrase))

; Chooses a random item from a list
(define (pick-random lst) (list-ref lst (random (length lst))))
