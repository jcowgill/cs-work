#lang racket

; Doctor entry point
(define (visit-doctor)
  ; Ask for patient's name
  (print '(next!))
  (print '(who are you?))
  (let ((name (read)))
    ; Exit if this name is bogus
    (if (or (eof-object? name) (equal? (car name) 'death))
        (print '(oh crap))

        (begin
          (print (list 'hello (car name)))
          (print '(what seems to be the trouble?))
          (doctor-driver-loop (car name) '())
          (visit-doctor)
        )
    )
   )
)

; Main loop where commands are received and stuff is printed
(define (doctor-driver-loop name responses)
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
                    (print (reply user-response responses))
                    (doctor-driver-loop name (cons user-response responses))
                )))))

; Calculates the reply to a user's request
(define (reply user-response responses)
  (if (fifty-fifty)
      (append (qualifier) (change-person user-response))
      (if (or (null? responses) (fifty-fifty))
          (hedge)
          (pick-prev-response responses)
      )
  )
)

; Adjusts a previous response so it can be printed
(define (pick-prev-response responses)
  (append '(earlier you said that) (change-person (pick-random responses)))
)

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

; Search though replacement pairs and modify item if needed
(define (replace-by-pairs replacement-pairs item)
  (if (null? replacement-pairs)
      ; No more pairs to check - leave item unchanged
      item

      ; Replace item or call self again
      (if (equal? item (car (car replacement-pairs)))
          (car (cdr (car replacement-pairs)))
          (replace-by-pairs (cdr replacement-pairs) item)
      )
  )
)

; Performs many replacements within a list
;  replacement-pairs = a list of pairs (pattern replacement)
;   containing everything to replace and what to replace with
(define (many-replace replacement-pairs lst)
  (if (null? lst)
      ; Empty input list
      lst

      ; Extract first item in the list and replace it if needed
      (cons
       (replace-by-pairs replacement-pairs (car lst))
       (many-replace replacement-pairs (cdr lst))
      )
  )
)

; Creates a list containing all the pairs in the given list
;  in addition to their swapped variants (order is unspecified)
(define (create-swapped-pairs lst)
  (if (null? lst)
      ; Empty input list
      lst

      ; Duplicate first entry and append to recursive call
      (let ((pair (car lst)))
        (cons
         pair
         (cons
          (list (car (cdr pair)) (car pair))
          (create-swapped-pairs (cdr lst))
         )
        )
      )
  )
)

; List of replacement pairs used in change-person
(define change-person-pairs
  (create-swapped-pairs '(
    (i you)
    (me you)
    (am are)
    (my your)
  ))
)

; Changes the person in a phrase
(define (change-person phrase)
  (many-replace change-person-pairs phrase)
)

; Chooses a random item from a list
(define (pick-random lst) (list-ref lst (random (length lst))))
