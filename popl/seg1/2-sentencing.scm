#lang racket

; List of words
(define noun-list (list 'dog 'cat 'student 'professor 'book 'computer))
(define verb-list (list 'ran 'ate 'slept 'drank 'exploded 'decomposed))
(define trans-verb-list (list 'crushed 'destroyed 'ravaged 'shattered 'slayed 'assimilated))
(define adjective-list (list 'red 'slow 'dead 'pungent 'over-paid 'drunk))
(define adverb-list (list 'quickly 'slowly 'wickedly 'majestically))

; Picks a random element from a list
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; These functions select a random element from the relevant lists
(define (a-noun)      (pick-random noun-list))
(define (a-verb)      (pick-random verb-list))
(define (a-trans-verb)(pick-random trans-verb-list))
(define (a-adjective) (pick-random adjective-list))
(define (a-adverb)    (pick-random adverb-list))

; Takes two procedures and returns the result of one at random
(define (either a b)
  (if (= (random 2) 0) (a) (b))
)

; Phrase definitions (return lists)
(define (noun-phrase)
  (either
   (lambda () (list (a-noun)))
   (lambda () (cons (a-adjective) (noun-phrase)))
  )
)

(define (the-noun-phrase)
  (cons 'the (noun-phrase))
)

(define (verb-phrase)
  (either
   (lambda () (list (a-verb)))
   (lambda () (list (a-verb) (a-adverb)))
  )
)

(define (trans-verb-phrase)
  (either
   (lambda () (list (a-trans-verb)))
   (lambda () (list (a-trans-verb) (a-adverb)))
  )
)

; Final sentence definition
(define (sentence)
  (either
   (lambda () (append (the-noun-phrase) (verb-phrase)))
   (lambda () (append (the-noun-phrase) (trans-verb-phrase) (the-noun-phrase)))
  )
)

(display (sentence))
(newline)
