#lang racket

;Exercise 3.6: It is useful to be able to reset a random-number generator to produce a sequence starting from a given value. Design a new rand procedure that is called with an argument
;that is either the symbol generate or the symbol reset and behaves as follows: (rand 'generate) produces a new random number; ((rand 'reset) ⟨new-value⟩) resets the internal state
;variable to the designated ⟨new-value⟩. Thus, by resetting the state, one can generate repeatable sequences. These are very handy to have when testing and debugging programs that use ;random numbers.

;A:
;Using Racket functions:
;(define (rand op)
;  (cond
;    ((eq? op 'generate)
;     (random)
;     )
;    ((eq? op 'reset)
;     random-seed)
;    (else
;     (error "rand unknown op " op))))

(define (random-update n)
  (let ((a 134775813 )
        (b 1)
        (m (expt 2 32)))
    (remainder (+ (* a n) b) m)))

(define INITIAL_RAND_SEED 1234)

(define (make-rand initial-seed)
  (let ((rand-state initial-seed))
    (define (dispatch op)
      (cond
        ((eq? op 'generate)
         (set! rand-state (random-update rand-state))
         rand-state)
        ((eq? op 'reset)
         (lambda (seed) (set! rand-state seed)))
        (else
         (error "rand unknown op " op))))
    dispatch))

(define rand (make-rand INITIAL_RAND_SEED))
