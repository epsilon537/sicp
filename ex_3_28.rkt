#lang racket

;Exercise 3.28: Define an or-gate as a primitive function box. Your or-gate constructor should be similar to and-gate.

(define (get-signal a)
  'placeholder)

(define (after-delay d f)
  'placeholder)

(define or-gate-delay 0)

(define (set-signal! a v)
  'placeholder)

(define (add-action! s a)
  'placeholder)

(define (logical-or a b)
  (define (in-range v)
    (or (= v 0) (= v 1)))
  (cond ((not (in-range a)) (error "Invalid signal" a))
        ((not (in-range b)) (error "Invalid signal" b))
        ((or (= a 1) (= b 1)) 1)
        (else 0)))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)