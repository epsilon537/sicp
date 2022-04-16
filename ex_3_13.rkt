#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)

;Exercise 3.13: Consider the following make-cycle procedure, which uses the last-pair procedure defined in Exercise 3.12:
;
;(define (make-cycle x)
;  (set-cdr! (last-pair x) x)
;  x)
;
;Draw a box-and-pointer diagram that shows the structure z created by
;
;(define z (make-cycle (list 'a 'b 'c)))
;
;What happens if we try to compute (last-pair z)?
;
;A: We would get stuck in an infinite loop.

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
