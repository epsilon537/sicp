#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)

;Exercise 3.15: Draw box-and-pointer diagrams to explain the effect of set-to-wow! on the structures z1 and z2 above.

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 
  (cons (list 'a 'b) (list 'a 'b)))

(display "Before:")
(newline)
(display z1)
(newline)
(display z2)
(newline)

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
(set-to-wow! z2)

(display "After:")
(newline)
(display z1)
(newline)
(display z2)
(newline)
