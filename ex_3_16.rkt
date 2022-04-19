#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define pair? mpair?)

;Exercise 3.16: Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure. “It’s easy,” he reasons. “The number of pairs in any structure is the
;number in the car plus the number in the cdr plus one more to count the current pair.” So Ben writes the following procedure:
;
;(define (count-pairs x)
;  (if (not (pair? x))
;      0
;      (+ (count-pairs (car x))
;         (count-pairs (cdr x))
;         1)))
;
;Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben’s procedure would return
;3; return 4; return 7; never return at all.

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define z1 (cons (cons 0 0) (cons 0 0)))
(count-pairs z1)

(define t1 (cons 0 0))
(define z2 (cons (cons t1 0) t1))
(count-pairs z2)

(define t2 (cons 0 0))
(define t3 (cons t2 t2))
(define z3 (cons t3 t3))
(count-pairs z3)

(define t4 (cons 0 0))
(define z4 (cons (cons t4 0) 0))
(set-car! t4 z4)
(count-pairs z4)
