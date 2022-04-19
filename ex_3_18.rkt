#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define pair? mpair?)
(define length mlength)

(define (filter pred xs)
  (if (null? xs)
      '()
      (if (pred (car xs))
          (cons (car xs) (filter pred (cdr xs)))
          (filter pred (cdr xs)))))

;Exercise 3.18: Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a program that tried to find the end of the list by taking successive
;cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.

(define (cyclic? l)
  (define (contains-pair? l p)
    (not (eq? (filter (lambda (i) (eq? i p)) l) '())))
  (define (cyclic-iter? acc l)
    (if (eq? (cdr l) '())
        false
        (if (contains-pair? acc l)
            true
            (cyclic-iter? (cons l acc) (cdr l)))))
  (cyclic-iter? '() l))

(define straight (list 'a 'b 'c 'd 'e))

(cyclic? straight)

(define last (cons 'e '()))
(define six-shape (cons 'a (cons 'b (cons 'c (cons 'd last)))))
(cyclic? six-shape)

(set-cdr! last (cdr six-shape))
(cyclic? six-shape)