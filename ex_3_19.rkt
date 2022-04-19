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

;Exercise 3.19: Redo Exercise 3.18 using an algorithm that takes only a constant amount of space. (This requires a very clever idea.)

(define (cyclic? l)
  (define (distance-iter acc from to)
    (if (eq? from to)
        acc
        (distance-iter (+ acc 1) (cdr from) to)))
  
  (define (cyclic-iter? dist-from-start x)
    (if (eq? (cdr x) '())
        false
        (let ((new-dist (distance-iter 0 l x)))
          (if (< new-dist dist-from-start)
              true
              (cyclic-iter? new-dist (cdr x))))))
  (cyclic-iter? 0 l))

(define straight (list 'a 'b 'c 'd 'e))

(cyclic? straight)

(define last (cons 'e '()))
(define six-shape (cons 'a (cons 'b (cons 'c (cons 'd last)))))
(cyclic? six-shape)

(set-cdr! last (cdr six-shape))
(cyclic? six-shape)