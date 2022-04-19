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

;Exercise 3.17: Devise a correct version of the count-pairs procedure of Exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure,
;maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)

(define (count-pairs x)
  (define (contains-pair? l p)
    (not (eq? (filter (lambda (i) (eq? i p)) l) '())))

  (define (accumulate-unique-pairs acc l)
    (if (not (pair? l))
        acc
        (if (contains-pair? acc l)
            acc
            (let ((acc-w-new-pair (cons l acc)))
              (let ((accumulated-car (accumulate-unique-pairs acc-w-new-pair (car l))))
                (accumulate-unique-pairs accumulated-car (cdr l)))))))

  (length (accumulate-unique-pairs '() x)))

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