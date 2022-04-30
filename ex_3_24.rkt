#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define (caar m) (car (car m)))
(define (cddr m) (cdr (cdr m)))
(define (cadr m) (car (cdr m)))
(define list mlist)
(define pair? mpair?)
(define length mlength)

(define (filter pred xs)
  (if (null? xs)
      '()
      (if (pred (car xs))
          (cons (car xs) (filter pred (cdr xs)))
          (filter pred (cdr xs)))))

;Exercise 3.24: In the table implementations above, the keys are tested for equality using equal? (called by assoc). This is not always the appropriate test. For instance, we might have
;a table with numeric keys in which we don’t need an exact match to the number we’re looking up, but only a number within some tolerance of it. Design a table constructor make-table that
;takes as an argument a same-key? procedure that will be used to test “equality” of keys. Make-table should return a dispatch procedure that can be used to access appropriate lookup and
;insert! procedures for a local table.

(define (make-assoc same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) 
           (car records))
          (else (assoc key (cdr records)))))
  assoc)

(define (make-table same-key-1? same-key-2?)
  (let ((assoc-1 (make-assoc same-key-1?))
        (assoc-2 (make-assoc same-key-2?))
        (local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc-1 key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc-2 key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc-1 key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc-2 key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define (same-key-1pct? a b)
  (and (>= b (* 0.99 a)) (<= b (* 1.01 a))))

(define tbl (make-table equal? same-key-1pct?))

((tbl 'insert-proc!) 'a 100 'banana)
((tbl 'insert-proc!) 'a 100 'banaan)
((tbl 'insert-proc!) 'a 200 'pear)
((tbl 'insert-proc!) 'b 200 'orange)
((tbl 'insert-proc!) 'b 300 'apple)
((tbl 'lookup-proc) 'a 199)
((tbl 'lookup-proc) 'a 101)
((tbl 'lookup-proc) 'a 300)
((tbl 'lookup-proc) 'b 302)
