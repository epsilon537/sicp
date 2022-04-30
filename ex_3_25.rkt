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
(define list? mlist?)

(define (filter pred xs)
  (if (null? xs)
      '()
      (if (pred (car xs))
          (cons (car xs) (filter pred (cdr xs)))
          (filter pred (cdr xs)))))

;Exercise 3.25: Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be
;stored under different numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.


(define (assoc key records)
  (cond
    ((null? records) false)
    ((equal? key (caar records)) 
     (car records))
    (else (assoc key (cdr records)))))
 

(define (make-table)
  (let ((local-table (list '*table*)))
    
    (define (lookup keylist tbl)
      (if (empty? keylist)
          (cdr tbl)
          (if (list? (cdr tbl)) ;Is tbl a table?
              (let ((subtable 
                     (assoc (car keylist) (cdr tbl))))
                (if subtable
                    (lookup (cdr keylist) subtable)
                    false))
              false)))
    
    (define (insert! keylist value tbl)
      (if (empty? keylist)
          (set-cdr! tbl value)
          (if (list? (cdr tbl)) ;Is tbl a (possible empty) table?
              (let ((subtable (assoc (car keylist) (cdr tbl))))
                (if subtable
                    (insert! (cdr keylist) value subtable)
                    (let ((new-subtable (list (car keylist))))
                      (set-cdr! tbl (cons new-subtable (cdr tbl)))
                      (insert! (cdr keylist) value new-subtable))))
              (begin
                (set-cdr! tbl '()) ;make it a table
                (insert! keylist value tbl)))) ;retry
      'ok)
                
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keylist) (lookup keylist local-table)))
            ((eq? m 'insert-proc!) (lambda (keylist value) (insert! keylist value local-table)))
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define tbl (make-table))
((tbl 'insert-proc!) (list 'yellow) 1)
((tbl 'lookup-proc) (list 'yellow))
((tbl 'lookup-proc) (list 'yellow 'orange))
((tbl 'insert-proc!) (list 'yellow 'orange) 2)
((tbl 'lookup-proc) (list 'yellow 'orange))
((tbl 'lookup-proc) (list 'yellow))
((tbl 'insert-proc!) (list 'yellow 'orange 'blue) 3)
((tbl 'lookup-proc) (list 'yellow 'orange 'blue))
((tbl 'insert-proc!) (list 'yellow 'orange 'red) 4)
((tbl 'lookup-proc) (list 'yellow 'orange 'red))
((tbl 'lookup-proc) (list 'yellow 'orange 'blue))
