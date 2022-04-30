#lang racket

(require rnrs/mutable-pairs-6) ;for set-car! and set-cdr!
(require compatibility/mlist) ;for mlist

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define (caar m) (car (car m)))
(define (cddr m) (cdr (cdr m)))
(define (cadr m) (car (cdr m)))
(define (caddr m) (car (cdr (cdr m))))
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

;Exercise 3.26: To search a table as implemented above, one needs to scan through the list of records. This is basically the unordered list representation of 2.3.3. For large tables, it
;may be more efficient to structure the table in a different manner. Describe a table implementation where the (key, value) records are organized using a binary tree, assuming that keys
;can be ordered in some way (e.g., numerically or alphabetically). (Compare Exercise 2.66 of Chapter 2.)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (set-entry! tree entry) (set-car! tree entry))
(define (set-left-branch! tree lb) (set-car! (cdr tree) lb))
(define (set-right-branch! tree rb) (set-car! (cddr tree) rb))
(define (make-tree entry left right)
  (list entry left right))
(define (empty-tree? tree) (null? (entry tree)))

(define (make-entry key value)
  (list key value))
(define (entry-key entry)
  (car entry))
(define (entry-val entry)
  (cadr entry))
(define (set-entry-key! entry key)
  (set-car! entry key))
(define (set-entry-val! entry val)
  (set-car! (cdr entry) val))
   
(define (assoc-tree key tree)
  (if (or (null? tree) (empty-tree? tree))
      false
      (let ((entry (entry tree)))
        (let ((entry-key (entry-key entry)))
          (cond ((= key entry-key)
                 entry)
                ((< key entry-key) 
                 (assoc-tree key (left-branch tree)))
                ((> key entry-key) 
                 (assoc-tree key (right-branch tree))))))))

(define (lookup key table)
  (let ((entry (assoc-tree key table)))
    (if entry
        (entry-val entry)
        false)))

(define (insert! key value table)
  (if (empty-tree? table)
      (set-entry! table (make-entry key value))
      (let ((entry (entry table)))
        (let ((entry-key (entry-key entry)))
          (cond ((= key entry-key) (set-entry-val! entry value))
                ((< key entry-key)
                 (if (not (null? (left-branch table)))
                     (insert! key value (left-branch table))
                     (set-left-branch! table (make-tree (make-entry key value) '() '()))))
                ((> key entry-key)
                 (if (not (null? (right-branch table)))
                     (insert! key value (right-branch table))
                     (set-right-branch! table (make-tree (make-entry key value) '() '())))))))))
      
  
(define (make-table)
  (make-tree '() '() '()))

(define tbl (make-table))

(insert! 0 'a tbl)
(lookup 0 tbl)
(insert! 10 'b tbl)
(lookup 10 tbl)
(insert! -10 'c tbl)
(lookup -10 tbl)