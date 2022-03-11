#lang sicp

;Exercise 2.32: We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then
;the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear
;explanation of why it works:
;
;(define (subsets s)
;  (if (null? s)
;      (list nil)
;      (let ((rest (subsets (cdr s))))
;        (append rest (map ⟨??⟩ rest)))))

;The set of all subsets can be partioned into
; (1) the set of all subsets _not_ containing first element of the given set
; (2) the set of all subsets containing the first element.
;
;Note that (1) includes the empty set. (2) is the same as (1) with that first element added to each member of (1).

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))) ;rest = (() (3) (2) (2 3))
        (append rest (map (lambda (x) (cons (car s) x)) rest))))) ;add first element to each member of that list.

(subsets (list 1 2 3))
(subsets (list 1 2))
(subsets (list 1))
