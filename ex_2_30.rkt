#lang sicp

;Exercise 2.30: Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21. That is, square-tree should behave as follows:
;
;(square-tree
; (list 1
;       (list 2 (list 3 4) 5)
;       (list 6 7)))
;(1 (4 (9 16) 25) (36 49))
;
;Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion.

(define (square-tree-1 tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (x)
         (if (not (pair? x))
             (* x x)
             (square-tree-2 x)))
       tree))

(square-tree-1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree-2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))