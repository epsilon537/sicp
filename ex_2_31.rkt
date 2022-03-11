#lang sicp

;Exercise 2.31: Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as
;
;(define (square-tree tree) 
;  (tree-map square tree))

(define (square x)
  (* x x))

(define (tree-map-1 fun tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (fun tree))
        (else (cons (tree-map-1 fun (car tree))
                    (tree-map-1 fun (cdr tree))))))

(define (tree-map-2 fun tree)
  (map (lambda (x)
         (if (not (pair? x))
             (fun x)
             (tree-map-2 fun x)))
       tree))

(define (square-tree-1 tree) 
  (tree-map-1 square tree))

(define (square-tree-2 tree) 
  (tree-map-2 square tree))

(square-tree-1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree-2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
