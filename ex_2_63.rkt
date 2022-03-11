#lang sicp

;Exercise 2.63: Each of the following two procedures converts a binary tree to a list.
;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))
;
;    Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in Figure 2.16?
;    Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?

;They both produce ordered lists

(define tree-1 (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) (list 9 '() (list 11 '() '()))))

tree->list-1:
(append
 (tree->list-1 (list 3 (list 1 '() '()) (list 5 '() '())))
 (cons 7 (tree->list-1 (list 9 '() (list 11 '() '())))))
(append
 (tree->list-1 (list 3 (list 1 '() '()) (list 5 '() '())))
 (cons 7 (append
          '()
          (cons 9 (tree->list-1 (list 11 '() '()))))))
(append
 (tree->list-1 (list 3 (list 1 '() '()) (list 5 '() '())))
 (cons 7 (append
          '()
          (cons 9 (append
                   '()
                   (cons 11 (tree->list-1 '())))))))

(append
 (tree->list-1 (list 3 (list 1 '() '()) (list 5 '() '())))
 (list 7 9 11))
(append
 (append
  (tree->list-1 (list 1 '() '()))
  (cons 3 (tree->list-1 (list 5 '() '()))))
 (list 7 9 11))
(append
 (append
  (tree->list-1 (list 1 '() '()))
  (cons 3 (append
           '()
           (cons 5 (tree->list-1 '())))))
 (list 7 9 11))
(append
 (append
  (tree->list-1 (list 1 '() '()))
  (list 3 5))
 (list 7 9 11))
(append
 (append
  (append
   '()
   (cons 1 (tree->list-1 '())))
  (list 3 5))
 (list 3 9 11))
(list 1 3 5 7 9 11)

;tree->list-2:
;
;(copy-to-list
; (list 3 (list 1 '() '()) (list 5 '() '()))
; (cons 7 (copy-to-list
;          (list 9 '() (list 11 '() '()))
;          '())))
;(copy-to-list
; (list 3 (list 1 '() '()) (list 5 '() '()))
; (cons 7 (copy-to-list
;          '()
;          (cons 9 (copy-to-list 
;                   (list 11 '() '())
;                   '()))
;          '())))
;(copy-to-list
; (list 3 (list 1 '() '()) (list 5 '() '()))
; (cons 7 (copy-to-list
;          '()
;          (cons 9 (copy-to-list
;                   '()
;                   (cons 11 (copy-to-list '() '()))))
;          '())))
;(copy-to-list
; (list 3 (list 1 '() '()) (list 5 '() '()))
; (list 7 9 11))
;(copy-to-list
; (list 1 '() '())
; (cons 3 (copy-to-list
;          (list 5 '() '())
;          (list 7 9 11))))
;(copy-to-list
; (list 1 '() '())
; (cons 3 (copy-to-list
;          '()
;          (cons 5 (copy-to-list
;                   '()
;                   (list 7 9 11))))))
;(copy-to-list
; (list 1 '() '())
; (list 3 5 7 9 11))
;(copy-to-list
; '()
; (list 1 3 5 7 9 11))
;(list 1 3 5 7 9 11)

(define tree-2 (list 3 (list 1 '() '()) (list 7 (list 5 '() '()) (list 9 '() (list 11 '() '())))))
(define tree-3 (list 5 (list 3 (list 1 '() '()) '()) (list 9 (list 7 '() '()) (list 11 '() '()))))

(tree->list-1 tree-1)
(tree->list-1 tree-2)
(tree->list-1 tree-3)
(tree->list-2 tree-1)
(tree->list-2 tree-2)
(tree->list-2 tree-3)

;Both recursively split the problem into two, same growth. But one uses append, which is O(n), the other just uses cons, which is O(1).
;tree->list 1 grows as T(n) -> 2*T(n/2) + O(n/2) -> O(n*log(n)) growth
;tree->list-2 grows as T(n) -> 2*T(n/2) + O(1) -> O(1) growth