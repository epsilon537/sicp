#lang sicp

;Exercise 2.24: Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the
;interpretation of this as a tree (as in Figure 2.6).
;
;Result printed by interpreter: (1 (2 (3 4)))
; 
;Box and pointer:
;
; [,]->[,]->nil
;  |    |
;  1    [,]->[,]->nil
;        |    |
;        2    [,]->[,]->nil
;              |    |
;              3    4
;
;Tree:
;                  (1 (2 (3 4)))
;                      .
;                   -----
;                   |   |
;                   1   (2 (3 4))
;                        .
;                      -----
;                      |   |
;                      2   (3 4)
;                           .
;                          ----
;                          |  |
;                          3  4
;                             
;Cons:
;(cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil))

(define l (list 1 (list 2 (list 3 4))))
(define cl (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil)))
l
cl


