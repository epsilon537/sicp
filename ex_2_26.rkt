#lang sicp

;Exercise 2.26: Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

;What result is printed by the interpreter in response to evaluating each of the following expressions:
;
;(append x y)
;append joins to lists. So:
;-> (1 2 3 4 5 6)
(append x y)

;(cons x y)
;Not:
;((1 2 3) . (4 5 6))
;But:
;y is a list. (cons x y) adds x to the front of that list. So:
;((1 2 3) 4 5 6)
(cons x y)

;(list x y)
;((1 2 3) (4 5 6))
(list x y)

