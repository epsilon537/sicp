#lang sicp

;Exercise 2.22: Louis Reasoner tries to rewrite the first square-list procedure of Exercise 2.21 so that it evolves an iterative process:
;
;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons (square (car things))
;                    answer))))
;  (iter items nil))
;
;Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?

;Items are popped off the front of the things list (car) one-by-one and added (cons) to the front of the answer list one by one. So the last item in the things list
;ends up first in the answer list.

;Louis then tries to fix his bug by interchanging the arguments to cons:
;
;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons answer
;                    (square 
;                     (car things))))))
;  (iter items nil))
;
;This doesn’t work either. Explain.
;
;When stitching together a list with cons, the list (pointer) has to go in the cdr part and the value (point) part has to go in the car part.