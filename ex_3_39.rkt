#lang racket

;Exercise 3.39: Which of the five possibilities in the parallel execution shown above ;remain if we instead serialize execution as follows:
;
;(define x 10)
;(define s (make-serializer))
;(parallel-execute 
;  (lambda () 
;    (set! x ((s (lambda () (* x x))))))
;  (s (lambda () (set! x (+ x 1)))))

;Possible:
;101: P 1  sets x to 100 and then P 2  increments x to 101.
;Possible:
;121: P 2  increments x to 11 and then P 1  sets x to x times x.
;Impossible:
;110: P 2  changes x from 10 to 11 between the two times that P 1  accesses the value of 
;     x during the evaluation of (* x x).
;Impossible:
; 11: P 2  accesses x, then P 1  sets x to 100, then P 2  sets x.
;Possible:
;100: P 1  accesses x (twice), then P 2  sets x to 11, then P 1  sets x.
